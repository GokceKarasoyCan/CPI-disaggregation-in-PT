library(dplyr)
library(readxl)
library(tidyr)
library(zoo)

cat("Starting 04versions_generateforecastandoutturns.R...\n")

get_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, args)
  if (length(match) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[match[1]]))))
  }
  normalizePath(getwd())
}

yearqtr_to_string <- function(x) {
  paste(floor(as.numeric(x)), paste0(" Q", cycle(x)))
}

project_root <- get_project_root()
output_dir <- file.path(project_root, "outputs")
data_dir <- file.path(project_root, "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

oos_path <- file.path(output_dir, "oos_forecasts_versions_13q.rds")
workbook_path <- file.path(project_root, "data_set_vintages.xlsx")
estimation_workbook_path <- file.path(project_root, "data_set_v3.xlsx")

if (!file.exists(oos_path)) {
  stop("Missing outputs/oos_forecasts_versions_13q.rds. Run 03versions_forecasting.R first.")
}
if (!file.exists(workbook_path)) {
  stop("Missing data_set_vintages.xlsx required for vintage weights.")
}
if (!file.exists(estimation_workbook_path)) {
  stop("Missing data_set_v3.xlsx required for realised component outturns.")
}

oos_forecasts <- readRDS(oos_path)

required_cols <- c(
  "vintage_sheet", "vintage_date", "date", "forecast_horizon", "version",
  "estimated_food", "estimated_services", "estimated_core",
  "food", "services_contrib", "core", "energy"
)
missing_cols <- setdiff(required_cols, names(oos_forecasts))
if (length(missing_cols) > 0) {
  stop(sprintf("oos_forecasts_versions_13q.rds is missing columns: %s", paste(missing_cols, collapse = ", ")))
}

has_arrow <- requireNamespace("arrow", quietly = TRUE)

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
}

vintage_sheets <- sort(unique(as.character(oos_forecasts$vintage_sheet)))

vintage_weights <- purrr::map_dfr(
  vintage_sheets,
  function(sheet_name) {
    readxl::read_excel(
      workbook_path,
      sheet = sheet_name,
      range = "A11:V145"
    ) %>%
      mutate(
        date = as.yearqtr(date),
        across(c(f_wgt, s_wgt, cg_wgt), safe_numeric),
        vintage_sheet = sheet_name
      ) %>%
      dplyr::select(vintage_sheet, date, f_wgt, s_wgt, cg_wgt)
  }
)

realised_levels <- readxl::read_excel(
  estimation_workbook_path,
  sheet = "Estimation_data",
  range = "A11:L154"
) %>%
  mutate(
    date = as.yearqtr(date),
    across(c(food_at, services, core_gds), safe_numeric)
  ) %>%
  arrange(date) %>%
  mutate(
    realised_food_yoy = 100 * (log(food_at) - log(dplyr::lag(food_at, 4))),
    realised_services_yoy = 100 * (log(services) - log(dplyr::lag(services, 4))),
    realised_core_yoy = 100 * (log(core_gds) - log(dplyr::lag(core_gds, 4)))
  ) %>%
  dplyr::select(date, realised_food_yoy, realised_services_yoy, realised_core_yoy)

# Component-level view (estimated vs reconciled) to inspect redistribution by version
component_compare <- oos_forecasts %>%
  transmute(
    vintage_sheet,
    vintage_date = yearqtr_to_string(vintage_date),
    date = yearqtr_to_string(date),
    forecast_horizon = as.integer(forecast_horizon),
    version,
    estimated_food,
    food,
    estimated_services,
    services_contrib,
    estimated_core,
    core,
    energy
  ) %>%
  mutate(
    food_adjustment = food - estimated_food,
    services_adjustment = services_contrib - estimated_services,
    core_adjustment = core - estimated_core
  )

component_yoy_compare <- oos_forecasts %>%
  left_join(vintage_weights, by = c("vintage_sheet", "date")) %>%
  left_join(realised_levels, by = "date") %>%
  mutate(
    forecast_food_yoy = dplyr::if_else(!is.na(f_wgt) & f_wgt != 0, 1000 * food / f_wgt, NA_real_),
    forecast_services_yoy = dplyr::if_else(!is.na(s_wgt) & s_wgt != 0, 1000 * services_contrib / s_wgt, NA_real_),
    forecast_core_yoy = dplyr::if_else(!is.na(cg_wgt) & cg_wgt != 0, 1000 * core / cg_wgt, NA_real_)
  ) %>%
  transmute(
    vintage_sheet,
    vintage_date,
    date,
    forecast_horizon,
    version,
    forecast_food_yoy,
    forecast_services_yoy,
    forecast_core_yoy,
    realised_food_yoy,
    realised_services_yoy,
    realised_core_yoy
  ) %>%
  pivot_longer(
    cols = c(forecast_food_yoy, forecast_services_yoy, forecast_core_yoy),
    names_to = "forecast_series",
    values_to = "forecast_yoy"
  ) %>%
  mutate(
    component = dplyr::recode(
      forecast_series,
      forecast_food_yoy = "Food",
      forecast_services_yoy = "Services",
      forecast_core_yoy = "Core goods"
    ),
    realised_yoy = dplyr::case_when(
      component == "Food" ~ realised_food_yoy,
      component == "Services" ~ realised_services_yoy,
      component == "Core goods" ~ realised_core_yoy,
      TRUE ~ NA_real_
    ),
    sq_error_vs_outturn = (forecast_yoy - realised_yoy)^2
  ) %>%
  dplyr::select(
    vintage_sheet,
    vintage_date,
    date,
    forecast_horizon,
    version,
    component,
    forecast_yoy,
    realised_yoy,
    sq_error_vs_outturn
  )

component_version_scorecard <- component_yoy_compare %>%
  group_by(version, component) %>%
  summarise(
    n_obs = sum(!is.na(sq_error_vs_outturn)),
    rmse_vs_outturn = sqrt(mean(sq_error_vs_outturn, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(component, rmse_vs_outturn)

component_horizon_scorecard <- component_yoy_compare %>%
  group_by(version, component, forecast_horizon) %>%
  summarise(
    n_obs = sum(!is.na(sq_error_vs_outturn)),
    rmse_vs_outturn = sqrt(mean(sq_error_vs_outturn, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(component, forecast_horizon, rmse_vs_outturn)

component_version_winners_rmse <- component_version_scorecard %>%
  filter(n_obs > 0, !is.na(rmse_vs_outturn)) %>%
  group_by(component) %>%
  slice_min(order_by = rmse_vs_outturn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(component)

component_horizon_winners_rmse <- component_horizon_scorecard %>%
  filter(n_obs > 0, !is.na(rmse_vs_outturn)) %>%
  group_by(component, forecast_horizon) %>%
  slice_min(order_by = rmse_vs_outturn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(component, forecast_horizon)

write.csv(component_compare, file.path(data_dir, "forecast_versions_component_compare.csv"), row.names = FALSE)
write.csv(component_yoy_compare %>% mutate(
  vintage_date = yearqtr_to_string(vintage_date),
  date = yearqtr_to_string(date)
), file.path(data_dir, "forecast_versions_component_yoy_compare.csv"), row.names = FALSE)
write.csv(component_version_scorecard, file.path(data_dir, "forecast_versions_component_scorecard.csv"), row.names = FALSE)
write.csv(component_horizon_scorecard, file.path(data_dir, "forecast_versions_component_scorecard_by_horizon.csv"), row.names = FALSE)
write.csv(component_version_winners_rmse, file.path(data_dir, "forecast_versions_component_winners_rmse.csv"), row.names = FALSE)
write.csv(component_horizon_winners_rmse, file.path(data_dir, "forecast_versions_component_winners_rmse_by_horizon.csv"), row.names = FALSE)

saveRDS(component_compare, file.path(output_dir, "forecast_versions_component_compare.rds"))
saveRDS(component_yoy_compare, file.path(output_dir, "forecast_versions_component_yoy_compare.rds"))
saveRDS(component_version_scorecard, file.path(output_dir, "forecast_versions_component_scorecard.rds"))
saveRDS(component_horizon_scorecard, file.path(output_dir, "forecast_versions_component_scorecard_by_horizon.rds"))
saveRDS(component_version_winners_rmse, file.path(output_dir, "forecast_versions_component_winners_rmse.rds"))
saveRDS(component_horizon_winners_rmse, file.path(output_dir, "forecast_versions_component_winners_rmse_by_horizon.rds"))

cat("Saved version-comparison outputs:\n")
cat(" - data/forecast_versions_component_compare.csv\n")
cat(" - data/forecast_versions_component_yoy_compare.csv\n")
cat(" - data/forecast_versions_component_scorecard.csv\n")
cat(" - data/forecast_versions_component_scorecard_by_horizon.csv\n")
cat(" - data/forecast_versions_component_winners_rmse.csv\n")
cat(" - data/forecast_versions_component_winners_rmse_by_horizon.csv\n")
cat(" - outputs/forecast_versions_component_compare.rds\n")
cat(" - outputs/forecast_versions_component_yoy_compare.rds\n")
cat(" - outputs/forecast_versions_component_scorecard.rds\n")
cat(" - outputs/forecast_versions_component_scorecard_by_horizon.rds\n")
cat(" - outputs/forecast_versions_component_winners_rmse.rds\n")
cat(" - outputs/forecast_versions_component_winners_rmse_by_horizon.rds\n")

if (has_arrow) {
  arrow::write_parquet(component_compare, file.path(data_dir, "forecast_versions_component_compare.parquet"))
  arrow::write_parquet(
    component_yoy_compare %>% mutate(
      vintage_date = yearqtr_to_string(vintage_date),
      date = yearqtr_to_string(date)
    ),
    file.path(data_dir, "forecast_versions_component_yoy_compare.parquet")
  )
  arrow::write_parquet(component_version_scorecard, file.path(data_dir, "forecast_versions_component_scorecard.parquet"))
  arrow::write_parquet(component_horizon_scorecard, file.path(data_dir, "forecast_versions_component_scorecard_by_horizon.parquet"))
  arrow::write_parquet(component_version_winners_rmse, file.path(data_dir, "forecast_versions_component_winners_rmse.parquet"))
  arrow::write_parquet(component_horizon_winners_rmse, file.path(data_dir, "forecast_versions_component_winners_rmse_by_horizon.parquet"))
  cat(" - data/forecast_versions_component_compare.parquet\n")
  cat(" - data/forecast_versions_component_yoy_compare.parquet\n")
  cat(" - data/forecast_versions_component_scorecard.parquet\n")
  cat(" - data/forecast_versions_component_scorecard_by_horizon.parquet\n")
  cat(" - data/forecast_versions_component_winners_rmse.parquet\n")
  cat(" - data/forecast_versions_component_winners_rmse_by_horizon.parquet\n")
}

cat("Finished 04versions_generateforecastandoutturns.R\n")
