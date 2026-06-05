library(dplyr)
library(zoo)

cat("Starting 04_generateforecastandoutturns.R...\n")

library(dplyr)library(dply}

yearqtr_to_string <- function(x) {
  paste(floor(as.numeric(x)), paste0(" Q", cycle(x)))
}

# --------------------------------------------------
# Main paths
# --------------------------------------------------
project_root <- get_project_root()
output_dir <- file.path(project_root, "outputs")
data_dir <- file.path(project_root, "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

oos_path <- file.path(output_dir, "oos_forecasts_13q.rds")
full_data_path <- file.path(output_dir, "full_data.rds")

if (!file.exists(oos_path)) {
  stop("Missing outputs/oos_forecasts_13q.rds. Run 03_forecasting.R first.")
}
if (!file.exists(full_data_path)) {
  stop("Missing outputs/full_data.rds. Run 01_prepare_data.R first.")
}

oos_forecasts <- readRDS(oos_path)
full_data <- readRDS(full_data_path)

# --------------------------------------------------
# Realised outturn series from full_data
# --------------------------------------------------
realised_data <- full_data %>%
  transmute(
    date = date,
    core_gds_qoq = as.numeric(core_gds_qoq),
    services_qoq = as.numeric(services_qoq),
    food_at_qoq = as.numeric(food_at_qoq)
  ) %>%
  arrange(date)

# --------------------------------------------------
# Distinct vintage dates from forecast output
# --------------------------------------------------
vintage_dates <- oos_forecasts %>%
  distinct(vintage_date) %>%
  arrange(vintage_date)

# --------------------------------------------------
# Series configuration
# --------------------------------------------------
series_config <- list(
  list(
    stub = "cg",
    forecast_col = "core_gds_qoq_fc",
    outturn_col = "core_gds_qoq",
    label = "Core goods inflation"
  ),
  list(
    stub = "services",
    forecast_col = "services_qoq_fc",
    outturn_col = "services_qoq",
    label = "Services inflation"
  ),
  list(
    stub = "food",
    forecast_col = "food_at_qoq_fc",
    outturn_col = "food_at_qoq",
    label = "Food inflation"
  )
)

# --------------------------------------------------
# Function to build forecast file
# --------------------------------------------------
build_forecast_file <- function(oos_forecasts, forecast_col, label, source_name) {
  oos_forecasts %>%
    arrange(vintage_date, date) %>%
    transmute(
      vintage_date = yearqtr_to_string(vintage_date),
      value = as.numeric(.data[[forecast_col]]),
      frequency = "Q",
      forecast_horizon = as.integer(forecast_horizon),
      variable = label,
      source = source_name,
      date = yearqtr_to_string(date)
    ) %>%
    filter(!is.na(value))
}

# --------------------------------------------------
# Function to build outturn file
# --------------------------------------------------
build_outturn_file <- function(vintage_dates, realised_data, outturn_col, label) {
  outturn_base <- realised_data %>%
    transmute(
      date = date,
      value = as.numeric(.data[[outturn_col]])
    ) %>%
    filter(!is.na(value)) %>%
    arrange(date)

  tidyr::crossing(
    vintage_dates,
    outturn_base
  ) %>%
    mutate(
      forecast_horizon = as.numeric((date - vintage_date) * 4),
      frequency = "Q",
      variable = label
    ) %>%
    transmute(
      date = yearqtr_to_string(date),
      vintage_date = yearqtr_to_string(vintage_date),
      value = value,
      frequency = frequency,
      forecast_horizon = forecast_horizon,
      variable = variable
    ) %>%
    arrange(vintage_date, forecast_horizon)
}

# --------------------------------------------------
# Build and save all files
# --------------------------------------------------
for (cfg in series_config) {
  stub <- cfg$stub
  forecast_col <- cfg$forecast_col
  outturn_col <- cfg$outturn_col
  label <- cfg$label

  cat(sprintf("Building files for %s...\n", label))

  forecast_df <- build_forecast_file(
    oos_forecasts = oos_forecasts,
    forecast_col = forecast_col,
    label = label,
    source_name = "OLS-FixedCoeff25"
  )

  outturn_df <- build_outturn_file(
    vintage_dates = vintage_dates,
    realised_data = realised_data,
    outturn_col = outturn_col,
    label = label
  )

  forecast_csv <- file.path(data_dir, paste0("forecast_data", stub, ".csv"))
  forecast_parquet <- file.path(data_dir, paste0("forecast_data", stub, ".parquet"))
  outturn_csv <- file.path(data_dir, paste0("outturn_data", stub, ".csv"))
  outturn_parquet <- file.path(data_dir, paste0("outturn_data", stub, ".parquet"))

  write.csv(forecast_df, forecast_csv, row.names = FALSE)
  write_parquet(forecast_df, forecast_parquet)

  write.csv(outturn_df, outturn_csv, row.names = FALSE)
  write_parquet(outturn_df, outturn_parquet)

  cat(sprintf("Saved:\n"))
  cat(sprintf(" - %s\n", forecast_csv))
  cat(sprintf(" - %s\n", forecast_parquet))
  cat(sprintf(" - %s\n", outturn_csv))
  cat(sprintf(" - %s\n", outturn_parquet))
}

cat("Finished 04_build_eval_files_all.R\n")
library(zoo)
library(tidyr)
library(tibble)

# install.packages("arrow")
library(arrow)

cat("Starting 04_build_eval_files_all.R...\n")

get_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, args)
  if (length(match) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[match[1]]))))
  }
  normalizePath(getwd())


cat("04_generateforecastandoutturns.R scaffold created.\n")
