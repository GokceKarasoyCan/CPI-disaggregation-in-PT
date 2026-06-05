library(dplyr)
library(readxl)
library(tidyr)
library(zoo)
library(quantmod)
library(purrr)
library(tibble)

cat("Starting 03_forecasting.R...\n")

get_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, args)
  if (length(match) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[match[1]]))))
  }
  normalizePath(getwd())
}

assert_required_cols <- function(df, required_cols, label) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "%s is missing required columns: %s",
      label,
      paste(missing_cols, collapse = ", ")
    ))
  }
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
}

coef_or_zero <- function(beta, name) {
  if (!(name %in% names(beta))) {
    return(0)
  }
  val <- beta[[name]]
  if (is.na(val)) {
    return(0)
  }
  as.numeric(val)
}

# --------------------------------------------------
# Build level + growth data for one vintage sheet
# --------------------------------------------------
build_vintage_growth <- function(df_raw) {
  assert_required_cols(
    df_raw,
    c(
      "date", "stif_cpi", "core_gds", "services", "food_at",
      "energy_f", "pmdef_f", "eer_f", "infl_exp_f", "cpi_f",
      "cg_wgt", "e_wgt", "s_wgt", "f_wgt", "encont_f"
    ),
    "Vintage sheet"
  )

  df_lvl <- df_raw %>%
    mutate(
      date = as.yearqtr(date),
      pmdef_f = pmdef_f * 100
    ) %>%
    dplyr::select(
      date,
      stif_cpi, core_gds, services, food_at,
      energy_f, pmdef_f, eer_f, infl_exp_f, cpi_f,
      cg_wgt, e_wgt, s_wgt, f_wgt, encont_f
    ) %>%
    arrange(date)

  df_g <- df_lvl %>%
    mutate(across(
      c(food_at, pmdef_f, eer_f),
      ~ log(as.numeric(.)),
      .names = "ln_{.col}"
    )) %>%
    mutate(across(
      c(stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, cpi_f),
      ~ as.numeric(Delt(., type = "log")) * 100,
      .names = "{.col}_qoq"
    )) %>%
    mutate(across(
      c(stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, cpi_f),
      ~ as.numeric(Delt(., k = 4, type = "log")) * 100,
      .names = "{.col}_yoy"
    ))

  list(level = df_lvl, growth = df_g)
}

# --------------------------------------------------
# Recursive forecast: services
# --------------------------------------------------
forecast_services <- function(b_s, state_df, mpr_fcst) {
  last_serv <- tail(state_df$services_qoq, 1)
  nT <- nrow(mpr_fcst)
  serv_fc <- numeric(nT)

  for (t in seq_len(nT)) {
    y_hat <- coef_or_zero(b_s, "(Intercept)") +
      coef_or_zero(b_s, "lag(services_qoq, 1)") * last_serv +
      coef_or_zero(b_s, "infl_exp") * mpr_fcst$infl_exp_f[t] +
      coef_or_zero(b_s, "eer_qoq") * mpr_fcst$eer_f_qoq[t] +
      coef_or_zero(b_s, "pmdef_qoq") * mpr_fcst$pmdef_f_qoq[t]

    serv_fc[t] <- y_hat
    last_serv <- y_hat
  }

  mpr_fcst$services_qoq_fc <- serv_fc
  mpr_fcst
}

# --------------------------------------------------
# Recursive forecast: core goods
# --------------------------------------------------
forecast_core_goods <- function(b_cg, state_df, mpr_fcst) {
  last_cg <- tail(state_df$core_gds_qoq, 1)
  nT <- nrow(mpr_fcst)
  cg_fc <- numeric(nT)

  for (t in seq_len(nT)) {
    y_hat <-
      coef_or_zero(b_cg, "lag(core_gds_qoq, 1)") * last_cg +
      coef_or_zero(b_cg, "eer_qoq") * mpr_fcst$eer_f_qoq[t] +
      coef_or_zero(b_cg, "lag(pmdef_qoq, 2)") * mpr_fcst$pmdef_L2[t]

    cg_fc[t] <- y_hat
    last_cg <- y_hat
  }

  mpr_fcst$core_gds_qoq_fc <- cg_fc
  mpr_fcst
}

# --------------------------------------------------
# Recursive forecast: food ECM
# --------------------------------------------------
forecast_food <- function(b_lr, b_ecm, state_df, mpr_fcst) {
  last_food_level <- tail(state_df$food_at, 1)
  last_ECT <- tail(state_df$ECT_food, 1)
  food_lag_buffer <- tail(state_df$food_at_qoq, 4)

  nT <- nrow(mpr_fcst)
  food_qoq_fc <- numeric(nT)
  food_level_fc <- numeric(nT)

  for (t in seq_len(nT)) {
    x_f_L2 <- food_lag_buffer[3]   # t-2
    x_f_L4 <- food_lag_buffer[1]   # t-4

    y_hat_qoq <-
      coef_or_zero(b_ecm, "lag(food_at_qoq, 2)") * x_f_L2 +
      coef_or_zero(b_ecm, "lag(food_at_qoq, 4)") * x_f_L4 +
      coef_or_zero(b_ecm, "lag(energy_qoq, 4)") * mpr_fcst$e_L4[t] +
      coef_or_zero(b_ecm, "lag(infl_exp, 1)") * mpr_fcst$ie_L1[t] +
      coef_or_zero(b_ecm, "pmdef_qoq") * mpr_fcst$pmdef_f_qoq[t] +
      coef_or_zero(b_ecm, "lag(pmdef_qoq, 2)") * mpr_fcst$pmdef_L2[t] +
      coef_or_zero(b_ecm, "ECT_food_L1") * last_ECT

    food_qoq_fc[t] <- y_hat_qoq

    # Rebuild level from QoQ forecast
    new_food_level <- last_food_level * (1 + y_hat_qoq / 100)
    food_level_fc[t] <- new_food_level
    ln_food_t <- log(new_food_level)

    # Update ECT using long-run relation
    ECT_t <- ln_food_t - (
      coef_or_zero(b_lr, "(Intercept)") +
        coef_or_zero(b_lr, "ln_pmdef") * mpr_fcst$ln_pmdef_f[t] +
        coef_or_zero(b_lr, "ln_eer") * mpr_fcst$ln_eer_f[t]
    )

    # Roll forward state
    last_food_level <- new_food_level
    last_ECT <- ECT_t
    food_lag_buffer <- c(food_lag_buffer[-1], y_hat_qoq)
  }

  mpr_fcst$food_at_qoq_fc <- food_qoq_fc
  mpr_fcst$food_at_level_fc <- food_level_fc
  mpr_fcst
}

# --------------------------------------------------
# Run one vintage
# --------------------------------------------------
run_oos_for_vintage <- function(sheet_name, start_qtr, end_qtr, workbook_path, b_s, b_cg, b_lr, b_ecm) {
  raw_v <- readxl::read_excel(
    workbook_path,
    sheet = sheet_name,
    range = "A11:V145"
  ) %>%
    mutate(across(-date, safe_numeric))

  v_list <- build_vintage_growth(raw_v)
  df_lvl <- v_list$level

  df_g <- v_list$growth %>%
    filter(!is.na(date)) %>%
    arrange(date) %>%
    mutate(
      ECT_food = ln_food_at - (
        coef_or_zero(b_lr, "(Intercept)") +
          coef_or_zero(b_lr, "ln_pmdef") * ln_pmdef_f +
          coef_or_zero(b_lr, "ln_eer") * ln_eer_f
      ),
      ECT_food_L1 = dplyr::lag(ECT_food, 1)
    )

  # ----------------------------------------------
  # 1. State before forecast start
  # ----------------------------------------------
  state_df <- df_g %>%
    filter(date < start_qtr) %>%
    filter(
      !is.na(services_qoq),
      !is.na(core_gds_qoq),
      !is.na(food_at_qoq),
      !is.na(food_at),
      !is.na(ECT_food)
    )

  # ----------------------------------------------
  # 2. Forecast-period conditioning path
  # ----------------------------------------------
  mpr_fcst <- df_g %>%
    dplyr::select(
      date, infl_exp_f, eer_f_qoq, energy_f_qoq,
      pmdef_f_qoq, cpi_f_qoq, ln_pmdef_f, ln_eer_f
    ) %>%
    arrange(date) %>%
    mutate(
      ie_L1    = dplyr::lag(infl_exp_f, 1),
      pmdef_L2 = dplyr::lag(pmdef_f_qoq, 2),
      e_L4     = dplyr::lag(energy_f_qoq, 4)
    ) %>%
    filter(date >= start_qtr & date <= end_qtr)

  if (nrow(mpr_fcst) != 13) {
    stop(sprintf(
      "Sheet %s does not contain exactly 13 quarters in [%s, %s].",
      sheet_name, as.character(start_qtr), as.character(end_qtr)
    ))
  }

  if (nrow(state_df) < 8) {
    stop(sprintf(
      "Sheet %s does not have enough pre-forecast history to seed lags.",
      sheet_name
    ))
  }

  if (any(is.na(mpr_fcst$infl_exp_f)) ||
      any(is.na(mpr_fcst$eer_f_qoq)) ||
      any(is.na(mpr_fcst$pmdef_f_qoq))) {
    stop(sprintf(
      "Sheet %s has missing conditioning variables in the forecast window.",
      sheet_name
    ))
  }

  # ----------------------------------------------
  # 3. Recursive QoQ forecasts
  # ----------------------------------------------
  mpr_fcst_full <- mpr_fcst %>%
    forecast_services(b_s, state_df, .) %>%
    forecast_core_goods(b_cg, state_df, .) %>%
    forecast_food(b_lr, b_ecm, state_df, .)

  # ----------------------------------------------
  # 4. Rebuild component levels
  # ----------------------------------------------
  fcst_level <- df_lvl %>%
    filter(date < start_qtr) %>%
    dplyr::select(date, food_at, services, core_gds)

  fcst_qoq <- mpr_fcst_full %>%
    dplyr::select(
      date,
      food_at_qoq_fc,
      services_qoq_fc,
      core_gds_qoq_fc
    )

  fc_level <- bind_rows(fcst_level, fcst_qoq) %>%
    arrange(date)

  if (nrow(fc_level) >= 2) {
    for (i in 2:nrow(fc_level)) {
      if (is.na(fc_level$food_at[i]) && !is.na(fc_level$food_at_qoq_fc[i])) {
        fc_level$food_at[i] <- fc_level$food_at[i - 1] * (1 + fc_level$food_at_qoq_fc[i] / 100)
      }

      if (is.na(fc_level$services[i]) && !is.na(fc_level$services_qoq_fc[i])) {
        fc_level$services[i] <- fc_level$services[i - 1] * (1 + fc_level$services_qoq_fc[i] / 100)
      }

      if (is.na(fc_level$core_gds[i]) && !is.na(fc_level$core_gds_qoq_fc[i])) {
        fc_level$core_gds[i] <- fc_level$core_gds[i - 1] * (1 + fc_level$core_gds_qoq_fc[i] / 100)
      }
    }
  }

  # ----------------------------------------------
  # 5. Add vintage headline / energy / weights
  # ----------------------------------------------
  fc_level <- fc_level %>%
    left_join(
      df_lvl %>%
        dplyr::select(date, cpi_f, energy_f, cg_wgt, e_wgt, s_wgt, f_wgt, encont_f),
      by = "date"
    )

  # ----------------------------------------------
  # 6. Convert levels to YoY
  # ----------------------------------------------
  fc_yoy <- fc_level %>%
    mutate(
      food_at_yoy  = 100 * (log(food_at)  - log(dplyr::lag(food_at, 4))),
      services_yoy = 100 * (log(services) - log(dplyr::lag(services, 4))),
      core_gds_yoy = 100 * (log(core_gds) - log(dplyr::lag(core_gds, 4))),
      energy_yoy   = 100 * (log(energy_f) - log(dplyr::lag(energy_f, 4))),
      MPR_cpi      = 100 * (log(cpi_f)    - log(dplyr::lag(cpi_f, 4)))
    )

  # ----------------------------------------------
  # 7. Bottom-up CPI contributions
  # ----------------------------------------------
  fc_yoy <- fc_yoy %>%
    mutate(
      c_f  = f_wgt  * food_at_yoy  / 1000,
      c_s  = s_wgt  * services_yoy / 1000,
      c_e  = e_wgt  * energy_yoy   / 1000,
      c_cg = cg_wgt * core_gds_yoy / 1000
    ) %>%
    mutate(
      cpi_bottom_up = c_f + encont_f + c_s + c_cg,
      cpi_resid     = MPR_cpi - cpi_bottom_up,
      c_cg_r        = MPR_cpi - c_f - encont_f - c_s
    )

  # Keep only forecast window in final output
  fc_yoy <- fc_yoy %>%
    filter(date >= start_qtr & date <= end_qtr)

  # ----------------------------------------------
  # 8. Add metadata
  # ----------------------------------------------
  fc_yoy %>%
    mutate(
      vintage_sheet    = sheet_name,
      vintage_date     = start_qtr,
      forecast_horizon = as.integer((date - vintage_date) * 4)
    ) %>%
    dplyr::select(
      vintage_sheet, vintage_date, date, forecast_horizon,
      food_at, services, core_gds,
      food_at_qoq_fc, services_qoq_fc, core_gds_qoq_fc,
      food_at_yoy, services_yoy, core_gds_yoy, energy_yoy, MPR_cpi,
      c_f, c_s, c_e, c_cg, encont_f,
      cpi_bottom_up, cpi_resid, c_cg_r
    )
}

# --------------------------------------------------
# Sheet name -> forecast start quarter
# --------------------------------------------------
sheet_to_start_qtr <- function(sheet_name) {
  run_code <- substr(sheet_name, 1, 1)
  yy <- as.integer(substr(sheet_name, 2, 3))
  year <- 2000 + yy

  qtr <- dplyr::case_when(
    run_code == "F" ~ 1,
    run_code == "M" ~ 2,
    run_code == "A" ~ 3,
    run_code == "N" ~ 4,
    TRUE ~ NA_real_
  )

  if (is.na(qtr)) {
    stop(sprintf("Unsupported vintage sheet code in %s", sheet_name))
  }

  as.yearqtr(sprintf("%d Q%d", year, qtr))
}

# --------------------------------------------------
# Main
# --------------------------------------------------
project_root <- get_project_root()
output_dir <- file.path(project_root, "outputs")
workbook_path <- file.path(project_root, "data_set_vintages.xlsx")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(workbook_path)) {
  stop(sprintf("Vintage workbook not found: %s", workbook_path))
}

# Load estimated coefficients from 02_estimation.R
b_s   <- readRDS(file.path(output_dir, "b_services.rds"))
b_cg  <- readRDS(file.path(output_dir, "b_core_goods.rds"))
b_lr  <- readRDS(file.path(output_dir, "b_food_lr.rds"))
b_ecm <- readRDS(file.path(output_dir, "b_food_ecm.rds"))

# Find vintage sheets
all_sheets <- readxl::excel_sheets(workbook_path)
vintage_sheets <- all_sheets[grepl("^[FAMN][0-9]{2}$", all_sheets)]

if (length(vintage_sheets) == 0) {
  stop("No vintage sheets found. Expected names like F24, M24, A24, N24.")
}

# Build plan
vintage_plan <- tibble::tibble(sheet_name = vintage_sheets) %>%
  mutate(start_qtr = purrr::map(sheet_name, sheet_to_start_qtr)) %>%
  mutate(
    start_qtr = as.yearqtr(unlist(start_qtr)),
    end_qtr = start_qtr + 12/4,
    run_order = match(substr(sheet_name, 1, 1), c("F", "M", "A", "N")),
    year_num = as.integer(substr(sheet_name, 2, 3))
  ) %>%
  arrange(year_num, run_order) %>%
  dplyr::select(sheet_name, start_qtr, end_qtr)

# Run all vintages
oos_forecasts <- purrr::pmap_dfr(
  vintage_plan,
  function(sheet_name, start_qtr, end_qtr) {
    run_oos_for_vintage(
      sheet_name = sheet_name,
      start_qtr = start_qtr,
      end_qtr = end_qtr,
      workbook_path = workbook_path,
      b_s = b_s,
      b_cg = b_cg,
      b_lr = b_lr,
      b_ecm = b_ecm
    )
  }
)

# Save outputs
saveRDS(vintage_plan, file.path(output_dir, "vintage_plan.rds"))
saveRDS(oos_forecasts, file.path(output_dir, "oos_forecasts_13q.rds"))
write.csv(oos_forecasts, file.path(output_dir, "oos_forecasts_13q.csv"), row.names = FALSE)

cat("Saved out-of-sample outputs:\n")
cat(" - outputs/vintage_plan.rds\n")
cat(" - outputs/oos_forecasts_13q.rds\n")
cat(" - outputs/oos_forecasts_13q.csv\n")
cat("Finished 03_forecasting.R\n")