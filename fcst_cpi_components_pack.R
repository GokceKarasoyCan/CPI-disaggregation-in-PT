### IMPORTING DATA from EXCEL ####
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)
library(zoo)        # as.yearqtr()

# Force binary package installs on Windows to avoid source compilation prompts.
options(install.packages.check.source = "no")
if (!requireNamespace("zip", quietly = TRUE)) {
  install.packages("zip", repos = "https://cloud.r-project.org", type = "binary")
}
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx", repos = "https://cloud.r-project.org", type = "binary")
}
library(openxlsx)   # Excel output
library(scales)     # alpha()
# Suppress the known xts warning about dplyr::lag masking in interactive sessions.
options(xts.warn_dplyr_breaks_lag = FALSE)
library(quantmod)
library(ggplot2)

cat("Starting fcst_cpi_components.R...\n")

## --------------------------------------------------
#  1. DATA PREPARATION
## --------------------------------------------------

#----- 1.1 Data for estimation--------

# Importing raw data 1990Q2-2025Q4
cpi_tbl <-
  readxl::read_excel(
    "C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data_set_v3.xlsx",
    sheet = "Estimation_data",
    range = "A11:L154"
  )

cpi_tbl$date <- as.yearqtr(cpi_tbl$date)

cpi_tbl <-
  cpi_tbl %>%
  mutate(pmdef = pmdef * 100)

# Data in levels (Indexes, rates) 1991-2025
df_cpi_level <-
  cpi_tbl %>%
  dplyr::select(date, cpi, core_gds, services, food_at, energy, pmdef, eer, infl_exp)

# Data in log-levels
df_cpi_log <-
  df_cpi_level %>%
  mutate(
    across(
      c(cpi, core_gds, services, food_at, energy, pmdef, eer),
      ~ log(.),
      .names = "ln_{.col}"
    )
  )

# Data in quarterly percentage change - qoq
df_cpi_qoq <-
  df_cpi_log %>%
  mutate(
    across(
      c(cpi, core_gds, services, food_at, energy, pmdef, eer),
      ~ Delt(., type = "log") * 100,
      .names = "{.col}_qoq"
    )
  ) %>%
  slice(-1)

# Annual percentage change
df_cpi_yoy <-
  df_cpi_log %>%
  mutate(
    across(
      c(cpi, core_gds, services, food_at, energy, pmdef, eer),
      ~ Delt(., k = 4, type = "log") * 100,
      .names = "{.col}_yoy"
    )
  )

# Creating dummies
df_cpi_tbl <-
  df_cpi_qoq %>%
  mutate(D_1991Q3   = as.integer(date >= as.yearqtr("1991 Q3") & date <= as.yearqtr("1991 Q3"))) %>%
  mutate(D_2001Q2   = as.integer(date >= as.yearqtr("2001 Q2") & date <= as.yearqtr("2001 Q2"))) %>%
  mutate(D_2008Q2   = as.integer(date >= as.yearqtr("2008 Q2") & date <= as.yearqtr("2008 Q2"))) %>%
  mutate(D_2009Q2   = as.integer(date >= as.yearqtr("2009 Q2") & date <= as.yearqtr("2009 Q2"))) %>%
  mutate(D_2021Q2   = as.integer(date >= as.yearqtr("2021 Q2") & date <= as.yearqtr("2021 Q2"))) %>%
  mutate(D_2021Q2Q3 = as.integer(date >= as.yearqtr("2021 Q2") & date <= as.yearqtr("2021 Q3"))) %>%
  mutate(D_2023Q2   = as.integer(date >= as.yearqtr("2023 Q2") & date <= as.yearqtr("2023 Q2"))) %>%
  mutate(D_2023Q3   = as.integer(date >= as.yearqtr("2023 Q3") & date <= as.yearqtr("2023 Q3")))

# Selecting a set of variables
full_data <-
  df_cpi_tbl %>%
  dplyr::select(
    date, cpi_qoq, services_qoq,
    core_gds_qoq, food_at_qoq,
    infl_exp, pmdef_qoq, energy_qoq, eer_qoq,
    food_at, ln_food_at, ln_pmdef, ln_eer,
    D_1991Q3, D_2001Q2, D_2008Q2,
    D_2009Q2, D_2021Q2, D_2021Q2Q3, D_2023Q2, D_2023Q3
  )

#----- 1.2 Data for forecasting including CECD FORECAST and wider BANK FORECAST

# Importing wider forecast for inflation and activity 1996Q1-2029Q2
df_fcst <-
  readxl::read_excel(
    "C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data_set_v3.xlsx",
    sheet = "M26_Baseline",
    range = "A11:V145"
  )

df_fcst$date <- as.yearqtr(df_fcst$date)

# Data in levels (Indexes, rates)
df_fcst_level <-
  df_fcst %>%
  mutate(pmdef_f = pmdef_f * 100) %>%
  dplyr::select(date, stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, infl_exp_f, cpi_f)

# Data in log-levels, qoq and yoy growth
df_fcst_growth <-
  df_fcst_level %>%
  mutate(
    across(
      c(food_at, pmdef_f, eer_f),
      ~ log(.),
      .names = "ln_{.col}"
    )
  ) %>%
  mutate(
    across(
      c(stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, cpi_f),
      ~ Delt(., type = "log") * 100,
      .names = "{.col}_qoq"
    )
  ) %>%
  mutate(
    across(
      c(stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, cpi_f),
      ~ Delt(., k = 4, type = "log") * 100,
      .names = "{.col}_yoy"
    )
  )

## --------------------------------------------------
#  2. MODEL ESTIMATION
## --------------------------------------------------

library(lmtest)
library(sandwich)
library(ecm)
library(purrr)
library(car)
library(strucchange)
library(dynlm)

#------Estimation window----------
full_sample <- subset(
  full_data,
  date >= as.yearqtr("1991 Q2") &
    date <= as.yearqtr("2025 Q4")
)

#===== Services model and forecast =========
s_adl <- lm(
  services_qoq ~ lag(services_qoq, 1) + infl_exp +
    eer_qoq + pmdef_qoq + D_1991Q3 + D_2021Q2 + D_2023Q2,
  data = full_sample
)

summary(s_adl)
b_s <- coef(s_adl)

#===== Core goods =======
cg_adl <- lm(
  core_gds_qoq ~ 0 + lag(core_gds_qoq, 1) +
    eer_qoq + lag(pmdef_qoq, 2) +
    D_2009Q2 + D_2023Q3,
  data = full_sample
)

summary(cg_adl)
b_cg <- coef(cg_adl)

#===== Food, alcohol and tobacco =======

# 1. Long-run equation
coint_food_lm <- lm(ln_food_at ~ ln_eer + ln_pmdef, data = full_sample)
summary(coint_food_lm)

b_lr <- coef(coint_food_lm)

full_sample$ECT_food    <- resid(coint_food_lm)
full_sample$ECT_food_L1 <- dplyr::lag(full_sample$ECT_food, 1)

# Save resid in forecast data as well
ECT_df <- full_sample %>%
  select(date) %>%
  mutate(
    ECT_food    = resid(coint_food_lm),
    ECT_food_L1 = dplyr::lag(ECT_food, 1)
  )

df_fcst_growth <- df_fcst_growth %>%
  left_join(ECT_df, by = "date")

# 2. Short-run equation
ecm_food <- dynlm(
  food_at_qoq ~ 0 + lag(food_at_qoq, 2) + lag(food_at_qoq, 4) + lag(energy_qoq, 4) +
    lag(infl_exp, 1) + pmdef_qoq + lag(pmdef_qoq, 2) +
    ECT_food_L1 + D_2001Q2 + D_2008Q2,
  data = full_sample
)

summary(ecm_food)
b_ecm <- coef(ecm_food)

## -----------------------------------------------------
#---- USING NEW DATA CONSISTENT WITH CECD FORECAST
## -----------------------------------------------------

#---- CHOOSE THE # OF QUARTERS TO BE CONSIDERED FROM STIF IN EOD FORECAST
df_plus_nearcast <- subset(
  df_fcst_growth,
  date >= as.yearqtr("1996 Q2") &
    date <= as.yearqtr("2026 Q3")
) # Regular practice: 2 quarters since actual data

#---- Adding required ECT series for food forecast------
ECM_data <- df_plus_nearcast %>%
  select(date, ln_pmdef_f, ln_food_at, ln_eer_f) %>%
  mutate(
    ln_pmdef = ln_pmdef_f,
    ln_eer   = ln_eer_f
  )

# Forecast window
fc_start <- as.yearqtr("2026 Q1")
fc_end   <- as.yearqtr("2026 Q3")

idx_fc <- with(
  ECM_data,
  date >= fc_start & date <= fc_end
)

# Long-run fitted values using ECM food model
yhat_fc <- predict(
  coint_food_lm,
  newdata = ECM_data[idx_fc, c("ln_pmdef", "ln_eer")]
)

# Actual food values, residuals (ECT) and append them into existing ECT_food series
y_fc <- ECM_data$ln_food_at[idx_fc]
resid_fc <- y_fc - yhat_fc

df_plus_nearcast$ECT_food[idx_fc] <- resid_fc
df_plus_nearcast$ECT_food_L1 <- dplyr::lag(df_plus_nearcast$ECT_food, 1)

## -----------------------------------------------------
#  PRODUCING EOD FORECAST consistent with wider MPR FORECAST 2026Q4-2029Q2
## -----------------------------------------------------

#------MPR wider forecast consistent with forecast horizon----------
mpr_fcst <-
  df_fcst_growth %>%
  dplyr::select(
    date, infl_exp_f, eer_f_qoq, energy_f_qoq,
    pmdef_f_qoq, cpi_f_qoq, ln_pmdef_f, ln_eer_f
  ) %>%
  arrange(date) %>%
  mutate(
    ie_L1    = dplyr::lag(infl_exp_f, 1),
    ie_L3    = dplyr::lag(infl_exp_f, 3),
    pmdef_L2 = dplyr::lag(pmdef_f_qoq, 2),
    e_L4     = dplyr::lag(energy_f_qoq, 4)
  ) %>%
  filter(
    date >= as.yearqtr("2026 Q4"),
    date <= as.yearqtr("2029 Q2")
  )

#------Forecast functions---------

# Services
forecast_services <- function(b_s, df_plus_nearcast, mpr_fcst) {
  last_serv <- tail(df_plus_nearcast$services_qoq, 1)
  nT <- nrow(mpr_fcst)
  serv_fc <- numeric(nT)

  for (t in seq_len(nT)) {
    x_serv_lag    <- last_serv
    x_infl_exp_f  <- mpr_fcst$infl_exp_f[t]
    x_eer_f_qoq   <- mpr_fcst$eer_f_qoq[t]
    x_pmdef_f_qoq <- mpr_fcst$pmdef_f_qoq[t]

    y_hat <- b_s["(Intercept)"] +
      b_s["lag(services_qoq, 1)"] * x_serv_lag +
      b_s["infl_exp"] * x_infl_exp_f +
      b_s["eer_qoq"] * x_eer_f_qoq +
      b_s["pmdef_qoq"] * x_pmdef_f_qoq

    serv_fc[t] <- y_hat
    last_serv <- y_hat
  }

  mpr_fcst$services_qoq_fc <- serv_fc
  mpr_fcst
}

# Core goods
forecast_core_goods <- function(b_cg, df_plus_nearcast, mpr_fcst) {
  last_cg <- tail(df_plus_nearcast$core_gds_qoq, 1)
  nT <- nrow(mpr_fcst)
  cg_fc <- numeric(nT)

  for (t in seq_len(nT)) {
    x_cg_L1     <- last_cg
    x_eer_f_qoq <- mpr_fcst$eer_f_qoq[t]
    x_pmdef_L2  <- mpr_fcst$pmdef_L2[t]

    y_hat <-
      b_cg["lag(core_gds_qoq, 1)"] * x_cg_L1 +
      b_cg["eer_qoq"] * x_eer_f_qoq +
      b_cg["lag(pmdef_qoq, 2)"] * x_pmdef_L2

    cg_fc[t] <- y_hat
    last_cg  <- y_hat
  }

  mpr_fcst$core_gds_qoq_fc <- cg_fc
  mpr_fcst
}

# Food
forecast_food <- function(b_lr, b_ecm, df_plus_nearcast, mpr_fcst) {
  last_food_level <- tail(df_plus_nearcast$food_at, 1)
  last_ECT        <- tail(df_plus_nearcast$ECT_food, 1)

  # Order: [t-4, t-3, t-2, t-1]
  food_lag_buffer <- tail(df_plus_nearcast$food_at_qoq, 4)

  nT            <- nrow(mpr_fcst)
  food_qoq_fc   <- numeric(nT)
  food_level_fc <- numeric(nT)

  for (t in seq_len(nT)) {

    # Endogenous lags
    x_f_L2 <- food_lag_buffer[3]
    x_f_L4 <- food_lag_buffer[1]

    # Exogenous lags
    x_e_L4      <- mpr_fcst$e_L4[t]
    x_ie_L1     <- mpr_fcst$ie_L1[t]
    x_pmdef_qoq <- mpr_fcst$pmdef_f_qoq[t]
    x_pmdef_L2  <- mpr_fcst$pmdef_L2[t]

    # ECT lag
    x_ECT_L1 <- last_ECT

    # Short-run ECM forecast
    y_hat_qoq <-
      b_ecm["lag(food_at_qoq, 2)"] * x_f_L2 +
      b_ecm["lag(food_at_qoq, 4)"] * x_f_L4 +
      b_ecm["lag(energy_qoq, 4)"] * x_e_L4 +
      b_ecm["lag(infl_exp, 1)"] * x_ie_L1 +
      b_ecm["pmdef_qoq"] * x_pmdef_qoq +
      b_ecm["lag(pmdef_qoq, 2)"] * x_pmdef_L2 +
      b_ecm["ECT_food_L1"] * x_ECT_L1

    food_qoq_fc[t] <- y_hat_qoq

    # Update level of food prices
    new_food_level <- last_food_level * (1 + y_hat_qoq / 100)
    food_level_fc[t] <- new_food_level

    # Update ECT_t
    ln_pmdef_t <- mpr_fcst$ln_pmdef_f[t]
    ln_eer_t   <- mpr_fcst$ln_eer_f[t]
    ln_food_t  <- log(new_food_level)

    ECT_t <- ln_food_t - (
      b_lr["(Intercept)"] +
        b_lr["ln_pmdef"] * ln_pmdef_t +
        b_lr["ln_eer"] * ln_eer_t
    )

    # Roll forward
    last_food_level <- new_food_level
    last_ECT        <- ECT_t

    food_lag_buffer <- c(food_lag_buffer[-1], y_hat_qoq)
  }

  mpr_fcst$food_at_qoq_fc   <- food_qoq_fc
  mpr_fcst$food_at_level_fc <- food_level_fc

  mpr_fcst
}

#-----FORECASTING 2026Q4-2029Q2--------
mpr_fcst_full <- mpr_fcst %>%
  forecast_services(b_s, df_plus_nearcast, .) %>%
  forecast_core_goods(b_cg, df_plus_nearcast, .) %>%
  forecast_food(b_lr, b_ecm, df_plus_nearcast, .)

#-------------------------------------
# FORECAST IN TERMS OF YEAR ON YEAR
#-------------------------------------

# 1. Historical data + CECD forecast: 1996Q2–2026Q3
fcst_level <- df_plus_nearcast %>%
  filter(
    date >= as.yearqtr("1996 Q2"),
    date <= as.yearqtr("2026 Q3")
  ) %>%
  select(date, food_at, services, core_gds)

# 2. Select forecast qoq inflation
fcst_qoq <- mpr_fcst_full %>%
  select(
    date,
    food_at_qoq_fc,
    services_qoq_fc,
    core_gds_qoq_fc
  )

# 3. Bind and arrange chronologically
fc_level <- bind_rows(fcst_level, fcst_qoq) %>%
  arrange(date)

# 4. Rebuild levels iteratively
for (i in seq_len(nrow(fc_level))) {

  if (is.na(fc_level$food_at[i])) {
    fc_level$food_at[i] <- fc_level$food_at[i - 1] *
      (1 + fc_level$food_at_qoq_fc[i] / 100)
  }

  if (is.na(fc_level$services[i])) {
    fc_level$services[i] <- fc_level$services[i - 1] *
      (1 + fc_level$services_qoq_fc[i] / 100)
  }

  if (is.na(fc_level$core_gds[i])) {
    fc_level$core_gds[i] <- fc_level$core_gds[i - 1] *
      (1 + fc_level$core_gds_qoq_fc[i] / 100)
  }
}

# Adding the rest of CPI components
fc_level <- fc_level %>%
  left_join(
    df_fcst_level %>%
      select(date, cpi_f, energy_f),
    by = "date"
  )

# 5. Compute YoY inflation (log-diff)
fc_yoy <- fc_level %>%
  mutate(
    food_at_yoy  = 100 * (log(food_at)  - log(dplyr::lag(food_at, 4))),
    services_yoy = 100 * (log(services) - log(dplyr::lag(services, 4))),
    core_gds_yoy = 100 * (log(core_gds) - log(dplyr::lag(core_gds, 4))),
    energy_yoy   = 100 * (log(energy_f) - log(dplyr::lag(energy_f, 4))),
    MPR_cpi      = 100 * (log(cpi_f) - log(dplyr::lag(cpi_f, 4)))
  )

#------------------------------------------------
# AGGREGATED CPI using component annual contribution
#------------------------------------------------

# Adding weights into dataset
fc_yoy <- fc_yoy %>%
  left_join(
    df_fcst %>%
      select(date, s_wgt, f_wgt, e_wgt, cg_wgt, encont_f),
    by = "date"
  )

# Contributions by components (UNRECONCILED BASE)
fc_yoy <- fc_yoy %>%
  mutate(
    c_f  = f_wgt  * food_at_yoy  / 1000,
    c_s  = s_wgt  * services_yoy / 1000,
    c_e  = e_wgt  * energy_yoy   / 1000,
    c_cg = cg_wgt * core_gds_yoy / 1000
  )

# Base bottom-up CPI and residuals
# Keep energy fixed using CECD energy contribution path: encont_f
fc_yoy <- fc_yoy %>%
  mutate(
    non_energy_target         = MPR_cpi - encont_f,
    non_energy_bottom_up_base = c_f + c_s + c_cg,
    gap_ne                    = non_energy_target - non_energy_bottom_up_base,
    cpi_bottom_up_base        = non_energy_bottom_up_base + encont_f,
    cpi_resid_base            = MPR_cpi - cpi_bottom_up_base
  )

#------------------------------------------------
# VERSION 0: CURRENT RULE
#   Put the full non-energy residual into core goods
#------------------------------------------------
fc_yoy <- fc_yoy %>%
  mutate(
    c_f_v0  = c_f,
    c_s_v0  = c_s,
    c_cg_v0 = c_cg + gap_ne,

    non_energy_bottom_up_v0 = c_f_v0 + c_s_v0 + c_cg_v0,
    cpi_bottom_up_v0        = non_energy_bottom_up_v0 + encont_f,
    cpi_resid_v0            = MPR_cpi - cpi_bottom_up_v0
  )

#------------------------------------------------
# VERSION 1: DYNAMIC M26-WEIGHTED RECONCILIATION
#   Quarter-specific M26_Baseline weights absorb the non-energy gap
#------------------------------------------------
fc_yoy <- fc_yoy %>%
  mutate(
    w_f_v1  = f_wgt,
    w_s_v1  = s_wgt,
    w_cg_v1 = cg_wgt,
    w_sum_v1 = w_f_v1 + w_s_v1 + w_cg_v1,

    c_f_v1  = c_f  + gap_ne * w_f_v1  / w_sum_v1,
    c_s_v1  = c_s  + gap_ne * w_s_v1  / w_sum_v1,
    c_cg_v1 = c_cg + gap_ne * w_cg_v1 / w_sum_v1,

    non_energy_bottom_up_v1 = c_f_v1 + c_s_v1 + c_cg_v1,
    cpi_bottom_up_v1        = non_energy_bottom_up_v1 + encont_f,
    cpi_resid_v1            = MPR_cpi - cpi_bottom_up_v1
  )

#------------------------------------------------
# VERSION 2: DATA-DRIVEN C-WLS STYLE RECONCILIATION
#   Use in-sample model RMSE as flexibility weights
#   (Later you can replace these with pseudo-vintage RMSFEs)
#------------------------------------------------
rmse_services_qoq <- sqrt(mean(resid(s_adl)^2,   na.rm = TRUE))
rmse_core_qoq     <- sqrt(mean(resid(cg_adl)^2,  na.rm = TRUE))
rmse_food_qoq     <- sqrt(mean(resid(ecm_food)^2, na.rm = TRUE))

# Variance-style weights (relative flexibility)
w_f_v2  <- rmse_food_qoq^2
w_s_v2  <- rmse_services_qoq^2
w_cg_v2 <- rmse_core_qoq^2
w_sum_v2 <- w_f_v2 + w_s_v2 + w_cg_v2

fc_yoy <- fc_yoy %>%
  mutate(
    c_f_v2  = c_f  + gap_ne * w_f_v2  / w_sum_v2,
    c_s_v2  = c_s  + gap_ne * w_s_v2  / w_sum_v2,
    c_cg_v2 = c_cg + gap_ne * w_cg_v2 / w_sum_v2,

    non_energy_bottom_up_v2 = c_f_v2 + c_s_v2 + c_cg_v2,
    cpi_bottom_up_v2        = non_energy_bottom_up_v2 + encont_f,
    cpi_resid_v2            = MPR_cpi - cpi_bottom_up_v2
  )

#------------------------------------------------
# VERSION 3: HYBRID — BASKET SHARE × RMSE² WEIGHTS
#   Components that are large (high basket weight) AND
#   hard to predict (high RMSE) absorb more of the gap.
#   w_i = basket_wgt_i × rmse_i²  (time-varying × fixed scalar)
#------------------------------------------------
fc_yoy <- fc_yoy %>%
  mutate(
    w_f_v3   = f_wgt  * rmse_food_qoq^2,
    w_s_v3   = s_wgt  * rmse_services_qoq^2,
    w_cg_v3  = cg_wgt * rmse_core_qoq^2,
    w_sum_v3 = w_f_v3 + w_s_v3 + w_cg_v3,

    c_f_v3  = c_f  + gap_ne * w_f_v3  / w_sum_v3,
    c_s_v3  = c_s  + gap_ne * w_s_v3  / w_sum_v3,
    c_cg_v3 = c_cg + gap_ne * w_cg_v3 / w_sum_v3,

    non_energy_bottom_up_v3 = c_f_v3 + c_s_v3 + c_cg_v3,
    cpi_bottom_up_v3        = non_energy_bottom_up_v3 + encont_f,
    cpi_resid_v3            = MPR_cpi - cpi_bottom_up_v3
  )

#------------------------------------------------
# SAVE FULL RESULTS BEFORE PLOTTING FILTER
#------------------------------------------------
fc_yoy_all <- fc_yoy

#------------------------------------------------
# BUILD VERSION COMPARISON TABLES
#------------------------------------------------
base_contrib <- fc_yoy_all %>%
  select(date, c_f, c_s, c_cg)

build_version_df <- function(df, suffix, version_label) {
  df %>%
    transmute(
      date,
      version = version_label,
      MPR_cpi,
      non_energy_target,
      gap_ne,
      food       = .data[[paste0("c_f_", suffix)]],
      services   = .data[[paste0("c_s_", suffix)]],
      core_goods = .data[[paste0("c_cg_", suffix)]],
      energy     = encont_f,
      non_energy_bottom_up = .data[[paste0("non_energy_bottom_up_", suffix)]],
      cpi_bottom_up        = .data[[paste0("cpi_bottom_up_", suffix)]],
      cpi_resid            = .data[[paste0("cpi_resid_", suffix)]]
    )
}

versions_wide <- bind_rows(
  build_version_df(fc_yoy_all, "v0", "Version 0 - all residual to core goods"),
  build_version_df(fc_yoy_all, "v1", "Version 1 - dynamic M26 weights"),
  build_version_df(fc_yoy_all, "v2", "Version 2 - RMSE-weighted"),
  build_version_df(fc_yoy_all, "v3", "Version 3 - hybrid basket x RMSE weights")
) %>%
  left_join(base_contrib, by = "date") %>%
  mutate(
    food_adj       = food       - c_f,
    services_adj   = services   - c_s,
    core_goods_adj = core_goods - c_cg
  )

versions_long_contrib <- versions_wide %>%
  mutate(date_plot = as.Date(date)) %>%
  pivot_longer(
    cols = c(food, energy, services, core_goods),
    names_to = "component",
    values_to = "contribution"
  )

versions_long_adjust <- versions_wide %>%
  mutate(date_plot = as.Date(date)) %>%
  pivot_longer(
    cols = c(food_adj, services_adj, core_goods_adj),
    names_to = "component",
    values_to = "adjustment"
  ) %>%
  mutate(
    component = dplyr::recode(
      component,
      food_adj       = "Food",
      services_adj   = "Services",
      core_goods_adj = "Core goods"
    )
  )

#------------------------------------------------
# WEIGHTS / MODEL FIT TABLES
#------------------------------------------------
weights_v1 <- tibble(
  date = fc_yoy_all$date,
  version = "Version 1 - dynamic M26 weights reconciliation",
  w_food = fc_yoy_all$w_f_v1,
  w_services = fc_yoy_all$w_s_v1,
  w_core_goods = fc_yoy_all$w_cg_v1,
  w_sum = fc_yoy_all$w_sum_v1
)

weights_v2 <- tibble(
  version   = "Version 2 - RMSE-weighted",
  component = c("Food", "Services", "Core goods"),
  rmse_qoq  = c(rmse_food_qoq, rmse_services_qoq, rmse_core_qoq),
  weight    = c(w_f_v2, w_s_v2, w_cg_v2)
)

# Version 3: hybrid weights are time-varying (basket) × fixed scalar (RMSE²)
weights_v3 <- tibble(
  date         = fc_yoy_all$date,
  version      = "Version 3 - hybrid basket x RMSE weights",
  w_food       = fc_yoy_all$w_f_v3,
  w_services   = fc_yoy_all$w_s_v3,
  w_core_goods = fc_yoy_all$w_cg_v3,
  w_sum        = fc_yoy_all$w_sum_v3
)

model_fit_stats <- tibble(
  component = c("Food ECM", "Services ADL", "Core goods ADL"),
  rmse_qoq  = c(rmse_food_qoq, rmse_services_qoq, rmse_core_qoq)
)

# Side-by-side comparison in wide format: separate columns per version
version_component_compare <- fc_yoy_all %>%
  transmute(
    date,
    MPR_cpi,
    energy = encont_f,

    estimated_food = c_f,
    food_v0 = c_f_v0,
    food_v1 = c_f_v1,
    food_v2 = c_f_v2,
    food_v3 = c_f_v3,

    estimated_services = c_s,
    services_v0 = c_s_v0,
    services_v1 = c_s_v1,
    services_v2 = c_s_v2,
    services_v3 = c_s_v3,

    estimated_core = c_cg,
    core_v0 = c_cg_v0,
    core_v1 = c_cg_v1,
    core_v2 = c_cg_v2,
    core_v3 = c_cg_v3,

    cpi_bottom_up_v0,
    cpi_bottom_up_v1,
    cpi_bottom_up_v2,
    cpi_bottom_up_v3,

    cpi_resid_v0,
    cpi_resid_v1,
    cpi_resid_v2,
    cpi_resid_v3
  )

# Workbook tab guide
read_me <- tibble(
  sheet = c(
    "read_me",
    "formula_sheet",
    "fc_yoy_all",
    "versions_wide",
    "version_component_compare",
    "versions_long_contrib",
    "versions_long_adjust",
    "weights_v1",
    "weights_v2",
    "weights_v3",
    "model_fit_stats"
  ),
  description = c(
    "Tab guide for this workbook.",
    "Compact formulas for V0-V3 reconciliation rules.",
    "Master quarterly dataset with base and reconciled series (all versions).",
    "One row per date-version with headline, bottom-up and component contributions.",
    "Wide side-by-side table by date with food/services/core columns for V0-V3.",
    "Long format contributions by date-version-component (for stacked charts).",
    "Long format adjustments vs unreconciled base contributions.",
    "Version 1 dynamic M26 basket weights by quarter.",
    "Version 2 static RMSE-based weights by component.",
    "Version 3 hybrid (basket x RMSE^2) weights by quarter.",
    "Model fit summary used for RMSE-based weighting."
  )
)

formula_sheet <- tibble(
  block = c(
    "Base identities",
    "Base identities",
    "Base identities",
    "Version 0",
    "Version 0",
    "Version 1",
    "Version 1",
    "Version 2",
    "Version 2",
    "Version 3",
    "Version 3"
  ),
  expression = c(
    "non_energy_target = MPR_cpi - encont_f",
    "non_energy_bottom_up_base = c_f + c_s + c_cg",
    "gap_ne = non_energy_target - non_energy_bottom_up_base",
    "c_f_v0 = c_f; c_s_v0 = c_s; c_cg_v0 = c_cg + gap_ne",
    "cpi_bottom_up_v0 = (c_f_v0 + c_s_v0 + c_cg_v0) + encont_f",
    "w_f_v1 = f_wgt; w_s_v1 = s_wgt; w_cg_v1 = cg_wgt",
    "c_i_v1 = c_i + gap_ne * w_i_v1 / (w_f_v1 + w_s_v1 + w_cg_v1)",
    "w_f_v2 = rmse_food_qoq^2; w_s_v2 = rmse_services_qoq^2; w_cg_v2 = rmse_core_qoq^2",
    "c_i_v2 = c_i + gap_ne * w_i_v2 / (w_f_v2 + w_s_v2 + w_cg_v2)",
    "w_i_v3 = basket_wgt_i * rmse_i^2",
    "c_i_v3 = c_i + gap_ne * w_i_v3 / (w_f_v3 + w_s_v3 + w_cg_v3)"
  ),
  note = c(
    "All rates/contributions are in percentage points unless noted.",
    "Unreconciled non-energy sum from model outputs.",
    "Residual non-energy gap to be redistributed.",
    "All non-energy gap allocated to core goods.",
    "Residual after reconciliation: cpi_resid_v0 = MPR_cpi - cpi_bottom_up_v0.",
    "Quarter-specific basket weights from baseline.",
    "Apply for i in {f, s, cg}.",
    "Static RMSE-based weights by component.",
    "Apply for i in {f, s, cg}.",
    "Hybrid dynamic weights: basket share x uncertainty.",
    "Apply for i in {f, s, cg}."
  )
)

#------------------------------------------------
# WRITE OUTPUTS TO EXCEL
#------------------------------------------------
output_xlsx <- "cpi_reconciliation_versions.xlsx"

wb <- createWorkbook()

addWorksheet(wb, "read_me")
writeData(wb, "read_me", read_me)

addWorksheet(wb, "formula_sheet")
writeData(wb, "formula_sheet", formula_sheet)

addWorksheet(wb, "fc_yoy_all")
writeData(wb, "fc_yoy_all", fc_yoy_all)

addWorksheet(wb, "versions_wide")
writeData(wb, "versions_wide", versions_wide)

addWorksheet(wb, "version_component_compare")
writeData(wb, "version_component_compare", version_component_compare)

addWorksheet(wb, "versions_long_contrib")
writeData(wb, "versions_long_contrib", versions_long_contrib)

addWorksheet(wb, "versions_long_adjust")
writeData(wb, "versions_long_adjust", versions_long_adjust)

addWorksheet(wb, "weights_v1")
writeData(wb, "weights_v1", weights_v1)

addWorksheet(wb, "weights_v2")
writeData(wb, "weights_v2", weights_v2)

addWorksheet(wb, "weights_v3")
writeData(wb, "weights_v3", weights_v3)

addWorksheet(wb, "model_fit_stats")
writeData(wb, "model_fit_stats", model_fit_stats)

saveWorkbook(wb, output_xlsx, overwrite = TRUE)

#------------------------------------------------
# PLOTTING
#------------------------------------------------
library(boeCharts)

# Main brand colours
boe_dark_blue <- "#12273F"
boe_aqua      <- "#3CD7D9"
boe_stone     <- "#C4C9CF"

# Secondary accents
boe_orange <- "#FF7300"
boe_purple <- "#9E71FE"
boe_gold   <- "#D4AF37"

# BoE-style theme
theme_boe <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      text = element_text(colour = boe_dark_blue),
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(size = base_size),
      plot.caption = element_text(size = base_size - 3, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = alpha(boe_stone, 0.55), linewidth = 0.4),
      axis.title = element_text(colour = boe_dark_blue),
      axis.text  = element_text(colour = boe_dark_blue),
      axis.ticks = element_line(colour = alpha(boe_dark_blue, 0.35)),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key = element_rect(fill = "white", colour = NA),
      plot.margin = margin(10, 12, 8, 10)
    )
}

# Plot window
plot_start <- as.yearqtr("2025 Q1")
plot_end   <- as.yearqtr("2029 Q2")

versions_plot <- versions_wide %>%
  filter(date >= plot_start, date <= plot_end) %>%
  mutate(date_plot = as.Date(date))

versions_long_contrib_plot <- versions_long_contrib %>%
  filter(date >= plot_start, date <= plot_end)

versions_long_adjust_plot <- versions_long_adjust %>%
  filter(date >= plot_start, date <= plot_end)

#------------------------------------------------
# GRAPH 1: Aggregate comparison across versions
#------------------------------------------------
plot_aggregate_compare <- versions_plot %>%
  select(date_plot, version, MPR_cpi, cpi_bottom_up) %>%
  pivot_longer(
    cols = c(MPR_cpi, cpi_bottom_up),
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    series = dplyr::recode(
      series,
      MPR_cpi = "MPR CPI (Baseline)",
      cpi_bottom_up = "Bottom-up CPI"
    )
  ) %>%
  ggplot(aes(x = date_plot, y = value, colour = series)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ version, ncol = 1) +
  scale_colour_manual(values = c(
    "MPR CPI (Baseline)" = boe_orange,
    "Bottom-up CPI"      = boe_dark_blue
  )) +
  scale_x_date(
    name = NULL,
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = "Aggregate CPI comparison across reconciliation versions",
    subtitle = "MPR headline CPI vs bottom-up CPI",
    y = "Percentage change (yoy)"
  ) +
  theme_boe()

ggsave(
  "cpi_aggregate_compare_versions.png",
  plot_aggregate_compare,
  width = 11, height = 9, dpi = 300
)

#------------------------------------------------
# GRAPH 2: Stacked contributions across versions
#------------------------------------------------
bank_cols <- c(
  food       = boe_aqua,
  energy     = boe_orange,
  services   = boe_purple,
  core_goods = boe_gold
)

component_labs <- c(
  food       = "Food",
  energy     = "Energy",
  services   = "Services",
  core_goods = "Core goods"
)

plot_contrib_compare <- ggplot(
  versions_long_contrib_plot,
  aes(x = date_plot, y = contribution, fill = component)
) +
  geom_col(width = 85, colour = NA) +
  geom_line(
    data = versions_plot,
    aes(x = date_plot, y = MPR_cpi),
    colour = boe_dark_blue,
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, colour = alpha(boe_dark_blue, 0.6), linewidth = 0.6) +
  facet_wrap(~ version, ncol = 1) +
  scale_fill_manual(values = bank_cols, labels = component_labs) +
  scale_x_date(
    name = NULL,
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    name = "Contribution to CPI (p.p.)",
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  labs(
    title = "Contribution of CPI components to MPR CPI across reconciliation versions",
    subtitle = "Energy path held fixed; non-energy residual redistributed"
  ) +
  theme_boe()

ggsave(
  "cpi_contributions_compare_versions.png",
  plot_contrib_compare,
  width = 11, height = 10, dpi = 300
)

#------------------------------------------------
# GRAPH 3: Component adjustments relative to unreconciled base
#------------------------------------------------
plot_adjustments_compare <- versions_long_adjust_plot %>%
  ggplot(aes(x = date_plot, y = adjustment, colour = component)) +
  geom_hline(yintercept = 0, colour = alpha(boe_dark_blue, 0.6), linewidth = 0.6) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ version, ncol = 1) +
  scale_colour_manual(values = c(
    "Food"       = boe_aqua,
    "Services"   = boe_purple,
    "Core goods" = boe_gold
  )) +
  scale_x_date(
    name = NULL,
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = "Reconciliation adjustments relative to unreconciled base",
    subtitle = "Positive values mean the component absorbs more of the non-energy gap",
    y = "Adjustment to contribution (p.p.)"
  ) +
  theme_boe()

ggsave(
  "cpi_adjustments_compare_versions.png",
  plot_adjustments_compare,
  width = 11, height = 9, dpi = 300
)

#------------------------------------------------
# OPTIONAL: Keep your original YoY component chart
#------------------------------------------------
fc_yoy_plot <- fc_yoy_all %>%
  filter(date >= plot_start, date <= plot_end)

plot_components <- fc_yoy_plot %>%
  select(date, MPR_cpi, services_yoy, core_gds_yoy, food_at_yoy) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(
    series = factor(
      series,
      levels = c("MPR_cpi", "services_yoy", "core_gds_yoy", "food_at_yoy"),
      labels = c("MPR CPI (Baseline)", "Services", "Core goods", "Food")
    )
  ) %>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.9) +
  geom_vline(
    xintercept = as.numeric(as.yearqtr("2026 Q1")),
    linetype = "dashed",
    colour = "grey40"
  ) +
  scale_colour_manual(values = c(
    "MPR CPI (Baseline)" = boe_orange,
    "Services"           = boe_purple,
    "Core goods"         = boe_gold,
    "Food"               = boe_aqua
  )) +
  labs(x = NULL, y = "Percentage change (yoy)", colour = NULL) +
  theme_minimal(base_family = "sans")

ggsave("cpi_components_yoy.png", plot_components, width = 10, height = 5, dpi = 300)

if (interactive()) {
  print(plot_aggregate_compare)
  print(plot_contrib_compare)
  print(plot_adjustments_compare)
}

cat("Finished fcst_cpi_components.R\n")
cat("Generated files:\n")
cat(" - cpi_reconciliation_versions.xlsx\n")
cat(" - cpi_components_yoy.png\n")
cat(" - cpi_aggregate_compare_versions.png\n")
cat(" - cpi_contributions_compare_versions.png\n")
cat(" - cpi_adjustments_compare_versions.png\n")