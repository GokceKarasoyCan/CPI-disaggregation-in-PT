library(readxl)
library(tibble)
library(dplyr)
library(tidyr)

options(xts.warn_dplyr_breaks_lag = FALSE)
library(quantmod)
library(zoo)

cat("Starting 01_prepare_data.R...\n")

get_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, args)
  if (length(match) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[match[1]]))))
  }
  normalizePath(getwd())
}

project_root <- get_project_root()
output_dir <- file.path(project_root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

excel_data_path <- file.path(project_root, "data_set_v3.xlsx")

if (!file.exists(excel_data_path)) {
  stop(sprintf("Required workbook not found: %s", excel_data_path))
}

## --------------------------------------------------
#  1. DATA PREPARATION
## --------------------------------------------------

#----- 1.1 Data for estimation--------

# Importing raw data 1990Q2-2025Q4
cpi_tbl <- 
  readxl::read_excel(
    excel_data_path,
    sheet = "Estimation_data",
    range = "A11:L154"
  )

cpi_tbl$date <- as.yearqtr(cpi_tbl$date)

cpi_tbl <-
  cpi_tbl %>%
  mutate(pmdef = pmdef * 100)

# Data in levels (Indexes, rates)
df_cpi_level <- 
  cpi_tbl %>%
  dplyr::select(date, cpi, core_gds, services, food_at, energy, pmdef, eer, infl_exp)

# Data in log-levels
df_cpi_log <-
  df_cpi_level %>%
  mutate(across(
    c(cpi, core_gds, services, food_at, energy, pmdef, eer),
    ~ log(.),
    .names = "ln_{.col}"
  ))

# Data in quarterly percentage change - qoq
df_cpi_qoq <-
  df_cpi_log %>%
  mutate(across(
    c(cpi, core_gds, services, food_at, energy, pmdef, eer),
    ~ Delt(., type = "log") * 100,
    .names = "{.col}_qoq"
  )) %>%
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

# creating dummies
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

# Importing wider forecast for inflation and activity
df_fcst <- 
  readxl::read_excel(
    excel_data_path,
    sheet = "M26_Baseline",
    range = "A11:V145"
  )

df_fcst$date <- as.yearqtr(df_fcst$date)

# Data in levels
df_fcst_level <- 
  df_fcst %>%
  mutate(pmdef_f = pmdef_f * 100) %>%
  dplyr::select(date, stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, infl_exp_f, cpi_f)

# Data in log-levels, qoq and yoy growth
df_fcst_growth <-
  df_fcst_level %>%
  mutate(across(
    c(food_at, pmdef_f, eer_f),
    ~ log(.),
    .names = "ln_{.col}"
  )) %>%
  mutate(across(
    c(stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, cpi_f),
    ~ Delt(., type = "log") * 100,
    .names = "{.col}_qoq"
  )) %>%
  mutate(across(
    c(stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, cpi_f),
    ~ Delt(., k = 4, type = "log") * 100,
    .names = "{.col}_yoy"
  ))

## --------------------------------------------------
#  2. SAVE OUTPUTS
## --------------------------------------------------

saveRDS(full_data, file.path(output_dir, "full_data.rds"))
saveRDS(df_fcst_growth, file.path(output_dir, "fcst_growth.rds"))
saveRDS(df_fcst_level, file.path(output_dir, "fcst_level.rds"))
saveRDS(df_fcst, file.path(output_dir, "fcst_raw.rds"))

saveRDS(
  list(
    full_data = full_data,
    fcst_growth = df_fcst_growth,
    fcst_level = df_fcst_level,
    fcst_raw = df_fcst,
    source_workbook = excel_data_path,
    prepared_at = Sys.time()
  ),
  file.path(output_dir, "prepared_data_bundle.rds")
)

cat("Saved processed datasets:\n")
cat(sprintf(" - %s\n", file.path(output_dir, "full_data.rds")))
cat(sprintf(" - %s\n", file.path(output_dir, "fcst_growth.rds")))
cat(sprintf(" - %s\n", file.path(output_dir, "fcst_level.rds")))
cat(sprintf(" - %s\n", file.path(output_dir, "fcst_raw.rds")))
cat(sprintf(" - %s\n", file.path(output_dir, "prepared_data_bundle.rds")))
cat("Finished 01_prepare_data.R\n")
