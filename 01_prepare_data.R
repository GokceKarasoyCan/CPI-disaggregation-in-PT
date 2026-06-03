library(dplyr)
options(xts.warn_dplyr_breaks_lag = FALSE)
library(quantmod)
library(readxl)
library(tibble)
library(tidyr)
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

resolve_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop(sprintf("None of the candidate paths exist: %s", paste(paths, collapse = ", ")))
  }
  existing[[1]]
}

first_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    return(NA_character_)
  }
  existing[[1]]
}

resolve_dataset_paths <- function(stem) {
  list(
    parquet = first_existing_path(c(
      file.path(data_dir, paste0(stem, ".parquet")),
      file.path(data_dir, paste0(stem, "_latest.parquet"))
    )),
    csv = first_existing_path(c(
      file.path(data_dir, paste0(stem, ".csv")),
      file.path(data_dir, paste0(stem, "_latest.csv"))
    ))
  )
}

read_pipeline_dataset <- function(paths) {
  if (!is.na(paths$parquet) && requireNamespace("arrow", quietly = TRUE)) {
    return(arrow::read_parquet(paths$parquet, as_data_frame = TRUE))
  }
  if (!is.na(paths$csv)) {
    return(read.csv(paths$csv, stringsAsFactors = FALSE))
  }
  stop(sprintf("Could not load dataset from parquet or CSV: %s / %s", paths$parquet, paths$csv))
}

project_root <- get_project_root()
data_dir <- file.path(project_root, "data", "fame_exports")
output_dir <- file.path(project_root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

estimation_paths <- resolve_dataset_paths("estimation_data")
baseline_paths <- resolve_dataset_paths("baseline_data")

## --------------------------------------------------
#  1. DATA PREPARATION
## --------------------------------------------------

#----- 1.1 Data for estimation--------

cpi_tbl <- read_pipeline_dataset(estimation_paths)
cpi_tbl$date <- as.yearqtr(cpi_tbl$date)

cpi_tbl <-
  cpi_tbl %>%
  mutate(pmdef = pmdef * 100)

df_cpi_level <-
  cpi_tbl %>%
  dplyr::select(date, cpi, core_gds, services, food_at, energy, pmdef, eer, infl_exp)

df_cpi_log <-
  df_cpi_level %>%
  mutate(across(c(cpi, core_gds, services, food_at,
                  energy, pmdef, eer),
                ~ log(.),
                .names = "ln_{.col}"))

df_cpi_qoq <-
  df_cpi_log %>%
  mutate(across(c(cpi, core_gds, services, food_at,
                  energy, pmdef, eer),
                ~ Delt(., type = "log") * 100,
                .names = "{.col}_qoq")) %>%
  slice(-1)

df_cpi_yoy <-
  df_cpi_log %>%
  mutate(across(c(cpi, core_gds, services, food_at,
                  energy, pmdef, eer),
                ~ Delt(., k = 4, type = "log") * 100,
                .names = "{.col}_yoy"))

df_cpi_tbl <-
  df_cpi_qoq %>%
  mutate(D_1991Q3 = as.integer(date >= as.yearqtr("1991 Q3") & date <= as.yearqtr("1991 Q3"))) %>%
  mutate(D_2001Q2 = as.integer(date >= as.yearqtr("2001 Q2") & date <= as.yearqtr("2001 Q2"))) %>%
  mutate(D_2008Q2 = as.integer(date >= as.yearqtr("2008 Q2") & date <= as.yearqtr("2008 Q2"))) %>%
  mutate(D_2009Q2 = as.integer(date >= as.yearqtr("2009 Q2") & date <= as.yearqtr("2009 Q2"))) %>%
  mutate(D_2021Q2 = as.integer(date >= as.yearqtr("2021 Q2") & date <= as.yearqtr("2021 Q2"))) %>%
  mutate(D_2021Q2Q3 = as.integer(date >= as.yearqtr("2021 Q2") & date <= as.yearqtr("2021 Q3"))) %>%
  mutate(D_2023Q2 = as.integer(date >= as.yearqtr("2023 Q2") & date <= as.yearqtr("2023 Q2"))) %>%
  mutate(D_2023Q3 = as.integer(date >= as.yearqtr("2023 Q3") & date <= as.yearqtr("2023 Q3")))

full_data <-
  df_cpi_tbl %>%
  dplyr::select(date, cpi_qoq, services_qoq,
                core_gds_qoq, food_at_qoq,
                infl_exp, pmdef_qoq, energy_qoq, eer_qoq,
                food_at, ln_food_at, ln_pmdef, ln_eer,
                D_1991Q3, D_2001Q2, D_2008Q2,
                D_2009Q2, D_2021Q2, D_2021Q2Q3, D_2023Q2, D_2023Q3)

#----- 1.2 Data for forecasting including CECD FORECAST and wider BANK FORECAST

df_fcst <- read_pipeline_dataset(baseline_paths)
df_fcst$date <- as.yearqtr(df_fcst$date)

df_fcst_level <-
  df_fcst %>%
  mutate(pmdef_f = pmdef_f * 100) %>%
  dplyr::select(date, stif_cpi, core_gds, services, food_at, energy_f, pmdef_f, eer_f, infl_exp_f, cpi_f)

df_fcst_growth <-
  df_fcst_level %>%
  mutate(across(c(food_at, pmdef_f, eer_f),
                ~ log(.),
                .names = "ln_{.col}")) %>%
  mutate(across(c(stif_cpi, core_gds, services, food_at,
                  energy_f, pmdef_f, eer_f, cpi_f),
                ~ Delt(., type = "log") * 100,
                .names = "{.col}_qoq")) %>%
  mutate(across(c(stif_cpi, core_gds, services, food_at,
                  energy_f, pmdef_f, eer_f, cpi_f),
                ~ Delt(., k = 4, type = "log") * 100,
                .names = "{.col}_yoy"))

saveRDS(full_data, file.path(output_dir, "full_data.rds"))
saveRDS(df_fcst_growth, file.path(output_dir, "fcst_growth.rds"))
saveRDS(df_fcst_level, file.path(output_dir, "fcst_level.rds"))
saveRDS(df_fcst, file.path(output_dir, "fcst_raw.rds"))

cat("Saved processed datasets:\n")
cat("Input preference: parquet first, CSV fallback if needed\n")
cat(" - outputs/full_data.rds\n")
cat(" - outputs/fcst_growth.rds\n")
cat(" - outputs/fcst_level.rds\n")
cat(" - outputs/fcst_raw.rds\n")
