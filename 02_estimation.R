library(car)
library(dplyr)
library(dynlm)
library(ecm)
library(lmtest)
library(purrr)
library(sandwich)
library(strucchange)
library(zoo)

cat("Starting 02_estimation.R...\n")

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
    stop(sprintf("%s is missing required columns: %s", label, paste(missing_cols, collapse = ", ")))
  }
}

project_root <- get_project_root()
output_dir <- file.path(project_root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

bundle_path <- file.path(output_dir, "prepared_data_bundle.rds")

if (file.exists(bundle_path)) {
  prepared_bundle <- readRDS(bundle_path)
  full_data <- prepared_bundle$full_data
} else {
  full_data_path <- file.path(output_dir, "full_data.rds")
  
  if (!file.exists(full_data_path)) {
    stop("Missing prepared data files. Run 01_prepare_data.R first.")
  }
  
  full_data <- readRDS(full_data_path)
}

assert_required_cols(
  full_data,
  c(
    "date", "services_qoq", "infl_exp", "eer_qoq", "pmdef_qoq",
    "core_gds_qoq", "food_at_qoq", "energy_qoq",
    "ln_food_at", "ln_eer", "ln_pmdef",
    "D_1991Q3", "D_2021Q2", "D_2023Q2",
    "D_2009Q2", "D_2023Q3", "D_2001Q2", "D_2008Q2"
  ),
  "full_data"
)

## --------------------------------------------------
#  1. MODEL ESTIMATION
## --------------------------------------------------

full_sample <- subset(
  full_data,
  date >= as.yearqtr("1991 Q2") &
    date <= as.yearqtr("2025 Q4")
)

s_adl <- lm(
  services_qoq ~ lag(services_qoq, 1) + infl_exp +
    eer_qoq + pmdef_qoq + D_1991Q3 + D_2021Q2 + D_2023Q2,
  data = full_sample
)

b_s <- coef(s_adl)

cg_adl <- lm(
  core_gds_qoq ~ 0 + lag(core_gds_qoq, 1) +
    eer_qoq + lag(pmdef_qoq, 2) +
    D_2009Q2 + D_2023Q3,
  data = full_sample
)

b_cg <- coef(cg_adl)

coint_food_lm <- lm(
  ln_food_at ~ ln_eer + ln_pmdef,
  data = full_sample
)

b_lr <- coef(coint_food_lm)

full_sample$ECT_food <- resid(coint_food_lm)
full_sample$ECT_food_L1 <- dplyr::lag(full_sample$ECT_food, 1)

ecm_food <- dynlm(
  food_at_qoq ~ 0 + lag(food_at_qoq, 2) + lag(food_at_qoq, 4) + lag(energy_qoq, 4) +
    lag(infl_exp, 1) + pmdef_qoq + lag(pmdef_qoq, 2) +
    ECT_food_L1 + D_2001Q2 + D_2008Q2,
  data = full_sample
)

b_ecm <- coef(ecm_food)

saveRDS(b_s, file.path(output_dir, "b_services.rds"))
saveRDS(b_cg, file.path(output_dir, "b_core_goods.rds"))
saveRDS(b_lr, file.path(output_dir, "b_food_lr.rds"))
saveRDS(b_ecm, file.path(output_dir, "b_food_ecm.rds"))

saveRDS(
  list(
    b_services = b_s,
    b_core_goods = b_cg,
    b_food_lr = b_lr,
    b_food_ecm = b_ecm,
    estimation_window = c("1991 Q2", "2025 Q4"),
    estimated_at = Sys.time()
  ),
  file.path(output_dir, "estimation_bundle.rds")
)

cat("Saved estimation outputs:\n")
cat(" - outputs/b_services.rds\n")
cat(" - outputs/b_core_goods.rds\n")
cat(" - outputs/b_food_lr.rds\n")
cat(" - outputs/b_food_ecm.rds\n")
cat(" - outputs/estimation_bundle.rds\n")
