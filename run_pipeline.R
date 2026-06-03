## --------------------------------------------------
#  CPI component forecast pipeline runner
## --------------------------------------------------
# Executes the modular workflow in order:
#   1. 01_prepare_data.R
#   2. 02_estimation.R
#   3. 03_forecasting.R

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
scripts <- c("01_prepare_data.R", "02_estimation.R", "03_forecasting.R")

cat("Starting run_pipeline.R...\n")

for (script_name in scripts) {
  script_path <- file.path(project_root, script_name)
  if (!file.exists(script_path)) {
    stop(sprintf("Missing pipeline script: %s", script_path))
  }

  cat(sprintf("\nRunning %s\n", script_name))
  source(script_path, chdir = TRUE)
}

cat("\nFinished run_pipeline.R\n")