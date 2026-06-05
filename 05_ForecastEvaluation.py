# ============================================================
# Forecast Evaluation Dashboard
# ============================================================
#
# This script:
# 1. Loads forecast and outturn parquet files
# 2. Converts quarter strings like "2020 Q1" to quarter-end dates
# 3. Ensures key numeric columns are numeric
# 4. Adds the metric column expected by the forecast_evaluation package
# 5. Builds the ForecastData object
# 6. Runs the dashboard
#
# To switch series, only change:
#     stub = "cg"
# to:
#     stub = "services"
# or:
#     stub = "food"
#
# Expected files:
#   data/forecast_datacg.parquet
#   data/outturn_datacg.parquet
#   data/forecast_dataservices.parquet
#   data/outturn_dataservices.parquet
#   data/forecast_datafood.parquet
#   data/outturn_datafood.parquet
# ============================================================

import pandas as pd
import forecast_evaluation as fe

# ------------------------------------------------------------
# Choose the series to evaluate
# ------------------------------------------------------------
# "cg"        -> Core goods inflation
# "services"  -> Services inflation
# "food"      -> Food inflation
stub = "cg"

# ------------------------------------------------------------
# File paths
# ------------------------------------------------------------
forecast_path = f"data/forecast_data{stub}.parquet"
outturn_path = f"data/outturn_data{stub}.parquet"

# ------------------------------------------------------------
# Load forecast and outturn data
# ------------------------------------------------------------
forecast_data = pd.read_parquet(forecast_path)
outturn_data = pd.read_parquet(outturn_path)

# ------------------------------------------------------------
# Convert quarter strings to quarter-end timestamps
# ------------------------------------------------------------
# The R files store dates as strings such as "2020 Q1".
# The forecast_evaluation workflow works better if these are
# converted to proper timestamps.
#
# Example:
#   "2020 Q1" -> 2020-03-31
# ------------------------------------------------------------
forecast_data["date"] = (
    pd.PeriodIndex(
        forecast_data["date"].astype(str).str.replace(r"\s+", "", regex=True),
        freq="Q"
    )
    .to_timestamp(how="end")
    .normalize()
)

forecast_data["vintage_date"] = (
    pd.PeriodIndex(
        forecast_data["vintage_date"].astype(str).str.replace(r"\s+", "", regex=True),
        freq="Q"
    )
    .to_timestamp(how="end")
    .normalize()
)

outturn_data["date"] = (
    pd.PeriodIndex(
        outturn_data["date"].astype(str).str.replace(r"\s+", "", regex=True),
        freq="Q"
    )
    .to_timestamp(how="end")
    .normalize()
)

outturn_data["vintage_date"] = (
    pd.PeriodIndex(
        outturn_data["vintage_date"].astype(str).str.replace(r"\s+", "", regex=True),
        freq="Q"
    )
    .to_timestamp(how="end")
    .normalize()
)

# ------------------------------------------------------------
# Ensure numeric columns are numeric
# ------------------------------------------------------------
# This avoids hidden type problems if parquet/csv imports vary.
# ------------------------------------------------------------
forecast_data["value"] = pd.to_numeric(forecast_data["value"], errors="coerce")
outturn_data["value"] = pd.to_numeric(outturn_data["value"], errors="coerce")

forecast_data["forecast_horizon"] = pd.to_numeric(
    forecast_data["forecast_horizon"], errors="coerce"
)
outturn_data["forecast_horizon"] = pd.to_numeric(
    outturn_data["forecast_horizon"], errors="coerce"
)

# ------------------------------------------------------------
# Add metric column expected by forecast_evaluation
# ------------------------------------------------------------
forecast_data["metric"] = "pop"
outturn_data["metric"] = "pop"

# ------------------------------------------------------------
# Optional quick checks
# ------------------------------------------------------------
print("--------------------------------------------------")
print(f"Series selected: {stub}")
print("--------------------------------------------------")

print("\nForecast preview:")
print(forecast_data.head())

print("\nOutturn preview:")
print(outturn_data.head())

print("\nForecast columns:")
print(forecast_data.columns.tolist())

print("\nOutturn columns:")
print(outturn_data.columns.tolist())

print("\nForecast variable values:")
if "variable" in forecast_data.columns:
    print(forecast_data["variable"].drop_duplicates().tolist())

print("\nOutturn variable values:")
if "variable" in outturn_data.columns:
    print(outturn_data["variable"].drop_duplicates().tolist())

print("\nForecast source values:")
if "source" in forecast_data.columns:
    print(forecast_data["source"].drop_duplicates().tolist())

print("\nForecast horizon summary:")
print(forecast_data["forecast_horizon"].describe())

print("\nOutturn horizon summary:")
print(outturn_data["forecast_horizon"].describe())

# ------------------------------------------------------------
# OPTIONAL FILTER
# ------------------------------------------------------------
# If you only want true forward-looking evaluation, uncomment
# the next block and use forecast_data_eval / outturn_data_eval
# instead of forecast_data / outturn_data below.
#
# Note:
#   forecast_horizon < 0  -> before the vintage
#   forecast_horizon = 0  -> vintage quarter itself
#   forecast_horizon > 0  -> future relative to the vintage
# ------------------------------------------------------------
# forecast_data_eval = forecast_data.loc[forecast_data["forecast_horizon"] >= 1].copy()
# outturn_data_eval = outturn_data.loc[outturn_data["forecast_horizon"] >= 1].copy()

# ------------------------------------------------------------
# Build ForecastData object
# ------------------------------------------------------------
# Default: use the full panel
# ------------------------------------------------------------
fd = fe.ForecastData(
    forecasts_data=forecast_data,
    outturns_data=outturn_data
)

# ------------------------------------------------------------
# If you prefer only positive horizons, use this instead:
# ------------------------------------------------------------
# fd = fe.ForecastData(
#     forecasts_data=forecast_data_eval,
#     outturns_data=outturn_data_eval
# )

# ------------------------------------------------------------
# Run dashboard
# ------------------------------------------------------------
fd.run_dashboard(host="127.0.0.1")
