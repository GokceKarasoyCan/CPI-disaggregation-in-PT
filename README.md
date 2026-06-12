# CPI-disaggregation-in-PT

Disaggregated CPI forecasting for Portugal. Models are estimated separately for three non-energy components — **food**, **services**, and **core goods** — and their contributions are reconciled to match an aggregate CPI path.

---

## Data preparation

Data is sourced from two Excel workbooks that must be in the project root before running any pipeline:

| File | Content |
|------|---------|
| `data_set_v3.xlsx` | Sheet `Estimation_data` — historical levels for food, services, core goods, energy, CPI and regressors |
| `data_set_vintages.xlsx` | One sheet per vintage (`M23`, `M24`, `M25`, `M26`, …) — forecast inputs and basket weights (`f_wgt`, `s_wgt`, `cg_wgt`) used in V1/V3 reconciliation |

> **Note:** `collect_data.ipynb` and `collect_data_replicate.ipynb` are work-in-progress scripts intended to automate data extraction from FAME. They are not yet operational. All data must currently be maintained manually in the Excel files above.

---

## Standard pipeline — `run_pipeline.R`

Runs the **single-version** workflow end-to-end in four steps:

```
Rscript run_pipeline.R
```

| Step | Script | Purpose |
|------|--------|---------|
| 1 | `01_prepare_data.R` | Load and clean estimation data; save `outputs/full_data.rds` |
| 2 | `02_estimation.R` | Estimate ADL/ECM models for food, services, core goods; save `outputs/estimation_bundle.rds` |
| 3 | `03_forecasting.R` | Run out-of-sample recursive forecasts across vintage windows; save `outputs/oos_forecasts_13q.rds` |
| 4 | `04_generateforecastandoutturns.R` | Produce final forecast and outturn comparison files in `data/` |

The reconciliation in step 3/4 uses a single approach: any non-energy gap is allocated entirely to **core goods** (equivalent to V0 in the versioned pipeline).

---

## Versioned pipeline — `run_pipeline_versions.R`

Runs the same estimation but evaluates **four reconciliation strategies** (V0–V3) in parallel:

```
Rscript run_pipeline_versions.R
```

| Step | Script | Purpose |
|------|--------|---------|
| 1 | `01_prepare_data.R` | Same as standard pipeline |
| 2 | `02_estimation.R` | Same as standard pipeline |
| 3 | `03versions_forecasting.R` | OOS forecasting with V0–V3 reconciliation variants; saves `outputs/oos_forecasts_versions_13q.rds` |
| 4 | `04versions_generateforecastandoutturns.R` | Evaluates forecast accuracy per version vs realised component outturns (food/services/core yoy); saves RMSE scorecards and winner tables in `data/` |

### Reconciliation versions

| Version | Logic |
|---------|-------|
| **V0** | All non-energy gap assigned to core goods |
| **V1** | Gap distributed using dynamic basket weights (`f_wgt`, `s_wgt`, `cg_wgt`) from the current vintage |
| **V2** | Gap distributed using static RMSE² weights derived from in-sample model fit |
| **V3** | Hybrid: basket weight × RMSE² combined |

### Key outputs (`data/`)

| File | Content |
|------|---------|
| `forecast_versions_component_scorecard.csv` | RMSE by version × component (overall) |
| `forecast_versions_component_scorecard_by_horizon.csv` | RMSE by version × component × forecast horizon |
| `forecast_versions_component_winners_rmse.csv` | Best version per component (lowest RMSE) |
| `forecast_versions_component_winners_rmse_by_horizon.csv` | Best version per component and horizon |
| `forecast_versions_component_yoy_compare.csv` | Full forecast vs realised yoy series |

All outputs are also saved as `.rds` in `outputs/` and `.parquet` in `data/` if the `arrow` package is available.
