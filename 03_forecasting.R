library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(boeCharts)

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

project_root <- get_project_root()
output_dir <- file.path(project_root, "outputs")

df_fcst_growth <- readRDS(file.path(output_dir, "fcst_growth.rds"))
df_fcst_level <- readRDS(file.path(output_dir, "fcst_level.rds"))
df_fcst <- readRDS(file.path(output_dir, "fcst_raw.rds"))
b_s <- readRDS(file.path(output_dir, "b_services.rds"))
b_cg <- readRDS(file.path(output_dir, "b_core_goods.rds"))
b_lr <- readRDS(file.path(output_dir, "b_food_lr.rds"))
b_ecm <- readRDS(file.path(output_dir, "b_food_ecm.rds"))
df_plus_nearcast <- readRDS(file.path(output_dir, "last_state.rds"))

## --------------------------------------------------
#  1. FORECASTING
## --------------------------------------------------

ECM_data <- df_plus_nearcast %>%
  dplyr::select(date, ln_pmdef_f, ln_food_at, ln_eer_f) %>%
  mutate(ln_pmdef = ln_pmdef_f, ln_eer = ln_eer_f)

fc_start <- as.yearqtr("2026 Q1")
fc_end <- as.yearqtr("2026 Q3")

idx_fc <- with(ECM_data,
               date >= fc_start & date <= fc_end)

yhat_fc <- with(ECM_data[idx_fc, ],
                b_lr["(Intercept)"] +
                  b_lr["ln_eer"] * ln_eer +
                  b_lr["ln_pmdef"] * ln_pmdef)

y_fc <- ECM_data$ln_food_at[idx_fc]
resid_fc <- y_fc - yhat_fc

df_plus_nearcast$ECT_food[idx_fc] <- resid_fc
df_plus_nearcast$ECT_food_L1 <- dplyr::lag(df_plus_nearcast$ECT_food, 1)

mpr_fcst <-
  df_fcst_growth %>%
  dplyr::select(date, infl_exp_f, eer_f_qoq, energy_f_qoq,
                pmdef_f_qoq, cpi_f_qoq, ln_pmdef_f, ln_eer_f) %>%
  arrange(date) %>%
  mutate(
    ie_L1 = dplyr::lag(infl_exp_f, 1),
    ie_L3 = dplyr::lag(infl_exp_f, 3),
    pmdef_L2 = dplyr::lag(pmdef_f_qoq, 2),
    e_L4 = dplyr::lag(energy_f_qoq, 4)
  ) %>%
  filter(
    date >= as.yearqtr("2026 Q4"), date <= as.yearqtr("2029 Q2")
  )

forecast_services <- function(b_s, df_plus_nearcast, mpr_fcst) {
  last_serv <- tail(df_plus_nearcast$services_qoq, 1)
  nT <- nrow(mpr_fcst)
  serv_fc <- numeric(nT)

  for (t in seq_len(nT)) {
    x_serv_lag <- last_serv
    x_infl_exp_f <- mpr_fcst$infl_exp_f[t]
    x_eer_f_qoq <- mpr_fcst$eer_f_qoq[t]
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

forecast_core_goods <- function(b_cg, df_plus_nearcast, mpr_fcst) {
  last_cg <- tail(df_plus_nearcast$core_gds_qoq, 1)
  nT <- nrow(mpr_fcst)
  cg_fc <- numeric(nT)

  for (t in seq_len(nT)) {
    x_cg_L1 <- last_cg
    x_eer_f_qoq <- mpr_fcst$eer_f_qoq[t]
    x_pmdef_L2 <- mpr_fcst$pmdef_L2[t]

    y_hat <-
      b_cg["lag(core_gds_qoq, 1)"] * x_cg_L1 +
      b_cg["eer_qoq"] * x_eer_f_qoq +
      b_cg["lag(pmdef_qoq, 2)"] * x_pmdef_L2

    cg_fc[t] <- y_hat
    last_cg <- y_hat
  }

  mpr_fcst$core_gds_qoq_fc <- cg_fc
  mpr_fcst
}

forecast_food <- function(b_lr, b_ecm, df_plus_nearcast, mpr_fcst) {
  last_food_level <- tail(df_plus_nearcast$food_at, 1)
  last_ECT <- tail(df_plus_nearcast$ECT_food, 1)
  food_lag_buffer <- tail(df_plus_nearcast$food_at_qoq, 4)

  nT <- nrow(mpr_fcst)
  food_qoq_fc <- numeric(nT)
  food_level_fc <- numeric(nT)

  for (t in seq_len(nT)) {
    x_f_L2 <- food_lag_buffer[3]
    x_f_L4 <- food_lag_buffer[1]
    x_e_L4 <- mpr_fcst$e_L4[t]
    x_ie_L1 <- mpr_fcst$ie_L1[t]
    x_pmdef_qoq <- mpr_fcst$pmdef_f_qoq[t]
    x_pmdef_L2 <- mpr_fcst$pmdef_L2[t]
    x_ECT_L1 <- last_ECT

    y_hat_qoq <-
      b_ecm["lag(food_at_qoq, 2)"] * x_f_L2 +
      b_ecm["lag(food_at_qoq, 4)"] * x_f_L4 +
      b_ecm["lag(energy_qoq, 4)"] * x_e_L4 +
      b_ecm["lag(infl_exp, 1)"] * x_ie_L1 +
      b_ecm["pmdef_qoq"] * x_pmdef_qoq +
      b_ecm["lag(pmdef_qoq, 2)"] * x_pmdef_L2 +
      b_ecm["ECT_food_L1"] * x_ECT_L1

    food_qoq_fc[t] <- y_hat_qoq

    new_food_level <- last_food_level * (1 + y_hat_qoq / 100)
    food_level_fc[t] <- new_food_level

    ln_pmdef_t <- mpr_fcst$ln_pmdef_f[t]
    ln_eer_t <- mpr_fcst$ln_eer_f[t]
    ln_food_t <- log(new_food_level)

    ECT_t <- ln_food_t - (
      b_lr["(Intercept)"] +
        b_lr["ln_pmdef"] * ln_pmdef_t +
        b_lr["ln_eer"] * ln_eer_t
    )

    last_food_level <- new_food_level
    last_ECT <- ECT_t
    food_lag_buffer <- c(food_lag_buffer[-1], y_hat_qoq)
  }

  mpr_fcst$food_at_qoq_fc <- food_qoq_fc
  mpr_fcst$food_at_level_fc <- food_level_fc

  mpr_fcst
}

mpr_fcst_full <- mpr_fcst %>%
  forecast_services(b_s, df_plus_nearcast, .) %>%
  forecast_core_goods(b_cg, df_plus_nearcast, .) %>%
  forecast_food(b_lr, b_ecm, df_plus_nearcast, .)

## --------------------------------------------------
#  2. RECONSTRUCT LEVELS, AGGREGATE, AND PLOT
## --------------------------------------------------

fcst_level <- df_plus_nearcast %>%
  filter(date >= "1996 Q2", date <= "2026 Q3") %>%
  dplyr::select(date, food_at, services, core_gds)

fcst_qoq <- mpr_fcst_full %>%
  dplyr::select(date,
                food_at_qoq_fc,
                services_qoq_fc,
                core_gds_qoq_fc)

fc_level <- bind_rows(fcst_level, fcst_qoq) %>%
  arrange(date)

for (i in seq_len(nrow(fc_level))) {
  if (is.na(fc_level$food_at[i])) {
    fc_level$food_at[i] <- fc_level$food_at[i - 1] * (1 + fc_level$food_at_qoq_fc[i] / 100)
  }

  if (is.na(fc_level$services[i])) {
    fc_level$services[i] <- fc_level$services[i - 1] * (1 + fc_level$services_qoq_fc[i] / 100)
  }

  if (is.na(fc_level$core_gds[i])) {
    fc_level$core_gds[i] <- fc_level$core_gds[i - 1] * (1 + fc_level$core_gds_qoq_fc[i] / 100)
  }
}

fc_level <- fc_level %>%
  left_join(
    df_fcst_level %>%
      dplyr::select(date, cpi_f, energy_f),
    by = "date"
  )

fc_yoy <- fc_level %>%
  mutate(
    food_at_yoy = 100 * (log(food_at) - log(dplyr::lag(food_at, 4))),
    services_yoy = 100 * (log(services) - log(dplyr::lag(services, 4))),
    core_gds_yoy = 100 * (log(core_gds) - log(dplyr::lag(core_gds, 4))),
    energy_yoy = 100 * (log(energy_f) - log(dplyr::lag(energy_f, 4))),
    MPR_cpi = 100 * (log(cpi_f) - log(dplyr::lag(cpi_f, 4)))
  )

fc_yoy <- fc_yoy %>%
  left_join(
    df_fcst %>%
      dplyr::select(date, s_wgt, f_wgt, e_wgt, cg_wgt, encont_f),
    by = "date"
  )

fc_yoy <- fc_yoy %>%
  mutate(
    c_f = f_wgt * food_at_yoy / 1000,
    c_s = s_wgt * services_yoy / 1000,
    c_e = e_wgt * energy_yoy / 1000,
    c_cg = cg_wgt * core_gds_yoy / 1000
  )

fc_yoy$cpi_bottom_up <- with(fc_yoy, c_f + encont_f + c_s + c_cg)
fc_yoy$cpi_resid <- with(fc_yoy, MPR_cpi - cpi_bottom_up)
fc_yoy$c_cg_r <- with(fc_yoy, MPR_cpi - c_f - encont_f - c_s)

boe_dark_blue <- "#12273F"
boe_aqua <- "#3CD7D9"
boe_stone <- "#C4C9CF"
boe_orange <- "#FF7300"
boe_purple <- "#9E71FE"
boe_gold <- "#D4AF37"

fc_yoy <- fc_yoy %>%
  filter(
    date >= as.yearqtr("2025 Q1"),
    date <= as.yearqtr("2029 Q2")
  )

plot_components <- fc_yoy %>%
  dplyr::select(date, MPR_cpi,
                services_yoy, core_gds_yoy, food_at_yoy) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  tidyr::drop_na(value) %>%
  mutate(series = factor(series,
                         levels = c("MPR_cpi", "services_yoy", "core_gds_yoy", "food_at_yoy"),
                         labels = c("MPR CPI (Baseline)", "Services", "Core goods", "Food"))) %>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_vline(xintercept = as.numeric(as.yearqtr("2026 Q1")),
             linetype = "dashed", colour = "grey40") +
  scale_colour_manual(
    values = c(
      "MPR CPI (Baseline)" = boe_orange,
      "Services" = boe_purple,
      "Core goods" = boe_gold,
      "Food" = boe_aqua
    )
  ) +
  labs(x = NULL, y = "Percentage change (yoy)", colour = NULL) +
  theme_minimal(base_family = "sans")

ggsave(file.path(project_root, "cpi_components_yoy.png"), plot_components, width = 10, height = 5, dpi = 300)

plot_aggregate <- fc_yoy %>%
  dplyr::select(date, MPR_cpi, cpi_bottom_up, cpi_resid) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  tidyr::drop_na(value) %>%
  mutate(series = factor(series,
                         levels = c("MPR_cpi", "cpi_bottom_up", "cpi_resid"),
                         labels = c("MPR CPI (Baseline)", "CPI-Bottom up", "CPI residual"))) %>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  labs(x = NULL, y = "Percentage change (yoy)", colour = NULL) +
  theme_minimal(base_family = "sans")

ggsave(file.path(project_root, "cpi_aggregate_vs_bottomup.png"), plot_aggregate, width = 10, height = 5, dpi = 300)

fc_yoy_fc <- fc_yoy %>%
  filter(
    date >= as.yearqtr("2025 Q1"),
    date <= as.yearqtr("2029 Q2")
  )

fc_long <- fc_yoy_fc %>%
  mutate(date_plot = as.Date(date)) %>%
  dplyr::select(
    date, date_plot,
    c_f, encont_f, c_s, c_cg_r, MPR_cpi
  ) %>%
  pivot_longer(
    cols = c(c_f, encont_f, c_s, c_cg_r),
    names_to = "component",
    values_to = "contribution"
  ) %>%
  tidyr::drop_na(contribution)

headline_line <- fc_yoy_fc %>%
  mutate(date_plot = as.Date(date)) %>%
  dplyr::select(date_plot, MPR_cpi) %>%
  tidyr::drop_na(MPR_cpi)

bank_cols <- c(
  c_f = boe_aqua,
  encont_f = boe_orange,
  c_s = boe_purple,
  c_cg_r = boe_gold
)

component_labs <- c(
  c_f = "Food",
  encont_f = "Energy",
  c_s = "Services",
  c_cg_r = "Core goods"
)

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
      axis.text = element_text(colour = boe_dark_blue),
      axis.ticks = element_line(colour = alpha(boe_dark_blue, 0.35)),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key = element_rect(fill = "white", colour = NA),
      plot.margin = margin(10, 12, 8, 10)
    )
}

p <- ggplot(fc_long, aes(x = date_plot)) +
  geom_col(
    aes(y = contribution, fill = component),
    width = 85,
    colour = NA,
    na.rm = TRUE
  ) +
  geom_line(
    data = headline_line,
    aes(x = date_plot, y = MPR_cpi),
    colour = boe_dark_blue,
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  geom_hline(yintercept = 0, colour = alpha(boe_dark_blue, 0.6), linewidth = 0.6) +
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
    title = "Contribution of CPI components to MPR CPI (Baseline)",
    subtitle = "2025Q1–2029Q2"
  ) +
  theme_boe(base_size = 12)

ggsave(file.path(project_root, "cpi_contributions_stacked.png"), p, width = 11, height = 6, dpi = 300)

if (interactive()) {
  print(p)
}

cat("Finished 03_forecasting.R\n")
cat("Generated files:\n")
cat(" - cpi_components_yoy.png\n")
cat(" - cpi_aggregate_vs_bottomup.png\n")
cat(" - cpi_contributions_stacked.png\n")