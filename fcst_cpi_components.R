### IMPORTING DATA from EXCEL ####
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)
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
  readxl::read_excel("C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data_set_v3.xlsx",
                     sheet = "Estimation_data",
                     range = "A11:L154")

cpi_tbl$date <- as.yearqtr(cpi_tbl$date)

cpi_tbl <-
  cpi_tbl %>%
  mutate(pmdef = pmdef*100)


#Data in levels (Indexes, rates) 1991-2025
df_cpi_level <- 
  cpi_tbl %>%
  dplyr::select(date, cpi, core_gds, services, food_at,energy, pmdef, eer, infl_exp)

#Data in log-levels

df_cpi_log <-
  df_cpi_level%>%
  mutate(across(c(cpi, core_gds, services, food_at,
                  energy, pmdef, eer),
                ~ log(.), # log is function to calculate natural logarithm
                .names =("ln_{.col}")))

# Data in quarterly percentage Change - qoq

df_cpi_qoq <-
  df_cpi_log %>%
  mutate(across(c(cpi, core_gds, services, food_at,
                  energy, pmdef, eer),  
                ~ Delt(., type = "log")*100, # percent change using log diff
                .names =("{.col}_qoq")))%>%
  slice(-1)

# Annual percentage change
df_cpi_yoy <-
  df_cpi_log %>%
  mutate(
    across(
      c(cpi, core_gds, services, food_at,
        energy, pmdef, eer), 
      ~ Delt(., k = 4, type = "log") * 100,  # YoY percent change
      .names = "{.col}_yoy"
    )
  )

# creating dummies

df_cpi_tbl <-
  df_cpi_qoq %>%
  mutate(D_1991Q3 = as.integer(date >= as.yearqtr("1991 Q3") & date <= as.yearqtr("1991 Q3") ))%>%
  mutate(D_2001Q2 = as.integer(date >= as.yearqtr("2001 Q2") & date <= as.yearqtr("2001 Q2") ))%>%
  mutate(D_2008Q2 = as.integer(date >= as.yearqtr("2008 Q2") & date <= as.yearqtr("2008 Q2") ))%>%
  mutate(D_2009Q2 = as.integer(date >= as.yearqtr("2009 Q2") & date <= as.yearqtr("2009 Q2") ))%>%
  mutate(D_2021Q2 = as.integer(date >= as.yearqtr("2021 Q2") & date <= as.yearqtr("2021 Q2") ))%>%
  mutate(D_2021Q2Q3 = as.integer(date >= as.yearqtr("2021 Q2") & date <= as.yearqtr("2021 Q3") ))%>%
  mutate(D_2023Q2 = as.integer(date >= as.yearqtr("2023 Q2") & date <= as.yearqtr("2023 Q2") ))%>%
  mutate(D_2023Q3 = as.integer(date >= as.yearqtr("2023 Q3") & date <= as.yearqtr("2023 Q3") ))

#Selecting a set of variables
full_data <-
  df_cpi_tbl %>%
  dplyr::select(date,cpi_qoq,services_qoq, 
                core_gds_qoq, food_at_qoq,
                infl_exp,pmdef_qoq, energy_qoq, eer_qoq,
                food_at, ln_food_at, ln_pmdef, ln_eer,
                D_1991Q3,D_2001Q2,D_2008Q2,
                D_2009Q2,D_2021Q2, D_2021Q2Q3,D_2023Q2,D_2023Q3)


#----- 1.2 Data for forecasting including CECD FORECAST and wider BANK FORECAST

# Importing wider forecast for inflation and activity 1996Q1-2029Q2
df_fcst <- 
  readxl::read_excel("C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data_set_v3.xlsx",
                     sheet = "M26_Baseline",
                     range = "A11:V145")

df_fcst$date <- as.yearqtr(df_fcst$date)

#Data in levels (Indexes, rates) 1996-2026
df_fcst_level <- 
  df_fcst %>%
  mutate(pmdef_f = pmdef_f*100) %>%
  dplyr::select(date, stif_cpi, core_gds, services, food_at,energy_f, pmdef_f, eer_f, infl_exp_f, cpi_f)

#Data in log-levels, qoq and yoy growth

df_fcst_growth <-
  df_fcst_level%>%
  mutate(across(c(food_at, pmdef_f, eer_f),
                ~ log(.), # log is function to calculate natural logarithm
                .names =("ln_{.col}"))) %>%
  
  mutate(across(c(stif_cpi, core_gds, services, food_at,
                  energy_f, pmdef_f, eer_f, cpi_f),  
                ~ Delt(., type = "log")*100, # percent change using log diff
                .names =("{.col}_qoq"))) %>%
  
  mutate(across(c(stif_cpi, core_gds, services, food_at,
                  energy_f, pmdef_f, eer_f, cpi_f), 
                ~ Delt(., k = 4, type = "log") * 100,  # YoY percent change
                .names = ("{.col}_yoy")))

## --------------------------------------------------
#  2. MODEL ESTIMATION 
## --------------------------------------------------

library(zoo)
library(lmtest)
library(sandwich)
library(ecm)
library(purrr)
library(car)
library(strucchange)

#------Estimation window 1991-2025Q2----------

full_sample<- subset(full_data, 
                     date >= as.yearqtr("1991 Q2") & 
                       date <= as.yearqtr("2025 Q4"))

#===== Services model and forecast =========


s_adl <- lm(
  services_qoq ~ lag(services_qoq, 1) + infl_exp +
    eer_qoq + pmdef_qoq +  D_1991Q3 + D_2021Q2 + D_2023Q2,
  data = full_sample)

summary(s_adl)

b_s <- coef(s_adl)


#===== Core goods =======

cg_adl <- lm(
  core_gds_qoq ~ 0 + lag(core_gds_qoq,1) +  
    eer_qoq + lag(pmdef_qoq,2) + 
    D_2009Q2 + D_2023Q3, 
  data = full_sample)

summary (cg_adl)

b_cg <- coef(cg_adl)

#===== Food, alcohol and tobacco =======

library(dynlm)
# 1. Model
# Long run equation:

coint_food_lm <- lm(ln_food_at ~ ln_eer + ln_pmdef, data = full_sample)

summary (coint_food_lm)

b_lr <- coef(coint_food_lm)

full_sample$ECT_food    <- resid(coint_food_lm)
full_sample$ECT_food_L1 <- dplyr::lag(full_sample$ECT_food, 1)

#saving resid in forecst data as well
ECT_df <- full_sample %>%
  select(date) %>%
  mutate(ECT_food    = resid(coint_food_lm),
         ECT_food_L1 = dplyr::lag(ECT_food, 1))

df_fcst_growth <- df_fcst_growth %>%
  left_join(ECT_df, by = "date")

# Short run equation:
ecm_food <- dynlm(
  food_at_qoq ~ 0 + lag(food_at_qoq, 2) + lag(food_at_qoq, 4) + lag(energy_qoq, 4) +
    lag(infl_exp, 1) + pmdef_qoq + lag(pmdef_qoq, 2) +
    ECT_food_L1 + D_2001Q2 + D_2008Q2,
  data = full_sample)

summary (ecm_food)

b_ecm <- coef(ecm_food)


## -----------------------------------------------------
#---- USING NEW DATA CONSISTENT WITH CECD FORECAST 
## -----------------------------------------------------

#---- CHOOSE THE # OF QUARTERS TO BE CONSIDERED FROM STIF IN EOD FORECAST

df_plus_nearcast <- subset(df_fcst_growth, 
                           date >= as.yearqtr("1996 Q2") & 
                         date <= as.yearqtr("2026 Q3")) # Regular practice: #2 quarters since actual data

#----Adding required ECT series for food forecast------

ECM_data <- df_plus_nearcast %>%
  select(date, ln_pmdef_f,ln_food_at, ln_eer_f) %>%
  mutate(ln_pmdef = ln_pmdef_f, ln_eer =ln_eer_f)

# forecast window:I am including since Q1 due to the model estimation only include data up to 2025Q4
fc_start <- as.yearqtr("2026 Q1") 
fc_end   <- as.yearqtr("2026 Q3")

idx_fc <- with(ECM_data,
               date >= fc_start & date <= fc_end)

# Long-run fitted values using ECM food model
yhat_fc <- predict(coint_food_lm,newdata = ECM_data[idx_fc,
                                           c("ln_pmdef", "ln_eer")])

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
  df_fcst_growth%>%
  dplyr::select(date,infl_exp_f,eer_f_qoq, energy_f_qoq,
                pmdef_f_qoq,cpi_f_qoq, ln_pmdef_f, ln_eer_f) %>%
  arrange(date) %>%
  
  #Including lagged data to match with CECD constrains period
  mutate(
    ie_L1    = dplyr::lag(infl_exp_f, 1),     # 1‑lag of inflation expectations
    ie_L3    = dplyr::lag(infl_exp_f, 3),     # 3‑lag of inflation expectations
    pmdef_L2 = dplyr::lag(pmdef_f_qoq, 2),   # 2‑lag of pmdef_qoq
    e_L4     = dplyr::lag(energy_f_qoq, 4),  # 4‑lag of energy_qoq
  ) %>%
  filter(
    date >= as.yearqtr("2026 Q4"), date <= as.yearqtr("2029 Q2")) # here you can change forecast horizon

#------Forecasts functions---------

# Services

forecast_services <- function(b_s, df_plus_nearcast, mpr_fcst) {
  # Last observed QoQ services inflation
  last_serv <- tail(df_plus_nearcast$services_qoq, 1)
  nT      <- nrow(mpr_fcst)
  serv_fc <- numeric(nT)
  
  for (t in seq_len(nT)) {
    x_serv_lag  <- last_serv
    x_infl_exp_f  <- mpr_fcst$infl_exp_f[t]
    x_eer_f_qoq   <- mpr_fcst$eer_f_qoq[t]
    x_pmdef_f_qoq <- mpr_fcst$pmdef_f_qoq[t]
    
    y_hat <- b_s["(Intercept)"] +
      b_s["lag(services_qoq, 1)"] *  x_serv_lag  +
      b_s["infl_exp"] * x_infl_exp_f  +
      b_s["eer_qoq"] * x_eer_f_qoq   +
      b_s["pmdef_qoq"] * x_pmdef_f_qoq
    
    serv_fc[t] <- y_hat
    last_serv  <- y_hat  # update lagged endogenous for next period
  }
  
  mpr_fcst$services_qoq_fc <- serv_fc
  mpr_fcst
}

## Core goods

forecast_core_goods <- function(b_cg, df_plus_nearcast, mpr_fcst) {
  last_cg <- tail(df_plus_nearcast$core_gds_qoq, 1)
  nT    <- nrow(mpr_fcst)
  cg_fc <- numeric(nT)
  
  for (t in seq_len(nT)) {
    x_cg_L1  <- last_cg
    x_eer_f_qoq    <- mpr_fcst$eer_f_qoq[t]
    x_pmdef_L2   <- mpr_fcst$pmdef_L2[t]
    
    y_hat <-
      b_cg["lag(core_gds_qoq, 1)"]      * x_cg_L1     +
      b_cg["eer_qoq"]                 * x_eer_f_qoq   +
      b_cg["lag(pmdef_qoq, 2)"]       * x_pmdef_L2  
    
    cg_fc[t] <- y_hat
    last_cg  <- y_hat
  }
  
  mpr_fcst$core_gds_qoq_fc <- cg_fc
  mpr_fcst
}

## Food

forecast_food <- function(b_lr, b_ecm, df_plus_nearcast, mpr_fcst) {
  # 1. Starting values at the end of the estimation sample (2025Q2)
  last_food_level <- tail(df_plus_nearcast$food_at, 1)
  last_ECT        <- tail(df_plus_nearcast$ECT_food, 1)
  
  # Buffer with last 4 observed lags of food QoQ inflation
  # Order: [t-4, t-3, t-2, t-1]
  food_lag_buffer <- tail(df_plus_nearcast$food_at_qoq, 4)
  
  nT            <- nrow(mpr_fcst)
  food_qoq_fc   <- numeric(nT)
  food_level_fc <- numeric(nT)
  
  for (t in seq_len(nT)) {
    ## --- 2. Construct regressors for this period ---
    
    # Endogenous lags
    x_f_L2 <- food_lag_buffer[3]  
    x_f_L4 <- food_lag_buffer[1]  
    
    # Exogenous lags (already precomputed in mpr_fcst)
    x_e_L4      <- mpr_fcst$e_L4[t]
    x_ie_L1     <- mpr_fcst$ie_L1[t]
    x_pmdef_qoq <- mpr_fcst$pmdef_f_qoq[t]
    x_pmdef_L2  <- mpr_fcst$pmdef_L2[t]
    
    # ECT lag (from previous period)
    x_ECT_L1 <- last_ECT
    
    ## --- 3. Short‑run ECM forecast for QoQ food inflation ---
    y_hat_qoq <-
      b_ecm["lag(food_at_qoq, 2)"]    * x_f_L2      +
      b_ecm["lag(food_at_qoq, 4)"]    * x_f_L4      +
      b_ecm["lag(energy_qoq, 4)"]   * x_e_L4      +
      b_ecm["lag(infl_exp, 1)"]       * x_ie_L1     +
      b_ecm["pmdef_qoq"]            * x_pmdef_qoq +
      b_ecm["lag(pmdef_qoq, 2)"]    * x_pmdef_L2  +
      b_ecm["ECT_food_L1"] * x_ECT_L1
    
    food_qoq_fc[t] <- y_hat_qoq
    
    ## --- 4. Update level of food prices ---
    # ASSUMPTION (explicit): food_at_qoq is in percent (e.g. 1.5 = 1.5%)
    new_food_level   <- last_food_level * (1 + y_hat_qoq/100)
    food_level_fc[t] <- new_food_level
    
    ## --- 5. Update ECT_t using long-run relationship and N25 ln_pmdef, ln_eer ---
    ln_pmdef_t <- mpr_fcst$ln_pmdef_f[t]
    ln_eer_t   <- mpr_fcst$ln_eer_f[t]
    ln_food_t  <- log(new_food_level)
    
    ECT_t <- ln_food_t - (
      b_lr["(Intercept)"] +
        b_lr["ln_pmdef"] * ln_pmdef_t +
        b_lr["ln_eer"]   * ln_eer_t
    )
    
    ## --- 6. Roll forward state variables for next iteration ---
    last_food_level <- new_food_level
    last_ECT        <- ECT_t
    
    # Drop oldest lag, append current forecast to the buffer
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
#FORECAST IN TERMS OF YEAR ON YEAR
#-------------------------------------
# 1. Historical data + CECD forecast:  1991Q2–2026Q3

fcst_level <- df_plus_nearcast %>%
  filter(date >= "1996 Q2", date <= "2026 Q3") %>%
  select(date, food_at, services, core_gds)


# --- 2. Select forecast QoQ inflation ---
fcst_qoq <- mpr_fcst_full %>%
  select(date,
         food_at_qoq_fc,
         services_qoq_fc,
         core_gds_qoq_fc)

# --- 3. Bind and arrange chronologically ---
fc_level <- bind_rows(fcst_level, fcst_qoq) %>%
  arrange(date)

# --- 4. Rebuild levels iteratively (correct recursive approach) ---
for (i in seq_len(nrow(fc_level))) {
  
  if (is.na(fc_level$food_at[i])) {
    fc_level$food_at[i] <- fc_level$food_at[i-1] * (1 + fc_level$food_at_qoq_fc[i]/100)
  }
  
  if (is.na(fc_level$services[i])) {
    fc_level$services[i] <- fc_level$services[i-1] * (1 + fc_level$services_qoq_fc[i]/100)
  }
  
  if (is.na(fc_level$core_gds[i])) {
    fc_level$core_gds[i] <- fc_level$core_gds[i-1] * (1 + fc_level$core_gds_qoq_fc[i]/100)
  }
}

#Adding the rest of CPI components

fc_level <- fc_level %>%
  left_join(
    df_fcst_level %>% 
      select(date, cpi_f, energy_f),
    by = "date")

# --- 5. Compute YoY inflation (log-diff) ---
fc_yoy <- fc_level %>%
  mutate(
    food_at_yoy  = 100 * (log(food_at)  - log(dplyr::lag(food_at, 4))),
    services_yoy = 100 * (log(services) - log(dplyr::lag(services, 4))),
    core_gds_yoy = 100 * (log(core_gds) - log(dplyr::lag(core_gds, 4))),
    energy_yoy = 100 * (log(energy_f) - log(dplyr::lag(energy_f, 4))),
    MPR_cpi = 100 * (log(cpi_f) - log(dplyr::lag(cpi_f, 4))),
  )

#------------------------------------------------
# AGGREGATED CPI using component annual contribution  
#------------------------------------------------
#Adding wights into dataset

fc_yoy <- fc_yoy %>%
  left_join(
    df_fcst %>% 
      select(date, s_wgt, f_wgt, e_wgt, cg_wgt, encont_f),
    by = "date"
  )

#Contribution by componentes

fc_yoy <- fc_yoy %>%
  mutate(
    c_f  = f_wgt * food_at_yoy    / 1000,
    c_s  = s_wgt   * services_yoy    / 1000,
    c_e  = e_wgt   * energy_yoy      / 1000,
    c_cg = cg_wgt  * core_gds_yoy    / 1000
  )


#--- Using CECD energy contribution to calculate bottom-up CPI

#Aggregated CPI

fc_yoy$cpi_bottom_up <- with(fc_yoy, c_f + encont_f + c_s + c_cg)

# Residual vs COMPASS headline CPI (difference)

fc_yoy$cpi_resid <- with(fc_yoy, MPR_cpi - cpi_bottom_up)


# ------ CORE GOODS contribution as identity-------------

#Adding residuals into core goods contribution

fc_yoy$c_cg_r <- with(fc_yoy, MPR_cpi - c_f - encont_f - c_s)  #c_cg_r include residual between MPR and bottom-up CPI

#------------------------------------------------
# PLOTTING THE FORECAST OF CPI COMPONENTS (YOY)
#------------------------------------------------
library(boeCharts)

# Main brand colours (examples)
boe_dark_blue <- "#12273F"  # BoE Dark Blue
boe_aqua      <- "#3CD7D9"  # BoE Aqua
boe_stone     <- "#C4C9CF"  # BoE Stone

# Secondary accents
boe_orange <- "#FF7300"     # BoE Orange
boe_purple <- "#9E71FE"     # BoE Purple
boe_gold   <- "#D4AF37"     # BoE GolD

# period of chart

fc_yoy <- fc_yoy %>%
  filter(
    date >= as.yearqtr("2025 Q1"),
    date <= as.yearqtr("2029 Q2")
  )

#------------------------------------------------
# PLOTTING FORECAST OF CPI COMPONENTS
#------------------------------------------------

# Reshape to long format for the three components
plot_components <- fc_yoy %>%
  select( date, MPR_cpi,
          services_yoy, core_gds_yoy, food_at_yoy)%>% #cpi_bottom_up,
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(series = factor(series,
                         levels = c("MPR_cpi","services_yoy", "core_gds_yoy", "food_at_yoy"), 
                         labels = c("MPR CPI (Baseline)","Services", "Core goods", "Food")))%>% #"cpi_bottom_up", "CPI-Bottom up",
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = as.numeric(as.yearqtr("2026 Q1")),
             linetype = "dashed", colour = "grey40")+
  
  scale_colour_manual(
    values = c(
      "MPR CPI (Baseline)" = boe_orange,
      "Services"        = boe_purple,
      "Core goods"      = boe_gold,
      "Food"            = boe_aqua
    ) #"CPI-Bottom up"   = boe_stone,
  ) +
  
  labs(x = NULL, y = "Percentage change (yoy)", colour = NULL) +
  theme_minimal(base_family = "sans")

ggsave("cpi_components_yoy.png", plot_components, width = 10, height = 5, dpi = 300)

#------------------------------------------------
# PLOTTING AGGREGATED CPI AND COMPASS CPI
#------------------------------------------------

#Plotting the comparison of headline CPI and estimated CPI

plot_aggregate <- fc_yoy %>%
  select(date, MPR_cpi,cpi_bottom_up,cpi_resid)%>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(series = factor(series,
                         levels = c("MPR_cpi","cpi_bottom_up","cpi_resid"),
                         labels = c("MPR CPI (Baseline)","CPI-Bottom up","CPI residual")))%>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.8) +
  labs(x = NULL, y = "Percentage change (yoy)", colour = NULL) +
  theme_minimal(base_family = "sans")

ggsave("cpi_aggregate_vs_bottomup.png", plot_aggregate, width = 10, height = 5, dpi = 300)

#------------------------------------------------
# PLOTTING FORECAST OF CPI COMPONENTS by CONTRIBUTION
#------------------------------------------------

#--------------------------------------------------
# 1. Filter the forecast horizon: 2024Q1–2028Q4
#--------------------------------------------------
fc_yoy_fc <- fc_yoy %>%
  filter(
    date >= as.yearqtr("2025 Q1"),
    date <= as.yearqtr("2029 Q2")
  )

#--------------------------------------------------
# 2. Prepare data in long format for contributions
#--------------------------------------------------
fc_long <- fc_yoy_fc %>%
  mutate(date_plot = as.Date(date)) %>%
  select(
    date, date_plot,
    c_f, encont_f, c_s, c_cg_r, MPR_cpi
  ) %>%  
  pivot_longer(
    cols = c(c_f, encont_f, c_s, c_cg_r),
    names_to  = "component",
    values_to = "contribution"
  )

#--------------------------------------------------
# 3. BoE brand colours (from internal design palette)
#--------------------------------------------------
# Map your components to a BoE-like palette.
bank_cols <- c(
  c_f       = boe_aqua,
  encont_f  = boe_orange,
  c_s       = boe_purple,
  c_cg_r    = boe_gold#,
  #cpi_resid = boe_stone
)

# Optional: nicer legend labels (keeps your variable names intact)
component_labs <- c(
  c_f       = "Food",
  encont_f  = "Energy",
  c_s       = "Services",
  c_cg_r    = "Core goods"#,
  #cpi_resid = "Residual"
)

#--------------------------------------------------
# 4. BoE-style theme (standalone approximation)
#   - White background
#   - Subtle major grid only
#   - No panel border
#   - Bottom legend
#   - Bank blue text
#--------------------------------------------------
theme_boe <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      # Typography / colour
      text = element_text(colour = boe_dark_blue),
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(size = base_size),
      plot.caption = element_text(size = base_size - 3, hjust = 0),
      
      # Gridlines: only major, light stone/grey feel
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = alpha(boe_stone, 0.55), linewidth = 0.4),
      
      # Axes
      axis.title = element_text(colour = boe_dark_blue),
      axis.text  = element_text(colour = boe_dark_blue),
      axis.ticks = element_line(colour = alpha(boe_dark_blue, 0.35)),
      
      # Legend: bottom, compact
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key = element_rect(fill = "white", colour = NA),
      
      # Spacing
      plot.margin = margin(10, 12, 8, 10)
    )
}

#--------------------------------------------------
# 5. Plot: stacked contributions + headline CPI line
#--------------------------------------------------
p <- ggplot(fc_long, aes(x = date_plot)) +
  
  # Stacked bars: component contributions
  geom_col(
    aes(y = contribution, fill = component),
    width = 85,                  # ~quarter width in days; looks cleaner on date axes
    colour = NA
  ) +
  
  # Headline CPI YoY line (unique per quarter)
  geom_line(
    data = fc_yoy_fc %>%
      mutate(date_plot = as.Date(date)) %>%
      select(date_plot, MPR_cpi),
    aes(x = date_plot, y = MPR_cpi),
    colour = boe_dark_blue,
    linewidth = 0.9
  ) +
  # Zero line (slightly lighter than pure black)
  geom_hline(yintercept = 0, colour = alpha(boe_dark_blue, 0.6), linewidth = 0.6) +
  
  # Manual fill colours + labels
  scale_fill_manual(values = bank_cols, labels = component_labs) +
  
  # X-axis formatting: annual ticks
  scale_x_date(
    name        = NULL,
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  
  # Y axis: a little breathing room; optional formatting
  scale_y_continuous(
    name = "Contribution to CPI (p.p.)",
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  
  labs(
    title    = "Contribution of CPI components to MPR CPI (Baseline)",
    subtitle = "2025Q1–2029Q2"
    #caption  = "Source: Bank calculations."
  ) +
  
  theme_boe(base_size = 12)

ggsave("cpi_contributions_stacked.png", p, width = 11, height = 6, dpi = 300)

if (interactive()) {
  print(p)
}

cat("Finished fcst_cpi_components.R\n")
cat("Generated files:\n")
cat(" - cpi_components_yoy.png\n")
cat(" - cpi_aggregate_vs_bottomup.png\n")
cat(" - cpi_contributions_stacked.png\n")


