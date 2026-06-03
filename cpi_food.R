### DATA and DATA PREPARATION ####
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)
library(quantmod)
library(ggplot2)

# Importing raw data 1990-2028
cpi_tbl <- 
  readxl::read_excel("cpi_data.xlsx")
tibble(cpi_tbl)

cpi_tbl$Date <- as.Date(cpi_tbl$Date, format = "%Y-%m-%d")
str(cpi_tbl)

cpi_tbl <-
  cpi_tbl %>%
  rename(capu_dat = capu_f) %>%
  mutate(u_rate = unemp*100, u_rate_f = unemp_f*100, pmdef = pmdef*100, pmdefxf = pmdefxf*100)

#Data in levels (Indexes, rates) 1991-2019
df_cpi_level <- 
  cpi_tbl %>%
  dplyr::select(Date, cpi, core_gds, energy, services, food_at, ois_rate, bank_rate,
                infl_exp, ied, wage, pmdef, pmdefxf, eer, cpif) %>%
  slice_min(n = 141, order_by = Date)# Selecting back data period

str(df_cpi_level)

#Data in log-levels

df_cpi_log <-
  df_cpi_level%>%
  mutate(across(c(cpi, core_gds, 
                  energy, services, food_at, wage,
                  pmdef,pmdefxf, eer, cpif),
                ~ log(.), # log is function to calculate natural logarithm
                .names =("ln_{.col}")))

# Transforming data to one period percentage Change

df_cpi_percent <-
  df_cpi_log %>%
  mutate(across(c(cpi, core_gds, 
                  energy, services, food_at, wage, 
                  pmdef,pmdefxf, eer, cpif),
                ~ Delt(., type = "log")*100, # percent change using log diff
                .names =("{.col}_qoq")))%>%
  slice(-1)

# Creating lags
df_cpi_percent <- 
  df_cpi_percent %>%
  mutate(f_L1 = lag(food_at_qoq, 1),
         f_L2 = lag(food_at_qoq, 2),
         f_L3 = lag(food_at_qoq, 3),
         f_L4 = lag(food_at_qoq, 4),
         cg_L1 = lag(core_gds_qoq, 1),
         cg_L2 = lag(core_gds_qoq, 1),
         cg_L3 = lag(core_gds_qoq, 3),
         cg_L4 = lag(core_gds_qoq, 4),
         s_L1 = lag(services_qoq, 1),
         s_L2 = lag(services_qoq, 2),
         e_L1 = lag(energy_qoq, 1),
         e_L2 = lag(energy_qoq, 2),
         e_L3 = lag(energy_qoq, 3),
         e_L4 = lag(energy_qoq, 4),
         ied_L1 = lag(ied, 1),
         ied_L2 = lag(ied, 2),
         ied_L3 = lag(ied, 3),
         ied_L4 = lag(ied, 4),
         ie_L1 = lag(infl_exp, 1),
         ie_L2 = lag(infl_exp, 2),
         ie_L3 = lag(infl_exp, 3),
         ie_L4 = lag(infl_exp, 4),
         or_L1 = lag(ois_rate, 1),
         or_L2 = lag(ois_rate, 2),
         eer_L1 = lag(eer_qoq, 1),
         eer_L2 = lag(eer_qoq, 2),
         eer_L3 = lag(eer_qoq, 3),
         eer_L4 = lag(eer_qoq, 4),
         wage_L1 = lag(wage_qoq,1),
         pmdef_L1 = lag(pmdef_qoq, 1),
         pmdef_L2 = lag(pmdef_qoq, 2),
         pmdef_L3 = lag(pmdef_qoq, 3),
         pmdef_L4 = lag(pmdef_qoq, 4),
         pmdefxf_L1 = lag(pmdefxf_qoq, 1),
         pmdefxf_L2 = lag(pmdefxf_qoq, 2),
         pmdefxf_L3 = lag(pmdefxf_qoq, 3),
         pmdefxf_L4 = lag(pmdefxf_qoq, 4))%>%
  na.omit(df_cpi_percent)

# creating dummies

#Changing date format before creating dummies

df_cpi_tbl <- 
  df_cpi_percent %>% 
  as_tibble() %>%
  tibble(date = seq.Date(from = as.Date("1991-09-30"),
                         to   = as.Date("2025-06-30"),
                         by   = "3 months"))

df_cpi_tbl <-
  df_cpi_tbl %>%
  mutate(D_SHOCK_1991 = ifelse(date >= as.Date("1991-09-30") & date <= as.Date("1991-09-30"), 1, 0)) %>%
  mutate(D_SHOCK_1992 = ifelse(date >= as.Date("1992-09-30") & date <= as.Date("1992-09-30"), 1, 0)) %>%
  mutate(D_2001Q2 = ifelse(date >= as.Date("2001-06-30") & date <= as.Date("2001-06-30"), 1, 0)) %>%
  mutate(D_GFC_2008 = ifelse(date >= as.Date("2008-06-30") & date <= as.Date("2008-06-30"), 1, 0)) %>%
  mutate(D_COVID_2020 = ifelse(date >= as.Date("2020-06-30") & date <= as.Date("2020-06-30"), 1, 0)) %>%
  mutate(D_COVID_2021 = ifelse(date >= as.Date("2021-12-30") & date <= as.Date("2021-12-30"), 1, 0)) %>%
  mutate(D_COVID_2022 = ifelse(date >= as.Date("2022-12-30") & date <= as.Date("2022-12-30"), 1, 0)) %>%
  mutate(D_ENERGY_2023 = ifelse(date >= as.Date("2023-06-30") & date <= as.Date("2023-06-30"), 1, 0))


#=============================================
#===== MODEL ESTIMATION 1991-2025 =====
#=============================================
library(zoo)
library(lmtest)
library(sandwich)
library(ecm)
library(purrr)

library(car)

library(strucchange)

#Selecting a set of variables
full_data <-
  df_cpi_tbl %>%
  dplyr::select(date,food_at_qoq, f_L1, f_L2, f_L3,f_L4,core_gds_qoq, cg_L1,cg_L2,cg_L3,cg_L4,
                infl_exp, ie_L1,ie_L2,ie_L3,ie_L4,ied, ied_L1,ied_L2,ied_L3,ied_L4,
                energy_qoq, e_L1,e_L2,e_L3,e_L4,
                eer_qoq, eer_L1, eer_L2, eer_L3,eer_L4,
                pmdef_qoq, pmdef_L1 , pmdef_L2 , pmdef_L3 , pmdef_L4,
                pmdefxf_qoq, pmdefxf_L1 , pmdefxf_L2 , pmdefxf_L3 , pmdefxf_L4, 
                ois_rate, or_L1, or_L2, bank_rate, wage_qoq, wage_L1,
                D_SHOCK_1991, D_2001Q2, D_GFC_2008,
                D_SHOCK_1992, D_COVID_2020,D_COVID_2021,D_COVID_2022,D_ENERGY_2023,
                ln_food_at,ln_pmdef,ln_pmdefxf, ln_eer)

#Ensuring date format
full_data$date <- as.yearqtr(full_data$date)

full_data <- as.data.frame(full_data)

# Defining train/test ranges (create if not already defined)
train <- subset(full_data, 
                date >= as.yearqtr("1991 Q3") & 
                date <= as.yearqtr("2025 Q2"))
#test  <- subset(full_data, date >= as.yearqtr("2020 Q1") & date <= as.yearqtr("2025 Q2"))

'f_adl <- step(lm(food_at_qoq ~ 0 + f_L1+ f_L2+ f_L3 + f_L4 +
                      energy_qoq + e_L1 +e_L2 +e_L3 +e_L4 +
                      infl_exp+ ie_L1 +ie_L2 + ie_L3+ ie_L4 +
                      eer_qoq+ eer_L1 + eer_L2 + eer_L3 + eer_L4 +
                      pmdef_qoq + pmdef_L1 + pmdef_L2 + pmdef_L3 + pmdef_L4,
                      #D_2001Q2+D_COVID_2022+ D_ENERGY_2023,
                    data=train),
                 direction = "backward", k = log(nrow(train)))  # ~ BIC' 

#==== Food OLS_DL model ===========

f_adl <- lm(food_at_qoq ~ 0 + f_L2 + f_L4 + e_L4 +
                 ie_L1 + ie_L3 + pmdef_qoq + pmdef_L2+D_2001Q2+ D_GFC_2008,
                 #D_ENERGY_2023,
               data=train) #'

summary (f_adl)

# Coefficient test

lmtest::coeftest(f_adl, vcov = NeweyWest(f_adl, prewhite = FALSE))

#=== OUTLIER TEST TO SET DUMMIES ===#

#Approach_1
# Compute standardised residuals
t_resid_f <- rstandard(f_adl)
# Flag outliers (e.g., |residual| > 2.5)
outliers_f <- which(abs(t_resid_f) > 2.5)
train[outliers_f,"food_at_qoq"]
train[outliers_f,"date"]

# Approach_2
cooks_f <- cooks.distance(f_adl)
# Flag influential observations (rule of thumb: > 4/n)
threshold_f <- 4 / nrow(train)
which(cooks_f > threshold_f)

# Approach_3
outlierTest(f_adl)  # Bonferroni p-values for outliers

#=== STRUCTURAL CHANGE AND ESTABILITY TEST ===#

# Identification of breakpoints
f_bp <- breakpoints(food_at_qoq ~ 0 + f_L2 + f_L4 + e_L4 +
                      ie_L1 + ie_L3 + pmdef_qoq + pmdef_L2+
                      D_2001Q2+ D_GFC_2008,
                    data=train)
summary(f_bp)
plot(f_bp)

breakpoints(food_at_qoq ~ 0 + f_L2 + f_L4 + e_L4 +
              ie_L1 + ie_L3 + pmdef_qoq + pmdef_L2+
              D_2001Q2+ D_GFC_2008,
            data=train, breaks=5)

# stability test
sctest(food_at_qoq ~ 0 + f_L2 + f_L4 + e_L4 +
         ie_L1 + ie_L3 + pmdef_qoq + pmdef_L2+
         D_2001Q2+ D_GFC_2008,
       data=train, type="supF")

'#=== PLOTTING RESIDUALS

#plot 1
resid_f_adl <- resid(f_adl)
lag.plot(resid_f_adl, diag.col = "forest green"
         , main = "Lag Scatter Plot - FOOD OLS_ADL MODEL")

#plot 2
res <- as.numeric(resid_f_adl)
m <- mean(res, na.rm = TRUE)
s <- sd(res, na.rm = TRUE)

hist(res,
     breaks = "FD", freq = FALSE,
     col = "gray90", border = "white",
     main = "Residuals – FOOD OLS_ADL: Histogram + Normal Curve",
     xlab = "Residuals")

# Kernel density estimate (data-driven)
lines(density(res, na.rm = TRUE), col = "steelblue", lwd = 2, lty = 2)

# Normal curve with sample mean/sd
curve(dnorm(x, mean = m, sd = s), add = TRUE, col = "red", lwd = 2)

legend("topright",
       legend = c("Kernel density", "Normal curve"),
       col = c("steelblue", "red"), lwd = 2, lty = c(2,1), bty = "n")'

#----------------------------------------------------------
# 3. FORECAST 12 QUARTERS HORIZON UTILITIES
#----------------------------------------------------------
# keeping fixed the coefficients 
beta25       <- coef(f_adl) 
coef_names <- names(beta25)

# Build x-vector in correct order
build_x <- function(t, y_dyn, data, coef_names) {
  x <- setNames(numeric(length(coef_names)), coef_names)
  
  #if ("(Intercept)" %in% coef_names) x["(Intercept)"] <- 1
  
  # Dynamic food lags
  if ("f_L2" %in% coef_names)
    x["f_L2"] <- ifelse(t - 2 >= 1, y_dyn[t - 2], NA)
  
  if ("f_L4" %in% coef_names)
    x["f_L4"] <- ifelse(t - 4 >= 1, y_dyn[t - 4], NA)
  
  # Exogenous (always actual)
  exog_vars <- setdiff(coef_names, c("f_L2", "f_L4"))
  for (v in exog_vars) x[v] <- data[[v]][t]
  
  # Replace NA in regressors with 0 (should not normally arise)
  x[is.na(x)] <- 0
  
  return(x)
}

#----------------------------------------------------------
# 4. SINGLE-ORIGIN DYNAMIC FORECAST (12Q)
#----------------------------------------------------------
forecast_origin <- function(origin_q, H = 12, data, beta25, coef_names) {
  
  dates <- data$date
  y_act <- data$food_at_qoq
  
  idx0 <- which(dates == origin_q)
  if (length(idx0) == 0) return(tibble())
  
  # Start dynamic path: actuals up to origin, forecast beyond
  y_dyn <- y_act
  if (idx0 < length(y_dyn))
    y_dyn[(idx0 + 1):length(y_dyn)] <- NA
  
  out <- vector("list", H)
  
  for (h in 1:H) {
    t <- idx0 + h
    if (t > nrow(data)) break
    
    # Build regressor vector
    x <- build_x(t, y_dyn, data, coef_names)
    
    # Forecast
    y_hat <- sum(beta25 * x)
    
    # Insert into dynamic path
    y_dyn[t] <- y_hat
    
    out[[h]] <- tibble(
      origin_date = as.character(origin_q),
      horizon    = h,
      target_date = as.character(dates[t]),
      y_hat      = y_hat,
      y_actual   = y_act[t],
      error      = ifelse(!is.na(y_act[t]), y_act[t] - y_hat, NA)
    )
  }
  
  bind_rows(out)
} 

#----------------------------------------------------------
# 5. RUN FORECASTS FOR ORIGINS 2019Q4–2025Q2
#----------------------------------------------------------
origins <- seq(as.yearqtr("2019 Q4"), as.yearqtr("2025 Q2"), by = 0.25)

fcst_olsf25 <- map_dfr(
  origins,
  ~ forecast_origin(.x, H = 12, data = full_data,
                    beta25 = beta25, coef_names = coef_names)
)

# Convert date columns back to yearqtr
fcst_olsf25 <- fcst_olsf25 %>%
  mutate(
    origin_date = as.yearqtr(origin_date),
    target_date = as.yearqtr(target_date)
  ) %>%
  arrange(origin_date, horizon)

# Show first rows
print(head(fcst_olsf25, 20))

#accuracy metrics
ac_olsf25 <- fcst_olsf25 %>%
  filter(!is.na(y_actual)) %>%
  group_by(horizon) %>%
  summarise(
    n       = n(),
    RMSE    = sqrt(mean((y_actual - y_hat)^2, na.rm = TRUE)),
    MAE     = mean(abs(y_actual - y_hat), na.rm = TRUE),
    Bias    = mean(y_hat - y_actual, na.rm = TRUE)
  ) %>%
  arrange(horizon)

print(ac_olsf25, n = Inf)


#======================================================
# ECM- MODEL
#======================================================

library(dynlm)
library(tidyr)
library(stringr)

#----------------------------
# 1) Long-run (cointegration)
#----------------------------
# Estimation on 'train'; forecasting will use 'full_data'.
coint_food_lm <- lm(ln_food_at ~ ln_pmdef + ln_eer, data = train)

summary(coint_food_lm)

# ECT in train (ok to use residuals because model fitted on train)
train <- train %>%
  mutate(
    ECT_food     = resid(coint_food_lm),
    ECT_food_L1  = dplyr::lag(ECT_food, 1)
  )

# ECT in full_data: use LR prediction (do NOT use resid() here)
full_data <- full_data %>%
  mutate(
    date         = as.yearqtr(date),                              # ensure quarterly index
    ln_food_hat  = as.numeric(predict(coint_food_lm, newdata = full_data)),
    ECT_food     = ln_food_at - ln_food_hat,
    ECT_food_L1  = dplyr::lag(ECT_food, 1)
  )

#----------------------------
# 2) Short-run ECM estimation
#----------------------------
# NOTE: 'train' and 'full_data' already contain eer_L1 and pmdef_L2 — do NOT recreate.
# The other regressors (ied, ECT_food_L1, D_GFC_2008, D_ENERGY_2023) are assumed present.

ecm_food <- dynlm(food_at_qoq ~ 0 + f_L2 + f_L4 + e_L4 +
                    ie_L1 + ie_L3 + pmdef_qoq + pmdef_L2+
                    ECT_food_L1 + D_2001Q2+ D_GFC_2008,   # convergence to long-run
    data = train
)

summary(ecm_food)

#======================================================
# Rolling multi-step ECM forecasts with fixed coefficients
# Origins: 2019Q4 to 2025Q2
# Horizon: 12 quarters at each origin
#======================================================

#--------------------------------------------------------
# 1) Helper: Extract fixed coefficients and model terms
#--------------------------------------------------------
ecm_coef <- coef(ecm_food)               # named numeric vector
ecm_terms <- names(ecm_coef)             # includes "(Intercept)" if present

# Identify names we will fill from 'state' (endogenous lags) vs exogenous
y_name         <- "food_at_qoq"          # dependent (short-run)
y_lag2_name    <- "f_L2"                 # assumed: lag2 of y
y_lag4_name    <- "f_L4"                 # assumed: lag4 of y
ect_lag1_name  <- "ECT_food_L1"          # lagged error-correction term

# Other regressors (exogenous) are taken as-is from 'full_data'
# We'll pull them by name intersection with the coefficient vector
exo_names <- setdiff(ecm_terms, c(y_lag2_name, y_lag4_name, ect_lag1_name))

#--------------------------------------------------------
# 2) Long-run prediction function (for arbitrary dates)
#    Uses only exogenous LR vars from 'full_data'
#--------------------------------------------------------
predict_lr <- function(newdata_row) {
  # newdata_row is a 1-row data.frame with columns used by coint_food_lm
  as.numeric(predict(coint_food_lm, newdata = newdata_row))
}

#--------------------------------------------------------
# 3) Transformation between growth and log-level
#    ASSUMPTION: food_at_qoq = 100 * (ln_food_t - ln_food_{t-1})
#--------------------------------------------------------
growth_scale <- 100  # change to 1 if your growth is in log units (not percent)

update_ln_from_growth <- function(ln_prev, growth_qoq) {
  ln_prev + (growth_qoq / growth_scale)
}

#--------------------------------------------------------
# 4) Forecaster for a single origin and horizon (dynamic)
#    - Keeps coefficients fixed (ecm_coef)
#    - Builds regressors for each horizon step
#    - Updates endogenous lags and ECT recursively
#--------------------------------------------------------
forecast_ecm_one_origin <- function(origin_date, h = 12,
                                    full_df = full_data,
                                    coef_vec = ecm_coef,
                                    term_names = ecm_terms) {
  # Index of the origin in full_data
  if (!origin_date %in% full_df$date) {
    stop("Origin date not found in full_data: ", as.character(origin_date))
  }
  origin_idx <- which(full_df$date == origin_date)
  
  # Maximum horizon limited by data availability for exogenous regressors
  max_h_avail <- nrow(full_df) - origin_idx
  if (max_h_avail <= 0) {
    return(tibble())  # nothing to forecast
  }
  h_use <- min(h, max_h_avail)
  
  # Initialize "state" with last known actuals at the origin
  # Last actual level
  ln_food_last <- full_df$ln_food_at[origin_idx]
  if (is.na(ln_food_last)) stop("ln_food_at is NA at origin; cannot initialize level.")
  
  # y history to compute endogenous lags f_L2, f_L4
  # We need at least 4 quarters before origin to compute up to L4 for the first step
  if (origin_idx < 5) stop("Not enough history before origin to compute y lags.")
  
  # Build initial buffer of past y (actuals up to origin)
  y_hist <- full_df[[y_name]][1:origin_idx]  # vector up to origin
  if (any(is.na(tail(y_hist, 4)))) {
    stop("NA in recent y history before origin; cannot initialize endogenous lags.")
  }
  
  # Compute ECT at origin (lagged ECT to be used for the first forecast step)
  # ECT_t = ln_food_t - ln_food_hat_t; for step t+1 we use ECT_t as 'ECT_food_L1'
  lr_hat_origin <- predict_lr(full_df[origin_idx, , drop = FALSE])
  ect_last <- ln_food_last - lr_hat_origin
  if (is.na(ect_last)) stop("Cannot compute ECT at origin (check LR inputs).")
  
  # Container for forecast results
  out <- vector("list", length = h_use)
  
  # Iterate horizons
  for (s in seq_len(h_use)) {
    target_idx  <- origin_idx + s
    target_date <- full_df$date[target_idx]
    
    # ------------- Build regressors for step s -------------
    # Endogenous lags from the y history buffer
    x_vals <- numeric(length(term_names))
    names(x_vals) <- term_names
    
    #if ("(Intercept)" %in% term_names) x_vals["(Intercept)"] <- 1
    
    # f_L2 and f_L4 are taken from the current history buffer (actuals up to origin, then forecasts)
    if (y_lag2_name %in% term_names) {
      x_vals[y_lag2_name] <- y_hist[length(y_hist) - 1]  # t-2 relative to target t = origin+s
    }
    if (y_lag4_name %in% term_names) {
      x_vals[y_lag4_name] <- y_hist[length(y_hist) - 3]  # t-4 relative to target
    }
    
    # ECT_food_L1 uses the last (known or recursive) ECT up to t-1
    if (ect_lag1_name %in% term_names) {
      x_vals[ect_lag1_name] <- ect_last
    }
    
    # Exogenous regressors at target date (taken from full_data)
    if (length(exo_names) > 0) {
      exo_row <- full_df[target_idx, exo_names, drop = FALSE]
      # Replace any NA dummies with 0
      exo_row[is.na(exo_row)] <- 0
      # Transfer into x_vals where names match
      for (nm in intersect(names(exo_row), names(x_vals))) {
        x_vals[nm] <- as.numeric(exo_row[[nm]])
      }
    }
    
    # Guard: any remaining NA regressors -> treat as 0 to avoid failure (or stop if you prefer)
    if (anyNA(x_vals)) {
      missing_x <- names(x_vals)[is.na(x_vals)]
      stop(paste("Missing regressors at", as.character(target_date), ":", paste(missing_x, collapse = ", ")))
    }
    
    # ------------- Predict growth (yhat) for step s -------------
    yhat_s <- sum(coef_vec * x_vals)
    
    # ------------- Update state for next step -------------
    # Update level using the growth yhat_s
    ln_food_next <- update_ln_from_growth(ln_food_last, yhat_s)
    
    # Compute LR fitted value at target to update ECT
    lr_hat_target <- predict_lr(full_df[target_idx, , drop = FALSE])
    ect_curr <- ln_food_next - lr_hat_target
    
    # Update buffers/states
    y_hist <- c(y_hist, yhat_s)     # append predicted y
    ln_food_last <- ln_food_next
    ect_last <- ect_curr
    
    # Save output
    out[[s]] <- tibble(
      origin_date          = as.Date(origin_date),   # <- coerce from yearqtr to Date
      target_date    = as.Date(target_date),   # <- coerce from yearqtr to Date
      horizon         = s,
      food_at_qoq_hat = yhat_s,
      ln_food_hat_path = ln_food_next,
      ECT_path_L0      = ect_curr
    )
  }
  
  bind_rows(out)
}

#--------------------------------------------------------
# 5) Build the set of origins: 2019Q4–2025Q2 (inclusive)
#--------------------------------------------------------
start_origin <- as.yearqtr("2019 Q4")
end_origin   <- as.yearqtr("2025 Q2")

origin_dates <- full_data %>%
  filter(date >= start_origin, date <= end_origin) %>%
  pull(date)

#--------------------------------------------------------
# 6) Run rolling forecasts over all origins (fixed coefs)
#--------------------------------------------------------

H <- 12
fcst_ecmf_25 <- purrr::map_df(origin_dates, ~ forecast_ecm_one_origin(.x, h = H))

#--------------------------------------------------------
# 7) (Optional) Attach actuals for error evaluation where available
#--------------------------------------------------------

full_data <- full_data %>%
  mutate(date = as.Date(date))

fcst_ecmf_25 <- fcst_ecmf_25 %>%
  left_join(full_data %>% select(date, !!y_name),
            by = c("target_date" = "date")) %>%
  rename(food_at_qoq_actual = !!y_name) %>%
  mutate(error = food_at_qoq_actual - food_at_qoq_hat)

# Convert date columns back to yearqtr
fcst_ecmf_25 <- fcst_ecmf_25 %>%
  mutate(origin_date = as.yearqtr(origin_date),
         target_date = as.yearqtr(target_date)) %>%
  arrange(origin_date, horizon)

print(head(fcst_ecmf_25, 20))


#------------------------------------------
#===========================================================
# ===== MODEL ESTIMATION 1991-2019 ====
#==========================================================
#------------------------------------------

full_data <- full_data %>%
  mutate(date = as.yearqtr(date))

train <- subset(full_data, 
                date >= as.yearqtr("1991 Q3") & 
                  date <= as.yearqtr("2019 Q4"))

#==== Food OLS_ADL model ===========

f_adl19 <- lm(food_at_qoq ~ 0 + f_L2 + f_L4 + e_L4 +
                ie_L1 + ie_L3 + pmdef_qoq + pmdef_L2+
                D_2001Q2+ D_GFC_2008,
            data=train) #'

summary (f_adl19)

# Coefficient test

lmtest::coeftest(f_adl19, vcov = NeweyWest(f_adl19, prewhite = FALSE))

#----------------------------------------------------------
# 3. FORECAST 12 QUARTERS HORIZON UTILITIES
#----------------------------------------------------------
# keeping fixed the coefficients 
beta19       <- coef(f_adl19) 
coef_names <- names(beta19)

# Build x-vector in correct order
build_x <- function(t, y_dyn, data, coef_names) {
  x <- setNames(numeric(length(coef_names)), coef_names)
  
  #if ("(Intercept)" %in% coef_names) x["(Intercept)"] <- 1
  
  # Dynamic food lags
  if ("f_L2" %in% coef_names)
    x["f_L2"] <- ifelse(t - 2 >= 1, y_dyn[t - 2], NA)
  
  if ("f_L4" %in% coef_names)
    x["f_L4"] <- ifelse(t - 4 >= 1, y_dyn[t - 4], NA)
  
  # Exogenous (always actual)
  exog_vars <- setdiff(coef_names, c( "f_L2", "f_L4"))
  for (v in exog_vars) x[v] <- data[[v]][t]
  
  # Replace NA in regressors with 0 (should not normally arise)
  x[is.na(x)] <- 0
  
  return(x)
}

#----------------------------------------------------------
# 4. SINGLE-ORIGIN DYNAMIC FORECAST (12Q)
#----------------------------------------------------------
forecast_origin <- function(origin_q, H = 12, data, beta19, coef_names) {
  
  dates <- data$date
  y_act <- data$food_at_qoq
  
  idx0 <- which(dates == origin_q)
  if (length(idx0) == 0) return(tibble())
  
  # Start dynamic path: actuals up to origin, forecast beyond
  y_dyn <- y_act
  if (idx0 < length(y_dyn))
    y_dyn[(idx0 + 1):length(y_dyn)] <- NA
  
  out <- vector("list", H)
  
  for (h in 1:H) {
    t <- idx0 + h
    if (t > nrow(data)) break
    
    # Build regressor vector
    x <- build_x(t, y_dyn, data, coef_names)
    
    # Forecast
    y_hat <- sum(beta19 * x)
    
    # Insert into dynamic path
    y_dyn[t] <- y_hat
    
    out[[h]] <- tibble(
      origin_date = as.character(origin_q),
      horizon    = h,
      target_date = as.character(dates[t]),
      y_hat      = y_hat,
      y_actual   = y_act[t],
      error      = ifelse(!is.na(y_act[t]), y_act[t] - y_hat, NA)
    )
  }
  
  bind_rows(out)
} 

#----------------------------------------------------------
# 5. RUN FORECASTS FOR ORIGINS 2019Q4–2025Q2
#----------------------------------------------------------
origins <- seq(as.yearqtr("2019 Q4"), as.yearqtr("2025 Q2"), by = 0.25)

fcst_olsf19 <- map_dfr(
  origins,
  ~ forecast_origin(.x, H = 12, data = full_data,
                    beta19 = beta19, coef_names = coef_names)
)

# Convert date columns back to yearqtr
fcst_olsf19 <- fcst_olsf19 %>%
  mutate(
    origin_date = as.yearqtr(origin_date),
    target_date = as.yearqtr(target_date)
  ) %>%
  arrange(origin_date, horizon)

# Show first rows
print(head(fcst_olsf19, 20))

#accuracy metrics
ac_olsf19 <- fcst_olsf19 %>%
  filter(!is.na(y_actual)) %>%
  group_by(horizon) %>%
  summarise(
    n       = n(),
    RMSE    = sqrt(mean((y_actual - y_hat)^2, na.rm = TRUE)),
    MAE     = mean(abs(y_actual - y_hat), na.rm = TRUE),
    Bias    = mean(y_hat - y_actual, na.rm = TRUE)
  ) %>%
  arrange(horizon)

print(ac_olsf19, n = Inf)


#======================================================
# ECM- MODEL 1991-2019
#======================================================

#----------------------------
# 1) Long-run (cointegration)
#----------------------------
# Estimation on 'train'; forecasting will use 'full_data'.
coint_food_lm19 <- lm(ln_food_at ~ ln_pmdef + ln_eer, data = train)

summary(coint_food_lm19)

# ECT in train (ok to use residuals because model fitted on train)
train <- train %>%
  mutate(
    ECT_food     = resid(coint_food_lm19),
    ECT_food_L1  = dplyr::lag(ECT_food, 1)
  )

# ECT in full_data: use LR prediction (do NOT use resid() here)
full_data <- full_data %>%
  mutate(
    date         = as.yearqtr(date),                              # ensure quarterly index
    ln_food_hat  = as.numeric(predict(coint_food_lm19, newdata = full_data)),
    ECT_food     = ln_food_at - ln_food_hat,
    ECT_food_L1  = dplyr::lag(ECT_food, 1)
  )

#----------------------------
# 2) Short-run ECM estimation
#----------------------------
# NOTE: 'train' and 'full_data' already contain eer_L1 and pmdef_L2 — do NOT recreate.
# The other regressors (ied, ECT_food_L1, D_GFC_2008) are assumed present.

ecm_food19 <- dynlm(food_at_qoq ~ 0 + f_L2 + f_L4 + e_L4 +
                      ie_L1 + ie_L3 + pmdef_qoq + pmdef_L2+
                      ECT_food_L1 + D_2001Q2+ D_GFC_2008,   #D_ENERGY_2023 convergence to long-run
  data = train
)

summary(ecm_food19)

#======================================================
# Rolling multi-step ECM forecasts with fixed coefficients
# Origins: 2019Q4 to 2025Q2
# Horizon: 12 quarters at each origin
#======================================================

#--------------------------------------------------------
# 1) Helper: Extract fixed coefficients and model terms
#--------------------------------------------------------
ecm_coef19 <- coef(ecm_food19)               # named numeric vector
ecm_terms <- names(ecm_coef19)             # includes "(Intercept)" if present

# Identify names we will fill from 'state' (endogenous lags) vs exogenous
y_name         <- "food_at_qoq"          # dependent (short-run)
y_lag2_name    <- "f_L2"                 # assumed: lag2 of y
y_lag4_name    <- "f_L4"                 # assumed: lag4 of y
ect_lag1_name  <- "ECT_food_L1"          # lagged error-correction term

# Other regressors (exogenous) are taken as-is from 'full_data'
# We'll pull them by name intersection with the coefficient vector
exo_names <- setdiff(ecm_terms, c(y_lag2_name, y_lag4_name, ect_lag1_name))

#--------------------------------------------------------
# 2) Long-run prediction function (for arbitrary dates)
#    Uses only exogenous LR vars from 'full_data'
#--------------------------------------------------------
predict_lr <- function(newdata_row) {
  # newdata_row is a 1-row data.frame with columns used by coint_food_lm
  as.numeric(predict(coint_food_lm19, newdata = newdata_row))
}

#--------------------------------------------------------
# 3) Transformation between growth and log-level
#    ASSUMPTION: food_at_qoq = 100 * (ln_food_t - ln_food_{t-1})
#--------------------------------------------------------
growth_scale <- 100  # change to 1 if your growth is in log units (not percent)

update_ln_from_growth <- function(ln_prev, growth_qoq) {
  ln_prev + (growth_qoq / growth_scale)
}

#--------------------------------------------------------
# 4) Forecaster for a single origin and horizon (dynamic)
#    - Keeps coefficients fixed (ecm_coef)
#    - Builds regressors for each horizon step
#    - Updates endogenous lags and ECT recursively
#--------------------------------------------------------
forecast_ecm_one_origin19 <- function(origin_date, h = 12,
                                    full_df = full_data,
                                    coef_vec = ecm_coef19,
                                    term_names = ecm_terms) {
  # Index of the origin in full_data
  if (!origin_date %in% full_df$date) {
    stop("Origin date not found in full_data: ", as.character(origin_date))
  }
  origin_idx <- which(full_df$date == origin_date)
  
  # Maximum horizon limited by data availability for exogenous regressors
  max_h_avail <- nrow(full_df) - origin_idx
  if (max_h_avail <= 0) {
    return(tibble())  # nothing to forecast
  }
  h_use <- min(h, max_h_avail)
  
  # Initialize "state" with last known actuals at the origin
  # Last actual level
  ln_food_last <- full_df$ln_food_at[origin_idx]
  if (is.na(ln_food_last)) stop("ln_food_at is NA at origin; cannot initialize level.")
  
  # y history to compute endogenous lags f_L2, f_L4
  # We need at least 4 quarters before origin to compute up to L4 for the first step
  if (origin_idx < 5) stop("Not enough history before origin to compute y lags.")
  
  # Build initial buffer of past y (actuals up to origin)
  y_hist <- full_df[[y_name]][1:origin_idx]  # vector up to origin
  if (any(is.na(tail(y_hist, 4)))) {
    stop("NA in recent y history before origin; cannot initialize endogenous lags.")
  }
  
  # Compute ECT at origin (lagged ECT to be used for the first forecast step)
  # ECT_t = ln_food_t - ln_food_hat_t; for step t+1 we use ECT_t as 'ECT_food_L1'
  lr_hat_origin <- predict_lr(full_df[origin_idx, , drop = FALSE])
  ect_last <- ln_food_last - lr_hat_origin
  if (is.na(ect_last)) stop("Cannot compute ECT at origin (check LR inputs).")
  
  # Container for forecast results
  out <- vector("list", length = h_use)
  
  # Iterate horizons
  for (s in seq_len(h_use)) {
    target_idx  <- origin_idx + s
    target_date <- full_df$date[target_idx]
    
    # ------------- Build regressors for step s -------------
    # Endogenous lags from the y history buffer
    x_vals <- numeric(length(term_names))
    names(x_vals) <- term_names
    
    #if ("(Intercept)" %in% term_names) x_vals["(Intercept)"] <- 1
    
    # f_L2 and f_L4 are taken from the current history buffer (actuals up to origin, then forecasts)
    if (y_lag2_name %in% term_names) {
      x_vals[y_lag2_name] <- y_hist[length(y_hist) - 1]  # t-2 relative to target t = origin+s
    }
    if (y_lag4_name %in% term_names) {
      x_vals[y_lag4_name] <- y_hist[length(y_hist) - 3]  # t-4 relative to target
    }
    
    # ECT_food_L1 uses the last (known or recursive) ECT up to t-1
    if (ect_lag1_name %in% term_names) {
      x_vals[ect_lag1_name] <- ect_last
    }
    
    # Exogenous regressors at target date (taken from full_data)
    if (length(exo_names) > 0) {
      exo_row <- full_df[target_idx, exo_names, drop = FALSE]
      # Replace any NA dummies with 0
      exo_row[is.na(exo_row)] <- 0
      # Transfer into x_vals where names match
      for (nm in intersect(names(exo_row), names(x_vals))) {
        x_vals[nm] <- as.numeric(exo_row[[nm]])
      }
    }
    
    # Guard: any remaining NA regressors -> treat as 0 to avoid failure (or stop if you prefer)
    if (anyNA(x_vals)) {
      missing_x <- names(x_vals)[is.na(x_vals)]
      stop(paste("Missing regressors at", as.character(target_date), ":", paste(missing_x, collapse = ", ")))
    }
    
    # ------------- Predict growth (yhat) for step s -------------
    yhat_s <- sum(coef_vec * x_vals)
    
    # ------------- Update state for next step -------------
    # Update level using the growth yhat_s
    ln_food_next <- update_ln_from_growth(ln_food_last, yhat_s)
    
    # Compute LR fitted value at target to update ECT
    lr_hat_target <- predict_lr(full_df[target_idx, , drop = FALSE])
    ect_curr <- ln_food_next - lr_hat_target
    
    # Update buffers/states
    y_hist <- c(y_hist, yhat_s)     # append predicted y
    ln_food_last <- ln_food_next
    ect_last <- ect_curr
    
    # Save output
    out[[s]] <- tibble(
      origin_date          = as.Date(origin_date),   # <- coerce from yearqtr to Date
      target_date    = as.Date(target_date),   # <- coerce from yearqtr to Date
      horizon         = s,
      food_at_qoq_hat = yhat_s,
      ln_food_hat_path = ln_food_next,
      ECT_path_L0      = ect_curr
    )
  }
  
  bind_rows(out)
}

#--------------------------------------------------------
# 5) Build the set of origins: 2019Q4–2025Q2 (inclusive)
#--------------------------------------------------------
start_origin <- as.yearqtr("2019 Q4")
end_origin   <- as.yearqtr("2025 Q2")

origin_dates <- full_data %>%
  filter(date >= start_origin, date <= end_origin) %>%
  pull(date)

#--------------------------------------------------------
# 6) Run rolling forecasts over all origins (fixed coefs)
#--------------------------------------------------------

H <- 12
fcst_ecmf19 <- purrr::map_df(origin_dates, ~ forecast_ecm_one_origin19(.x, h = H))

#--------------------------------------------------------
# 7) (Optional) Attach actuals for error evaluation where available
#--------------------------------------------------------

full_data <- full_data %>%
  mutate(date = as.Date(date))

fcst_ecmf19 <- fcst_ecmf19 %>%
  left_join(full_data %>% select(date, !!y_name),
            by = c("target_date" = "date")) %>%
  rename(food_at_qoq_actual = !!y_name) %>%
  mutate(error = food_at_qoq_actual - food_at_qoq_hat)


# Convert date columns back to yearqtr
fcst_ecmf19 <- fcst_ecmf19 %>%
  mutate(origin_date = as.yearqtr(origin_date),
         target_date = as.yearqtr(target_date)) %>%
  arrange(origin_date, horizon)

print(head(fcst_ecmf19, 20))


#=========================================================
# ECM - ROLLING RE-ESTIMATION + MULTI-HORIZON FORECASTS
# Evaluation window: 2019Q4–2025Q2
#=========================================================

#----------------------------
# Helpers & evaluation window
#----------------------------
add_quarters <- function(qtr, n) as.yearqtr(qtr) + n / 4

eval_start   <- as.yearqtr("2019 Q4")
eval_end     <- as.yearqtr("2025 Q2")
origins      <- seq(eval_start, eval_end, by = 1/4)
H            <- 12

# Ensure quarterly index in full_data
full_data <- full_data %>% mutate(date = as.yearqtr(date))

# Response name for later pulls
# (Set once — the response is the LHS of your ECM)
response_name <- "food_at_qoq"

# Schema for empty rows
empty_row <- tibble(
  origin_date   = as.yearqtr(NA),
  horizon       = as.integer(NA),
  target_date = as.yearqtr(NA),
  forecast      = as.numeric(NA),
  actual        = as.numeric(NA),
  error         = as.numeric(NA)
)

# Re-usable fetcher for a SINGLE target-date row of predictors
get_regressors_at <- function(df, tdate, xnames) {
  df_t <- df %>% filter(date == tdate)
  if (nrow(df_t) > 1) df_t <- df_t %>% distinct(across(everything()), .keep_all = TRUE)
  if (nrow(df_t) == 0) return(NULL)
  if (!all(xnames %in% names(df_t))) return(NULL)
  out <- df_t %>% select(all_of(xnames))
  if (nrow(out) != 1L) return(NULL)
  if (!all(stats::complete.cases(out))) return(NULL)
  as.data.frame(out)
}

#-------------------------------------------
# Core rolling loop: re-estimate at each origin
#-------------------------------------------
fcst_ecmr19_25 <- purrr::map_dfr(origins, function(o) {
  # 1) Build estimation slice up to and including origin
  est_slice <- full_data %>% filter(date <= o)
  
  # Guard: need enough data to estimate LR and ECM
  if (nrow(est_slice) < 24) return(empty_row)  # adjust threshold if you wish
  
  # 2) Long-run (cointegration) AT ORIGIN
  coint_o <- tryCatch(
    lm(ln_food_at ~ ln_pmdef + ln_eer, data = est_slice),
    error = function(e) NULL
  )
  if (is.null(coint_o)) return(empty_row)
  
  # Add ECT to estimation slice (residuals are valid because we fit on est_slice)
  est_slice <- est_slice %>%
    mutate(
      ECT_food    = resid(coint_o),
      ECT_food_L1 = dplyr::lag(ECT_food, 1)
    )
  
  # 3) Short-run ECM AT ORIGIN (using only <= origin data)
  ecm_o <- tryCatch(
    dynlm(food_at_qoq ~ 0 + f_L2 + f_L4 + e_L4 +
            ie_L1 + ie_L3 + pmdef_qoq + pmdef_L2+
            ECT_food_L1+D_2001Q2+ D_GFC_2008,
      data = est_slice
    ),
    error = function(e) NULL
  )
  if (is.null(ecm_o)) return(empty_row)
  
  # Extract predictor names for this origin-specific ECM
  preds_o <- attr(ecm_o$terms, "term.labels")
  
  # 4) Build a per-origin FULL view with ECT computed from LR@origin (no look-ahead in LR params)
  full_view_o <- full_data %>%
    mutate(
      ln_food_hat  = as.numeric(predict(coint_o, newdata = full_data)),
      ECT_food     = ln_food_at - ln_food_hat,
      ECT_food_L1  = dplyr::lag(ECT_food, 1)
    )
  
  growth_scale <- 100  # if food_at_qoq is in percent; set to 1 if it's in log points
  
  # 5) Multi-horizon dynamic forecasts from origin o (NO look-ahead)
  b         <- coef(ecm_o)
  terms_o   <- names(b)
  y_lag2    <- "f_L2"
  y_lag4    <- "f_L4"
  ect_l1    <- "ECT_food_L1"
  
  # Exogenous names for this origin's ECM (pull at each tdate from full_view_o)
  exo_names_o <- intersect(
    setdiff(terms_o, c(y_lag2, y_lag4, ect_l1)),
    names(full_view_o)
  )
  
  # Locate the origin row in the per-origin view
  origin_idx <- which(full_view_o$date == o)
  if (length(origin_idx) != 1L) return(empty_row)
  
  # Initialise states at the origin
  ln_food_last <- full_view_o$ln_food_at[origin_idx]     # last known level at origin
  if (is.na(ln_food_last)) return(empty_row)
  
  # History of y (food_at_qoq) up to the origin to seed the endogenous lags
  y_hist <- full_view_o[[response_name]][1:origin_idx]
  if (length(y_hist) < 4 || any(is.na(tail(y_hist, 4)))) return(empty_row)
  
  # ECT at the origin (L0) using LR@origin with ACTUAL level at origin
  ect_last <- full_view_o$ECT_food[origin_idx]
  
  purrr::map_dfr(1:H, function(h) {
    t_idx <- origin_idx + h
    if (t_idx > nrow(full_view_o)) return(empty_row)
    tdate <- full_view_o$date[t_idx]
    
    # keep inside the evaluation window
    if (tdate < eval_start || tdate > eval_end) return(empty_row)
    
    # ----------------- Build regressors for step h -----------------
    x <- numeric(length(terms_o)); names(x) <- terms_o
    #if ("(Intercept)" %in% terms_o) x["(Intercept)"] <- 1
    
    # endogenous lags from the forecast buffer
    if (y_lag2 %in% terms_o) x[y_lag2] <- y_hist[length(y_hist) - 1]
    if (y_lag4 %in% terms_o) x[y_lag4] <- y_hist[length(y_hist) - 3]
    
    # lagged ECT from the recursively updated path
    if (ect_l1 %in% terms_o) x[ect_l1] <- ect_last
    
    # exogenous regressors at tdate from full_view_o
    if (length(exo_names_o) > 0) {
      exo_row <- full_view_o[t_idx, exo_names_o, drop = FALSE]
      exo_row[is.na(exo_row)] <- 0
      for (nm in names(exo_row)) x[nm] <- as.numeric(exo_row[[nm]])
    }
    
    # ----------------- Predict one step ahead -----------------
    yhat <- sum(b * x)  # food_at_qoq_hat at tdate
    
    # update level recursively using the predicted growth
    ln_next <- ln_food_last + (yhat / growth_scale)
    
    # recompute LR fitted value at tdate with coint_o (LR@origin)
    lr_hat_t <- as.numeric(predict(coint_o, newdata = full_view_o[t_idx, , drop = FALSE]))
    
    # contemporaneous ECT on the forecasted level at t (this will be L1 next step)
    ect_curr <- ln_next - lr_hat_t
    
    # realised actual (for evaluation, if available)
    act <- suppressWarnings(as.numeric(full_view_o[[response_name]][t_idx]))
    
    # update states for next horizon
    y_hist       <- c(y_hist, yhat)
    ln_food_last <- ln_next
    ect_last     <- ect_curr
    
    tibble(
      origin_date   = o,
      horizon       = h,
      target_date   = tdate,
      food_at_qoq_hat     = yhat,
      actual        = act,
      error         = if (is.finite(act)) act - yhat else NA_real_
    )
  })
})
# Keep only populated rows
fcst_ecmr19_25 <- fcst_ecmr19_25 %>% filter(!is.na(horizon))

#-------------------------------------------
# Accuracy by horizon (re-estimated ECM)
#-------------------------------------------
metrics_ecmr19_25 <- fcst_ecmr19_25 %>%
  group_by(horizon) %>%
  summarise(
    n    = sum(!is.na(error)),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAE  = mean(abs(error), na.rm = TRUE),
    MAPE = mean(abs(error / actual)[is.finite(error / actual)], na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(head(metrics_ecmr19_25, 20))

#======================================================
#### FORMAT DATAFRAME to use the FORECAST EVALUATION TOOL
#=======================================================

forecast_data_1 <- fcst_olsf25 %>%
  mutate(vintage_date = as.character(origin_date),
         value = y_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Food inflation",
         source = "OLS-FixedCoef25",
         date = as.character(target_date),
         .keep = "none")
forecast_data_2 <- fcst_olsf19 %>%
  mutate(vintage_date = as.character(origin_date),
         value = y_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Food inflation",
         source = "OLS-FixedCoef19",
         date = as.character(target_date),
         .keep = "none")

forecast_data_3 <- fcst_ecmf_25  %>%
  mutate(vintage_date = as.character(origin_date),
         value = food_at_qoq_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Food inflation",
         source = "ECM-FixedCoef25",
         date = as.character(target_date),
         .keep = "none")

forecast_data_4 <- fcst_ecmf19  %>%
  mutate(vintage_date = as.character(origin_date),
         value = food_at_qoq_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Food inflation",
         source = "ECM-FixedCoef19",
         date = as.character(target_date),
         .keep = "none")

forecast_data_5 <- fcst_ecmr19_25  %>%
  mutate(vintage_date = as.character(origin_date),
         value = food_at_qoq_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Food inflation",
         source = "ECM-RollingCoef19_25",
         date = as.character(target_date),
         .keep = "none")

forecast_data = rbind(forecast_data_1,forecast_data_2, forecast_data_3, forecast_data_4,forecast_data_5)


## create vintages of outturns
df_outturn = list()
j = 1
for (i in 115:136){
  df_outturn[[j]] = full_data %>%
    filter(date <= full_data$date[i]) %>%
    mutate(vintage_date = full_data$date[i])
  j = j +1
}
df_outturn = dplyr::bind_rows(df_outturn)

df_outturn <- df_outturn %>%
  mutate(value = core_gds_qoq,
         frequency = "Q",
         forecast_horizon = (date - vintage_date)*4,
         variable = "Core goods inflation",
         date = as.character(date),
         vintage_date = as.character(vintage_date),
         .keep = "none")

# saving data in parquet format
library(arrow)

# Ensure local data directory exists
dir.create("C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data", recursive = TRUE, showWarnings = FALSE)
write_parquet(df_outturn, "C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data/outturn_datacg.parquet")
write_parquet(forecast_data, "C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data/forecast_datacg.parquet")



