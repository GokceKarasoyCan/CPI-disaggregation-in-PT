### IMPORTING DATA from EXCEL ####
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

cpi_tbl$date <- as.Date(cpi_tbl$Date, format = "%Y-%m-%d")
str(cpi_tbl)

cpi_tbl <-
  cpi_tbl %>%
  rename(capu_dat = capu_f) %>%
  mutate(u_rate = unemp*100, u_rate_f = unemp_f*100, pmdef = pmdef*100)

#Data in levels (Indexes, rates) 1991-2019
df_cpi_level <- 
  cpi_tbl %>%
  dplyr::select(date, cpi, core_gds, energy, services, food_at, ois_rate, bank_rate, infl_exp, ied, wage, pmdef, eer, cpif) %>%
  slice_min(n = 141, order_by = date)# Selecting back data period

str(df_cpi_level)

#Data in log-levels

df_cpi_log <-
  df_cpi_level%>%
  mutate(across(c(cpi, core_gds, 
                  energy, services, food_at, wage,
                  pmdef, eer, cpif),
                ~ log(.), # log is function to calculate natural logarithm
                .names =("ln_{.col}")))

# Transforming data to one period percentage Change

# Quarterly percentage change
df_cpi_qoq <-
  df_cpi_log %>%
  mutate(across(c(cpi, core_gds, 
                  energy, services, food_at, wage, 
                  pmdef, eer, cpif),
                ~ Delt(., type = "log")*100, # percent change using log diff
                .names =("{.col}_qoq")))%>%
  slice(-1)

# Annual percentage change
df_cpi_yoy <-
  df_cpi_log %>%
  mutate(
    across(
      c(cpi, core_gds, energy, services, food_at, wage, pmdef, eer, cpif),
      ~ Delt(., k = 4, type = "log") * 100,  # YoY percent change
      .names = "{.col}_yoy"
    )
  )

# Creating lags
df_cpi_percent <- 
  df_cpi_qoq %>%
  mutate(s_L1 = lag(services_qoq, 1),
         s_L2 = lag(services_qoq, 2),
         s_L3 = lag(services_qoq, 3),
         s_L4 = lag(services_qoq, 4),
         cg_L1 = lag(core_gds_qoq, 1),
         cg_L2 = lag(core_gds_qoq, 1),
         cg_L3 = lag(core_gds_qoq, 3),
         cg_L4 = lag(core_gds_qoq, 4),
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
         e_L1 = lag(energy_qoq, 1),
         e_L2 = lag(energy_qoq, 2),
         e_L3 = lag(energy_qoq, 3),
         e_L4 = lag(energy_qoq, 4),
         wage_L1 = lag(wage_qoq,1),
         pmdef_L1 = lag(pmdef_qoq, 1),
         pmdef_L2 = lag(pmdef_qoq, 2),
         pmdef_L3 = lag(pmdef_qoq, 3),
         pmdef_L4 = lag(pmdef_qoq, 4)) %>%
  na.omit(df_cpi_percent)

# creating dummies

#Changing date format before creating dummies

'df_cpi_tbl <- 
  df_cpi_percent %>% 
  as_tibble() %>%
  tibble(date = seq.Date(from = as.Date("1991-09-30"),
                         to   = as.Date("2025-06-30"),
                         by   = "3 months"))'
df_cpi_tbl <-
  df_cpi_percent %>%
  mutate(D_1991Q3 = ifelse(date >= as.Date("1991-09-30") & date <= as.Date("1991-09-30"), 1, 0)) %>%
  mutate(D_SHOCK_1992 = ifelse(date >= as.Date("1992-09-30") & date <= as.Date("1992-09-30"), 1, 0)) %>%
  mutate(D_GFC_2009 = ifelse(date >= as.Date("2009-06-30") & date <= as.Date("2009-06-30"), 1, 0)) %>%
  mutate(D_COVID_2020 = ifelse(date >= as.Date("2020-06-30") & date <= as.Date("2020-06-30"), 1, 0)) %>%
  mutate(D_2021Q2 = ifelse(date >= as.Date("2021-06-30") & date <= as.Date("2021-06-30"), 1, 0)) %>%
  mutate(D_2023Q2 = ifelse(date >= as.Date("2023-06-30") & date <= as.Date("2023-06-30"), 1, 0)) %>%
  mutate(D_2023Q3 = ifelse(date >= as.Date("2023-09-30") & date <= as.Date("2023-09-30"), 1, 0))


'#===== DATA ANALITICS AND STATISTIC DESCRIPTIVE =====

library(boeCharts)
library(zoo)

#Plotting yoy series

df_cpi_yoy %>%
  select(date, cpi_yoy,services_yoy,food_at_yoy,core_gds_yoy)%>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(series = factor(series,
                         levels = c("cpi_yoy","services_yoy","core_gds_yoy","food_at_yoy"),
                         labels = c("Headline CPI","Services","Core goods","Food")))%>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.8) +
  labs(x = NULL, y = "Percentage change (yoy)", colour = NULL) +
  theme_boe_identity()

#Calculating descriptive statistics

#date format
df_cpi_yoy <- 
  df_cpi_yoy %>%
  mutate(qtr = as.yearqtr(date))

#Helper function
summary_by_period <- function(data, start_qtr, end_qtr) {
  data %>%
    filter(qtr >= as.yearqtr(start_qtr),
           qtr <= as.yearqtr(end_qtr)) %>%
    summarise(
      across(
        c(cpi_yoy, food_at_yoy, core_gds_yoy,energy_yoy, services_yoy),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ sd(.x,   na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    mutate(period = paste0(start_qtr, "–", end_qtr)) %>%
    relocate(period)
}

# Descriptive statistic
stats_all <- bind_rows(
  summary_by_period(df_cpi_yoy, "1997 Q1", "2019 Q4"),
  summary_by_period(df_cpi_yoy, "2020 Q1", "2025 Q2")
  #summary_by_period(df_cpi_yoy, "1997 Q1", "2025 Q2")
)
stats_all#'


#===== MODEL ESTIMATION =====

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
  dplyr::select(date,services_qoq, s_L1,s_L2,s_L3,s_L4,
                infl_exp, ie_L1,ie_L2, ie_L3, ie_L4,ied, ied_L1,ied_L2,ied_L3,ied_L4, 
                eer_qoq, eer_L1, eer_L2, eer_L3,eer_L4,
                energy_qoq, e_L1, e_L2, e_L3,e_L4,
                pmdef_qoq, pmdef_L1 , pmdef_L2 , pmdef_L3 , pmdef_L4, 
                ois_rate, or_L1, or_L2, bank_rate, wage_qoq, wage_L1,
                D_1991Q3,D_GFC_2009,D_SHOCK_1992,D_COVID_2020,D_2021Q2,D_2023Q2)

#Ensuring date format
full_data$date <- as.yearqtr(full_data$date)

full_data <- as.data.frame(full_data)


#===========================================
# Full sample core good model 1990Q3-2025Q2 
#===========================================
full_sample<- subset(full_data, 
                     date >= as.yearqtr("1991 Q3") & 
                       date <= as.yearqtr("2025 Q2"))

's_adl <- step(lm(services_qoq  ~ s_L1 + s_L2 + s_L3 + s_L4+
                    infl_exp+ ie_L1+ie_L2+ie_L3+ie_L4+
                    energy_qoq + e_L1+ e_L2+ e_L3 + e_L4+
                    eer_qoq+ eer_L1+ eer_L2+ eer_L3+eer_L4+
                    pmdef_qoq + pmdef_L1 + pmdef_L2 + pmdef_L3 + pmdef_L4 +
                    D_1991Q3+D_2021Q2+D_2023Q2, 
                    data=full_sample),
     direction = "backward", k = log(nrow(full_sample)))  # BIC' 


s_adl <- lm(services_qoq ~ s_L1 + infl_exp + 
              eer_qoq + pmdef_qoq + 
              D_1991Q3 + D_2021Q2 + D_2023Q2,
             data=full_sample)#'

summary (s_adl)

# Coefficient test
lmtest::coeftest(s_adl, vcov = NeweyWest(s_adl, prewhite = FALSE))


#=== OUTLIER TEST TO SET DUMMIES ===#

# Approach_1
# Compute standardised residuals
t_resid_s <- rstandard(s_adl)
# Flag outliers (e.g., |residual| > 3)
outliers_s <- which(abs(t_resid_s) > 3)
full_sample[outliers_s, "services_qoq"]
full_sample[outliers_s, "date"]

# Approach_2
cooks_s <- cooks.distance(s_adl)
# Flag influential observations (rule of thumb: > 4/n)
threshold_s <- 4 / nrow(full_sample)
which(cooks_s > threshold_s)

# Approach_3
outlierTest(s_adl)  # Bonferroni p-values for outliers

#=== STRUCTURAL CHANGE AND STABILITY TEST ===#

# Identification of breakpoints
s_bp <- breakpoints(services_qoq ~ s_L1 + infl_exp + 
                      eer_qoq + pmdef_qoq + 
                      D_1991Q3 + D_2021Q2 + D_2023Q2,
                     data = full_sample)
summary(s_bp)
plot(s_bp)

breakpoints(services_qoq ~ s_L1 + infl_exp + 
              eer_qoq + pmdef_qoq + 
              D_1991Q3 + D_2021Q2 + D_2023Q2,
            data = full_sample, breaks = 5)

# stability test
sctest(services_qoq ~ s_L1 + infl_exp + 
         eer_qoq + pmdef_qoq + 
         D_1991Q3 + D_2021Q2 + D_2023Q2,
       data = full_sample, type = "supF")

# PLOTTING RESIDUALS

#plot 1
resid_s_adl <- resid(s_adl)
lag.plot(resid_s_adl, diag.col = "forest green"
         , main = "Lag Scatter Plot - Services OLS_ADL MODEL")

#plot 2

res <- as.numeric(resid_s_adl)
m <- mean(res, na.rm = TRUE)
s <- sd(res, na.rm = TRUE)

hist(res,
     breaks = "FD", freq = FALSE,
     col = "gray90", border = "white",
     main = "Residuals – Core goods OLS_ADL",
     xlab = "Residuals")

# Kernel density estimate (data-driven)
lines(density(res, na.rm = TRUE), col = "steelblue", lwd = 2, lty = 2)

# Normal curve with sample mean/sd
curve(dnorm(x, mean = m, sd = s), add = TRUE, col = "red", lwd = 2)

legend("topright",
       legend = c("Kernel density", "Normal curve"),
       col = c("steelblue", "red"), lwd = 2, lty = c(2,1), bty = "n")


#----------------------------------------------------------
# 3. FORECAST UTILITIES
#----------------------------------------------------------
# keeping fixed the coefficients 
beta25       <- coef(s_adl)
coef_names <- names(beta25)

# Build x-vector in correct order
build_x <- function(t, y_dyn, data, coef_names) {
  x <- setNames(numeric(length(coef_names)), coef_names)
  
  if ("(Intercept)" %in% coef_names) x["(Intercept)"] <- 1
  
  # Dynamic core_good lags
  if ("s_L1" %in% coef_names)
    x["s_L1"] <- ifelse(t - 1 >= 1, y_dyn[t - 1], NA)
  
  #if ("s_L3" %in% coef_names)
  #x["s_L3"] <- ifelse(t - 3 >= 1, y_dyn[t - 3], NA)
  
  # Exogenous (always actual)
  exog_vars <- setdiff(coef_names, c("(Intercept)", "s_L1"))
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
  y_act <- data$services_qoq
  
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

fcst_s_ols25 <- map_dfr(
  origins,
  ~ forecast_origin(.x, H = 12, data = full_data,
                    beta25 = beta25, coef_names = coef_names)
)

# Convert date columns back to yearqtr
fcst_s_ols25 <- fcst_s_ols25 %>%
  mutate(
    origin_date = as.yearqtr(origin_date),
    target_date = as.yearqtr(target_date)
  ) %>%
  arrange(origin_date, horizon)

# Show first rows
print(head(fcst_s_ols25, 20))


#accuracy metrics
ac_sf25 <- fcst_s_ols25 %>%
  filter(!is.na(y_actual)) %>%
  group_by(horizon) %>%
  summarise(
    n       = n(),
    RMSE    = sqrt(mean((y_actual - y_hat)^2, na.rm = TRUE)),
    MAE     = mean(abs(y_actual - y_hat), na.rm = TRUE),
    Bias    = mean(y_hat - y_actual, na.rm = TRUE)
  ) %>%
  arrange(horizon)

print(ac_sf25, n = Inf)

#===============================================
######## BENCHMARK AR(1) MODEL  1991-2025 #####
#===============================================

s_ar25 <- lm(services_qoq ~ s_L1 ,
              data=full_sample)#'

summary (s_ar25)

# Coefficient test
lmtest::coeftest(s_ar25 , vcov = NeweyWest(s_ar25, prewhite = FALSE))


#----------------------------------------------------------
# 3. FORECAST UTILITIES
#----------------------------------------------------------
# keeping fixed the coefficients 
beta_ar25       <- coef(s_ar25)
coef_names_ar25 <- names(beta_ar25)

# Build x-vector in correct order
build_x <- function(t, y_dyn, data, coef_names_ar25) {
  x <- setNames(numeric(length(coef_names_ar25)), coef_names_ar25)
  
  if ("(Intercept)" %in% coef_names_ar25) x["(Intercept)"] <- 1
  
  # Dynamic core_good lags
  if ("s_L1" %in% coef_names_ar25)
    x["s_L1"] <- ifelse(t - 1 >= 1, y_dyn[t - 1], NA)
  
  #if ("s_L3" %in% coef_names_ar25)
  #x["s_L3"] <- ifelse(t - 3 >= 1, y_dyn[t - 3], NA)
  
  # Exogenous (always actual)
  exog_vars <- setdiff(coef_names_ar25, c("(Intercept)", "s_L1"))
  for (v in exog_vars) x[v] <- data[[v]][t]
  
  # Replace NA in regressors with 0 (should not normally arise)
  x[is.na(x)] <- 0
  
  return(x)
}

#----------------------------------------------------------
# 4. SINGLE-ORIGIN DYNAMIC FORECAST (12Q)
#----------------------------------------------------------
forecast_origin <- function(origin_q, H = 12, data, beta_ar25, coef_names_ar25) {
  
  dates <- data$date
  y_act <- data$services_qoq
  
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
    x <- build_x(t, y_dyn, data, coef_names_ar25)
    
    # Forecast
    y_hat <- sum(beta_ar25 * x)
    
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

fcst_s_ar25 <- map_dfr(
  origins,
  ~ forecast_origin(.x, H = 12, data = full_data,
                    beta_ar25 = beta_ar25, coef_names_ar25 = coef_names_ar25)
)

# Convert date columns back to yearqtr
fcst_s_ar25 <- fcst_s_ar25 %>%
  mutate(
    origin_date = as.yearqtr(origin_date),
    target_date = as.yearqtr(target_date)
  ) %>%
  arrange(origin_date, horizon)

# Show first rows
print(head(fcst_s_ar25, 20))

#accuracy metrics
ac_s_ar25 <- fcst_s_ar25 %>%
  filter(!is.na(y_actual)) %>%
  group_by(horizon) %>%
  summarise(
    n       = n(),
    RMSE    = sqrt(mean((y_actual - y_hat)^2, na.rm = TRUE)),
    MAE     = mean(abs(y_actual - y_hat), na.rm = TRUE),
    Bias    = mean(y_hat - y_actual, na.rm = TRUE)
  ) %>%
  arrange(horizon)

print(ac_s_ar25, n = Inf)


#======================================================
# MODEL ESTIMATION 1991Q3-2019Q4 FOR OUTSAMPLE FORECAST 
#======================================================

train<- subset(full_data, 
               date >= as.yearqtr("1991 Q3") & 
                 date <= as.yearqtr("2019 Q4"))


s_adl19 <- lm(services_qoq ~ s_L1 + infl_exp + 
                eer_qoq + pmdef_qoq + 
                D_1991Q3,
               data=train)

summary (s_adl19)
# Coefficient test
lmtest::coeftest(s_adl19, vcov = NeweyWest(s_adl19, prewhite = FALSE))

#----------------------------------------------------------
# 3. OUT SAMPLE MULTI-STEP FORECAST WITH MODEL COEFF-FIXED
#----------------------------------------------------------
# keeping fixed the coefficients 
beta19       <- coef(s_adl19)
coef_names19 <- names(beta19)

# Build x-vector in correct order
build_x <- function(t, y_dyn, data, coef_names19) {
  x <- setNames(numeric(length(coef_names19)), coef_names19)
  
  if ("(Intercept)" %in% coef_names19) x["(Intercept)"] <- 1
  
  # Dynamic core_good lags
  if ("s_L1" %in% coef_names19)
    x["s_L1"] <- ifelse(t - 1 >= 1, y_dyn[t - 1], NA)
  
  #if ("s_L3" %in% coef_names19)
  #x["s_L3"] <- ifelse(t - 3 >= 1, y_dyn[t - 3], NA)
  
  # Exogenous (always actual)
  exog_vars <- setdiff(coef_names19, c("(Intercept)", "s_L1"))
  for (v in exog_vars) x[v] <- data[[v]][t]
  
  # Replace NA in regressors with 0 (should not normally arise)
  x[is.na(x)] <- 0
  
  return(x)
}

#----------------------------------------------------------
# 4. SINGLE-ORIGIN DYNAMIC FORECAST (12Q)
#----------------------------------------------------------
forecast_origin <- function(origin_q, H = 12, data, beta19, coef_names19) {
  
  dates <- data$date
  y_act <- data$services_qoq
  
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
    x <- build_x(t, y_dyn, data, coef_names19)
    
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

fcst_s_ols19 <- map_dfr(
  origins,
  ~ forecast_origin(.x, H = 12, data = full_data,
                    beta19 = beta19, coef_names19 = coef_names19)
)

# Convert date columns back to yearqtr
fcst_s_ols19 <- fcst_s_ols19 %>%
  mutate(
    origin_date = as.yearqtr(origin_date),
    target_date = as.yearqtr(target_date)
  ) %>%
  arrange(origin_date, horizon)

# Show first rows
print(head(fcst_s_ols19, 20))

#accuracy metrics
ac_sf19 <- fcst_s_ols19 %>%
  filter(!is.na(y_actual)) %>%
  group_by(horizon) %>%
  summarise(
    n       = n(),
    RMSE    = sqrt(mean((y_actual - y_hat)^2, na.rm = TRUE)),
    MAE     = mean(abs(y_actual - y_hat), na.rm = TRUE),
    Bias    = mean(y_hat - y_actual, na.rm = TRUE)
  ) %>%
  arrange(horizon)

print(ac_sf19, n = Inf)


#==============================================
######## BENCHMARK AR(1) MODEL 1991-2019 #####
#=============================================

s_ar19 <- lm(services_qoq ~ s_L1 ,
              data=train)#'

summary (s_ar19)

# Coefficient test
lmtest::coeftest(s_ar19 , vcov = NeweyWest(s_ar19, prewhite = FALSE))

#----------------------------------------------------------
# 3. FORECAST UTILITIES
#----------------------------------------------------------
# keeping fixed the coefficients 
beta_ar19       <- coef(s_ar19)
coef_names_ar19 <- names(beta_ar19)

# Build x-vector in correct order
build_x <- function(t, y_dyn, data, coef_names_ar19) {
  x <- setNames(numeric(length(coef_names_ar19)), coef_names_ar19)
  
  if ("(Intercept)" %in% coef_names_ar19) x["(Intercept)"] <- 1
  
  # Dynamic core_good lags
  if ("s_L1" %in% coef_names_ar19)
    x["s_L1"] <- ifelse(t - 1 >= 1, y_dyn[t - 1], NA)
  
  #if ("s_L3" %in% coef_names_ar19)
  #x["s_L3"] <- ifelse(t - 3 >= 1, y_dyn[t - 3], NA)
  
  # Exogenous (always actual)
  exog_vars <- setdiff(coef_names_ar19, c("(Intercept)", "s_L1"))
  for (v in exog_vars) x[v] <- data[[v]][t]
  
  # Replace NA in regressors with 0 (should not normally arise)
  x[is.na(x)] <- 0
  
  return(x)
}

#----------------------------------------------------------
# 4. SINGLE-ORIGIN DYNAMIC FORECAST (12Q)
#----------------------------------------------------------
forecast_origin <- function(origin_q, H = 12, data, beta_ar19, coef_names_ar19) {
  
  dates <- data$date
  y_act <- data$services_qoq
  
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
    x <- build_x(t, y_dyn, data, coef_names_ar19)
    
    # Forecast
    y_hat <- sum(beta_ar19 * x)
    
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

fcst_s_ar19 <- map_dfr(
  origins,
  ~ forecast_origin(.x, H = 12, data = full_data,
                    beta_ar19 = beta_ar19, coef_names_ar19 = coef_names_ar19)
)

# Convert date columns back to yearqtr
fcst_s_ar19 <- fcst_s_ar19 %>%
  mutate(
    origin_date = as.yearqtr(origin_date),
    target_date = as.yearqtr(target_date)
  ) %>%
  arrange(origin_date, horizon)

# Show first rows
print(head(fcst_s_ar19, 20))


#accuracy metrics
ac_s_ar19 <- fcst_s_ar19 %>%
  filter(!is.na(y_actual)) %>%
  group_by(horizon) %>%
  summarise(
    n       = n(),
    RMSE    = sqrt(mean((y_actual - y_hat)^2, na.rm = TRUE)),
    MAE     = mean(abs(y_actual - y_hat), na.rm = TRUE),
    Bias    = mean(y_hat - y_actual, na.rm = TRUE)
  ) %>%
  arrange(horizon)

print(ac_s_ar19, n = Inf)

#==========================================================
#  ROLLING WINDOW ESTIMATION 
#  Re-estimate coefficients at each origin (expanding window)
#  Dynamic recursion: actual lags at origin; forecasted lags beyond
#==========================================================

#----------------------------------------------------------
# 2) MODEL FORMULA (your pre-2020 spec)
#----------------------------------------------------------
model_formula <- services_qoq ~ s_L1 + infl_exp + 
  eer_qoq + pmdef_qoq + D_1991Q3

#----------------------------------------------------------
# 3) HELPERS (stable binding + dynamic-lag regressor builder)
#----------------------------------------------------------
# 0-row tibble with fixed schema (prevents bind_rows issues)
empty_fcst_row <- function() {
  tibble(
    origin_qtr = character(),
    horizon    = integer(),
    target_qtr = character(),
    y_hat      = numeric(),
    y_actual   = numeric(),
    error      = numeric()
  )
}

# Build regressor vector in the exact order of coef_names19.
# - y_dyn holds actuals through origin and forecasts after origin
# - exogenous values are taken from data (actuals) at target t_idx
build_x_vec <- function(t_idx, y_dyn, data, coef_namesd19) {
  x <- setNames(numeric(length(coef_namesd19)), coef_namesd19)
  
  if ("(Intercept)" %in% coef_namesd19) x["(Intercept)"] <- 1
  
  # dynamic core goods lags
  if ("s_L1" %in% coef_namesd19) x["s_L1"] <- if (t_idx - 1 >= 1) y_dyn[t_idx - 1] else NA_real_
  #if ("s_L3" %in% coef_names19) x["s_L3"] <- if (t_idx - 3 >= 1) y_dyn[t_idx - 3] else NA_real_
  
  # exogenous present in both model and data
  exog_in_model <- setdiff(coef_namesd19, c("(Intercept)", "s_L1"))#, "s_L3", "s_L2", "s_L4"))
  exog_in_data  <- intersect(exog_in_model, names(data))
  for (nm in exog_in_data) x[nm] <- data[[nm]][t_idx]
  
  # defensively replace NA with 0 (should rarely occur here)
  x[is.na(x)] <- 0
  x
} 

# Core routine: given (origin, coefficients), produce dynamic H-step forecast
dyn_forecast_origin <- function(origin_qtr, H, data, beta_vec, coef_namesd19) {
  data     <- data[order(data$date), ]
  dates    <- data$date
  y_actual <- data$services_qoq
  
  idx0 <- which(dates == origin_qtr)
  if (!length(idx0)) return(empty_fcst_row())
  
  # dynamic path: actuals through origin, then NA to be forecast
  y_dyn <- y_actual
  if (idx0 < length(y_dyn)) y_dyn[(idx0 + 1):length(y_dyn)] <- NA_real_
  
  # require actual exogenous at targets
  exog_in_model <- setdiff(coef_namesd19, c("(Intercept)", "s_L1"))#, "s_L3", "s_L2", "s_L4"))
  exog_in_data  <- intersect(exog_in_model, names(data))
  
  out <- vector("list", H)
  
  for (h in seq_len(H)) {
    t_idx <- idx0 + h
    if (t_idx > nrow(data)) break
    
    # strict rule: stop if any exogenous is missing at target
    if (length(exog_in_data)) {
      exog_vals <- unlist(data[t_idx, exog_in_data], use.names = FALSE)
      if (any(is.na(exog_vals))) break
    }
    
    x_vec <- build_x_vec(t_idx, y_dyn, data, coef_namesd19)
    y_hat <- sum(beta_vec * x_vec[names(beta_vec)])
    
    # feed forecast back into y_dyn to create forecasted lags for next steps
    y_dyn[t_idx] <- y_hat
    
    out[[h]] <- tibble(
      origin_qtr = as.character(origin_qtr),
      horizon    = as.integer(h),
      target_qtr = as.character(dates[t_idx]),
      y_hat      = as.numeric(y_hat),
      y_actual   = as.numeric(y_actual[t_idx]),
      error      = if (!is.na(y_actual[t_idx])) as.numeric(y_actual[t_idx] - y_hat) else NA_real_
    )
  }
  
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(empty_fcst_row())
  dplyr::bind_rows(out)
}

#----------------------------------------------------------
# 4) RE-ESTIMATE AT EACH ORIGIN (expanding window)
#     Estimation window: 1991Q3 .. origin_qtr
#----------------------------------------------------------
fit_and_forecast_at_origin <- function(origin_qtr, H = 12) {
  est_data <- subset(full_data, date >= as.yearqtr("1991 Q3") & date <= origin_qtr)
  
  # re-estimate same specification at this origin
  m <- lm(model_formula, data = est_data)
  beta_here <- coef(m)
  cn_here   <- names(beta_here)
  
  dyn_forecast_origin(
    origin_qtr = origin_qtr,
    H     = H,
    data       = full_data,
    beta_vec   = beta_here,
    coef_namesd19 = cn_here
  )
}

#----------------------------------------------------------
# 5) RUN FOR ALL ORIGINS: 2019Q1–2025Q2, H=12
#----------------------------------------------------------
origins <- seq(as.yearqtr("2019 Q4"), as.yearqtr("2025 Q2"), by = 0.25)
H       <- 12

fcst_eval_list <- purrr::map(origins, fit_and_forecast_at_origin)

fcst_sr19 <- dplyr::bind_rows(fcst_eval_list) %>%
  mutate(
    origin_qtr = zoo::as.yearqtr(origin_qtr),
    target_qtr = zoo::as.yearqtr(target_qtr)
  ) %>%
  arrange(origin_qtr, horizon)

# Inspect first rows
print(head(fcst_sr19, 20))


#======================================
#===== DATA FORMATTING FOR FE TOOLKIT
#======================================

#### format dataframe to use the forecast evaluation package
forecast_data_1 <- fcst_s_ols25 %>%
  mutate(vintage_date = as.character(origin_date),
         value = y_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Services inflation",
         source = "OLS-FixedCoeff25",
         date = as.character(target_date),
         .keep = "none")

forecast_data_2 <- fcst_s_ar25 %>%
  mutate(vintage_date = as.character(origin_date),
         value = y_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Services inflation",
         source = "AR(1)-FixedCoeff25",
         date = as.character(target_date),
         .keep = "none")

forecast_data_3<- fcst_s_ols19 %>%
  mutate(vintage_date = as.character(origin_date),
         value = y_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Services inflation",
         source = "OLS-FixedCoeff19",
         date = as.character(target_date),
         .keep = "none")

forecast_data_4<- fcst_s_ar19 %>%
  mutate(vintage_date = as.character(origin_date),
         value = y_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Services inflation",
         source = "AR(1)-FixedCoeff19",
         date = as.character(target_date),
         .keep = "none")

forecast_data_5 <- fcst_sr19 %>%
  mutate(vintage_date = as.character(origin_qtr),
         value = y_hat,
         frequency = "Q",
         forecast_horizon = horizon,
         variable = "Services inflation",
         source = "OLS-RollCoeff19-25",
         date = as.character(target_qtr),
         .keep = "none")

forecast_data = rbind(forecast_data_1, forecast_data_2, forecast_data_3, forecast_data_4, forecast_data_5)

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
  mutate(value = services_qoq,
         frequency = "Q",
         forecast_horizon = (date - vintage_date)*4,
         variable = "Services inflation",
         date = as.character(date),
         vintage_date = as.character(vintage_date),
         .keep = "none")

# saving data in parquet format
library(arrow)

#write_parquet(df_outturn, "N:/MPOD/Infrastructure Investment/02_Team_members/Carlos/3. CPI disaggregation in PT/github_project/CPI_disagg/data/outturn_data_s.parquet")
#write_parquet(forecast_data, "N:/MPOD/Infrastructure Investment/02_Team_members/Carlos/3. CPI disaggregation in PT/github_project/CPI_disagg/data/forecast_data_s.parquet")
# saving data in parquet format
library(arrow)

#dir.create("C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data", recursive = TRUE, showWarnings = FALSE)
write_parquet(df_outturn, "C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data/outturn_datacg.parquet")
write_parquet(forecast_data, "C:/Users/344792/Gokce/GIT PROJECTS/DisaggCPI/CPI-disaggregation-in-PT/data/forecast_datacg.parquet")

