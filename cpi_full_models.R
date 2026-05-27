### IMPORTING DATA from EXCEL ####
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)
library(quantmod)

cpi_tbl <- 
  readxl::read_excel("cpi_data.xlsx")
tibble(cpi_tbl)

cpi_tbl$Date <- as.Date(cpi_tbl$Date, format = "%Y-%m-%d")
str(cpi_tbl)

cpi_tbl <-
  cpi_tbl %>%
  rename(infl_exp = exp_cpi, capu_dat = capu_f) %>%
  mutate(u_rate = unemp*100, u_rate_f = unemp_f*100, pmdef = pmdef*100)

### 2. DATA TRANSFORMATION

## Selecting and transforming data on quarterly growth 
cpi_tbl_1 <- 
  cpi_tbl %>%
  select(-c(unemp, unemp_f)) %>%
  mutate(across(-c(Date, capu, capu_dat,
                  u_rate, u_rate_f,
                  ois_rate,
                  bank_rate,bank_rate_f,
                  infl_exp),
                ~ Delt(.)*100, # Delt is function to calculate qoq growth
                .names =("{.col}_qoq"))) %>%
  slice(-1)

str(cpi_tbl_1)  

### STATISTIC ANALYSIS ####

#install.packages(c("dynlm", "PerformanceAnalytics", "stats"))
#library(PerformanceAnalytics)

#install.packages(c("dynlm","ggplot2","Hmisc","psych","corrplot"))

library(dynlm)

library(stats)

library(ggplot2)

library(boedown)

library(corrplot)

library (zoo)

#selecting and transforming variables for model specification
cpi_tbl_2 <-
  cpi_tbl_1 %>%
  select(-matches("_f_qoq")) %>%
  select(Date,capu, 
         capu_dat,
         u_rate,
         ois_rate,
         bank_rate,
         infl_exp,
         matches("_qoq")) %>%
  slice_min(n = 140, order_by = Date)# I am selecting back data period

str(cpi_tbl_2)


### PLOTTING LINE CHARTS ####

# Ensuring date format

cpi_tbl_2$date <- as.Date(cpi_tbl_2$Date)

# Convert to long format
cpi_g1_series <- 
  cpi_tbl_2 %>%
  select(c(date,cpi_qoq,
           services_qoq,
         core_gds_qoq,
         food_at_qoq)) %>% 
  pivot_longer(cols = -date, names_to = "Series", values_to = "Value")

# Plotting first group of series 

ggplot(cpi_g1_series, aes(x = date, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Series, ncol = 2) +  # Adjust ncol for layout (e.g., 4 columns)
  labs(title = "Headline CPI and their major components",
       x = "Date", y = "Value") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))


# Convert to long format
cpi_g2_series <- 
  cpi_tbl_2 %>%
  select(c(date,
           wage_qoq,
           infl_exp,
           eer_qoq,
           pmdef_qoq)) %>% 
  pivot_longer(cols = -date, names_to = "Series", values_to = "Value")

# Plotting second group of series 

ggplot(cpi_g2_series, aes(x = date, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Series, ncol = 2) +  # Adjust ncol for layout (e.g., 4 columns)
  labs(title = "Potential explanatory variables",
       x = "Date", y = "Value") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))



### INTERTEMPORAL CROSS-CORRELATION ####
  
#install.packages(c("dynCorr", "zoo"))
library(dynCorr)

cpi_numeric <- 
  as.data.frame(lapply(cpi_tbl_2, function(x) as.numeric(as.character(x)))) %>%
  select(-c(Date, 
            date,
            cpi_a_qoq,
            food_qoq))

str(cpi_numeric)
'cpi_tbl_3 <-
  cpi_tbl_2 %>%
  select(-c(Date, 
            date,
            cpi_a_qoq,
            food_qoq))'
corr_cpi <- cor(cpi_numeric)

corrplot(corr_cpi, method = "circle")


# plotting series

'cpi_tbl_2 %>%
  ggplot(mapping = aes(x = services_qoq, y = infl_exp))+
  geom_point()+
  geom_smooth(method = "lm", color = "darkgreen") +  
  ggtitle(label = "CPI Services and inflation expectation", "1990-2025") +
  theme_light()

cpi_tbl_2 %>%
  ggplot(mapping = aes(x = services_qoq, y = wage_qoq))+
  geom_point()+
  geom_smooth(method = "lm", color = "darkgreen") +  
  ggtitle(label = "CPI Services and wages", "1990-2025") +
  theme_light()

cpi_tbl_2 %>%
  ggplot(mapping = aes(x = core_gds_qoq, y = cpif_qoq))+
  geom_point()+
  geom_smooth(method = "lm", color = "darkgreen") +  
  ggtitle(label = "UK Core good CPI and Global CPI", "1990-2025") +
  theme_light()

cpi_tbl_2 %>%
  ggplot(mapping = aes(x = food_at_qoq, y = eer_qoq))+
  geom_point()+
  geom_smooth(method = "lm", color = "darkgreen") +  
  ggtitle(label = "UK CPI food and Exchange rate", "1990-2025") +
  theme_light()

cpi_tbl_2 %>%
  ggplot(mapping = aes(x = core_gds_qoq, y = energy_qoq))+
  geom_point()+
  geom_smooth(method = "lm", color = "darkgreen") +  
  ggtitle(label = "UK CPI food and Exchange rate", "1990-2025") +
  theme_light()'

#### headline CPI
'CPI and energy

with(as.data.frame(cpi_numeric),
     ccf(cpi_qoq, energy_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#CPI and unemployment
with(as.data.frame(cpi_numeric),
     ccf(cpi_qoq, unemp, 
         lag.max = 18, 
         plot = TRUE, 
         na.action = na.omit))

#CPI and capu
with(as.data.frame(cpi_numeric),
     ccf(cpi_qoq, capu, 
         lag.max = 18, 
         plot = TRUE, 
         na.action = na.omit))

#CPI and World CPI
with(as.data.frame(cpi_numeric),
     ccf(cpi_qoq, cpif_qoq, 
         lag.max = 18, 
         plot = TRUE, 
         na.action = na.omit))

CPI and inflation expectation
with(as.data.frame(cpi_numeric),
     ccf(cpi_qoq, infl_exp, 
         lag.max = 18, 
         plot = TRUE, 
         na.action = na.omit))

CPI and wage
with(as.data.frame(cpi_numeric),
     ccf(cpi_qoq, wage_qoq, 
         lag.max = 18, 
         plot = TRUE, 
         na.action = na.omit))'

'#### Core goods
#Core goods  and capu
with(as.data.frame(cpi_numeric),
     ccf(cpi_qoq, capu, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Core goods  and energy
with(as.data.frame(cpi_numeric),
     ccf(core_gds_qoq, energy_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Core goods  and inflation expectation
with(as.data.frame(cpi_numeric),
     ccf(core_gds_qoq, infl_exp, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Core goods  and wage
with(as.data.frame(cpi_numeric),
     ccf(core_gds_qoq, wage_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Core goods  and global CPI
with(as.data.frame(cpi_numeric),
     ccf(core_gds_qoq, cpif_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Core goods  and import prices
with(as.data.frame(cpi_numeric),
     ccf(core_gds_qoq, pmdef_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#### Services
#Services  and bank rate
with(as.data.frame(cpi_numeric),
     ccf(services_qoq, bank_rate, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Services  and inflation expectation
with(as.data.frame(cpi_numeric),
     ccf(services_qoq, infl_exp, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Services  and wages
with(as.data.frame(cpi_numeric),
     ccf(services_qoq, wage_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Services  and global CPI
with(as.data.frame(cpi_numeric),
     ccf(services_qoq, cpif_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#### Food
#Food  and inflation expectation
with(as.data.frame(cpi_numeric),
     ccf(food_at_qoq, infl_exp, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Food  and energy
with(as.data.frame(cpi_numeric),
     ccf(food_at_qoq, energy_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Food  and Exchange rate
with(as.data.frame(cpi_numeric),
     ccf(food_at_qoq, eer_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Food  and wage
with(as.data.frame(cpi_numeric),
     ccf(food_at_qoq, wage_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Food  and global CPI
with(as.data.frame(cpi_numeric),
     ccf(food_at_qoq, cpif_qoq, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Food  and Unemployment
with(as.data.frame(cpi_numeric),
     ccf(food_at_qoq, u_rate, 
         lag.max = 12, 
         plot = TRUE, 
         na.action = na.omit))

#Food  and import price deflator
with(as.data.frame(cpi_numeric),
     ccf(food_at_qoq, pmdef_qoq, 
         lag.max = 8, 
         plot = TRUE, 
         na.action = na.omit))'


### ESTIMATING MODELS ####

#Installing TS packages 

'install.packages(c("lmtest", 
                   "TTR",
                   "sandwich",
                   "car",
                   "tseries",
                   "urca",
                   "ecm",
                   "forecast",
                   "modelr",
                   "astsa",
                   "tsbox",
                   "vars"))'

# Bringing packages
library(lmtest)
library(sandwich)
library(car)
library(tseries)
library(urca)
library(ecm)
library(forecast)
library(modelr)
library(astsa)
library(vars)

# converting data to estimate the models

str(cpi_numeric)
df_cpi <-
  cpi_numeric %>% 
  as_tibble() %>%
  tibble(date = seq.Date(from = as.Date("1990-09-30"),
                         to   = as.Date("2025-06-30"),
                         by   = "3 months"))

#### --MODELLING CORE GOODS-- ####

## CORE GOODS - OLS MODEL 1 ####

cg_m1 <- lm(core_gds_qoq ~ 
                   infl_exp + 
                   wage_qoq, 
                   data = df_cpi)

summary(cg_m1)

durbinWatsonTest(cg_m1) 
# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(cg_m1)

bgtest(cg_m1, order = 1) 

# Breush-Godfrey test. 
#Null hypothesis 
#(H₀): No serial correlation (errors are independent)
# p > 0.05 → Fail to reject H₀ → No evidence of autocorrelation.

resid_cg_m1 <- resid(cg_m1)
lag.plot(resid_cg_m1, diag.col = "forest green"
         , main = "Lag Scatter Plot - CORE GOODS MODEL 1")

df_cpi <- add_residuals(df_cpi, cg_m1, var = "resid_cg_m1") 



## CORE GOODS - OLS MODEL 2 = MODEL 1 + LAGS ####

# Creating  LAGS 
df_cpi <- 
  df_cpi %>%
  mutate(core_gds_L1 = lag(core_gds_qoq, 1),
         infl_exp_L1 = lag(infl_exp, 1),
         cpif_L1 = lag(cpif_qoq, 1),
         pmdef_L1 = lag(pmdef_qoq, 1),
         pmdef_L2 = lag(pmdef_qoq, 2),
         pmdef_L3 = lag(pmdef_qoq, 3),
         wage_L1 = lag(wage_qoq,1))

df_cpi <-na.omit(df_cpi)

# Specifying MODEL 2

cg_m2 <- lm(core_gds_qoq ~ -1 +
              core_gds_L1 +
             infl_exp +
              wage_L1+
              pmdef_L3, 
              data = df_cpi)

summary(cg_m2)

durbinWatsonTest(cg_m2)

# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(cg_m2)

bgtest(cg_m2, order = 1) 

# Plotting and saving residuals

resid_cg_m2 <- resid(cg_m2)
lag.plot(resid_cg_m2, diag.col = "forest green"
         , main = "Lag Scatter Plot - CG MODEL 2")
df_cpi <- add_residuals(df_cpi, cg_m2, var = "resid_cg_m2") 

'par(mfrow = c(2, 2))  # Arrange plots in 2x2 grid
plot(CG_model_2)



# Basic residual plot
plot(CG_model_4, which = 1)   # Residuals vs Fitted values'



'library(ggplot2)

# Create a data frame of fitted values and residuals
res_df_2 <- data.frame(
  fitted = fitted(CG_model_2),
  residuals = resid(CG_model_2)
)

# Plot residuals vs fitted
ggplot(res_df_2, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "green") +
  labs(title = "Residuals vs Fitted 2", x = "Fitted values", y = "Residuals") +
  theme_minimal()'

## CORE GOODS - OLS MODEL 3 - MOD2 + DUMMIES ####

## IDENTIFYING RESIDUAL OUTLIERS

#Approach_1

# Compute standardised residuals
std_resid_cg_m2 <- rstandard(cg_m2)
# Flag outliers (e.g., |residual| > 2)
outliers_cg_m2 <- which(abs(std_resid_cg_m2) > 2)
df_cpi[outliers_cg_m2,"core_gds_qoq"]
df_cpi[outliers_cg_m2,"date"]

# Approach_2

cooks_cg_m2 <- cooks.distance(cg_m2)
# Flag influential observations (rule of thumb: > 4/n)
threshold_cg_m2 <- 4 / nrow(df_cpi)
which(cooks_cg_m2 > threshold_cg_m2)
      
# Approach_3

outlierTest(cg_m2)  # Bonferroni p-values for outliers

## CREATING DUMMIES 

# 1. Dummies for energy price shocks and policy tightening under ERM -1991Q2
# 2. Dummies for GFC 2009Q3-2009Q4
# 3. Dummies for POST_COVID and Russia-Ukraine war 2021Q2-2022Q2

df_cpi <- 
  df_cpi %>%
  #mutate(D_shock_1991 = ifelse(date >= as.Date("1991-06-30") & date <= as.Date("1991-06-30"), 1, 0)) %>%
  mutate(D_GFC_2008 = ifelse(date >= as.Date("2009-06-30") & date <= as.Date("2009-09-30"), 1, 0)) %>%
  mutate(D_shock_2021 = ifelse(date >= as.Date("2021-06-30") & date <= as.Date("2022-03-30"), 1, 0)) %>%
  mutate(D_shock_pers_2023 = ifelse(date >= as.Date("2023-06-30") & date <= as.Date("2023-06-30"), 1, 0))

str(df_cpi)


# MODEL 3 SPECIFICATION

cg_m3 <- lm(core_gds_qoq ~ 
                   core_gds_L1 + 
                   infl_exp +
                   pmdef_L3 +      
                   wage_L1+
              D_GFC_2008+
              D_shock_2021,
                 data = df_cpi)

summary(cg_m3)

durbinWatsonTest(cg_m3)

# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(cg_m3)

bgtest(cg_m3, order = 1) 

outlierTest(cg_m3)  # Bonferroni p-values for outliers

# Plotting and saving residuals

resid_cg_m3 <- resid(cg_m3)
lag.plot(resid_cg_m3, diag.col = "forest green"
         , main = "Lag Scatter Plot - CORE GOODS MODEL 3")
df_cpi <- add_residuals(df_cpi, 
                       cg_m3, 
                       var = "resid_cg_m3") 


## CORE GOODS - MODEL 3 PRECOVID PERFORMANCE

df_cpi_pc <- 
  subset(df_cpi, date <= "2019-12-31")

str(df_cpi_pc)


#MODEL ESPECIFICATION

cg_m3_pc <- lm(core_gds_qoq ~               core_gds_L1 + 
                   infl_exp +
                   pmdef_L3 +
                 wage_L1 +
                 D_GFC_2008,
                 data = df_cpi_pc)

summary(cg_m3_pc)

durbinWatsonTest(cg_m3_pc)

# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(cg_m3_pc)

bgtest(cg_m3_pc , order = 1) 

outlierTest(cg_m3_pc)  # Bonferroni p-values for outliers

# Plotting and saving residuals

resid_cg_m3_pc <- resid(cg_m3_pc)
lag.plot(resid_cg_m3_pc, diag.col = "forest green"
         , main = "Lag Scatter Plot - CORE GOODS MODEL 3 PRE COVID")
lag.plot(resid_cg_m3_pc, 1)
df_cpi_pc <- add_residuals(df_cpi_pc, 
                           cg_m3_pc, 
                       var = "resid_cg_m3_pc") 

#

#### --MODELLING SERVICES -- ####


## SERVICES - OLS MODEL 1 ####
str(df_cpi)

serv_m1 <- lm(services_qoq ~ 
                infl_exp + 
                wage_qoq +
                bank_rate,
              data = df_cpi)

summary(serv_m1)

durbinWatsonTest(serv_m1) 
# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(serv_m1)

bgtest(serv_m1, order = 1) 

# Breush-Godfrey test. 
#Null hypothesis 
#(H₀): No serial correlation (errors are independent)
# p > 0.05 → Fail to reject H₀ → No evidence of autocorrelation.

resid_serv_m1 <- resid(serv_m1)
lag.plot(resid_serv_m1, diag.col = "forest green"
         , main = "Lag Scatter Plot - SERVICES MODEL 1")

df_cpi <- add_residuals(df_cpi,serv_m1, var = "resid_serv_m1") 


## SERVICES - OLS MODEL 2 = MODEL 1 + LAGS ####

# Creating  LAGS to include in MODEL 1

df_cpi <- 
  df_cpi %>%
  mutate(services_L1 = lag(services_qoq, 1),
         infl_exp_L1 = lag(infl_exp, 1),
         wage_L1 = lag(wage_qoq,1),
         bank_rate_L1 = lag(bank_rate, 1),
         ois_rate_L1 = lag(ois_rate, 1))

df_cpi <-na.omit(df_cpi)

serv_m2 <- lm(services_qoq ~
                services_L1 +
                infl_exp_L1 + 
                wage_qoq +
                bank_rate_L1,            , 
              data = df_cpi)

summary(serv_m2)

durbinWatsonTest(serv_m2) 
# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(serv_m2)

bgtest(serv_m2, order = 1) 

# Breush-Godfrey test. 
#Null hypothesis 
#(H₀): No serial correlation (errors are independent)
# p > 0.05 → Fail to reject H₀ → No evidence of autocorrelation.

resid_serv_m2 <- resid(serv_m2)
lag.plot(resid_serv_m2, diag.col = "forest green"
         , main = "Lag Scatter Plot - SERVICES MODEL 2")

df_cpi <- add_residuals(df_cpi,serv_m2, var = "resid_serv_m2") 



## SERVICES - OLS MODEL 3 - MOD2 + DUMMIES ####

## Identifying outliers - approach_1

# Compute standardised residuals
std_resid_serv_m2 <- rstandard(serv_m2)
# Flag outliers (e.g., |residual| > 2)
outliers_serv_m2 <- which(abs(std_resid_serv_m2) > 2)
df_cpi[outliers_serv_m2,"services_qoq"]
df_cpi[outliers_serv_m2,"date"]


# Identifying outliers - approach_2

cooks_serv_m2 <- cooks.distance(serv_m2)
# Flag influential observations (rule of thumb: > 4/n)
threshold_serv_m2 <- 4 / nrow(df_cpi)
which(cooks_serv_m2  > threshold_serv_m2)


# Identifying outliers - approach_3

outlierTest(serv_m2)  # Bonferroni p-values for outliers

## Creating dummies 

# 1. Dummies for energy price shocks and policy tightening under ERM -1991Q2
# 2. Dummies for GFC 2009Q3-2009Q4
# 3. Dummies for POST_COVID and Russia-Ukraine war 2021Q2-2022Q2

df_cpi <- 
  df_cpi %>%
 mutate(en_shock_1991_s = ifelse(date >= as.Date("1991-06-30") & date <= as.Date("1991-09-30"), 1, 0)) %>%
  #mutate(GFC_2008_09 = ifelse(date >= as.Date("2009-06-30") & date <= as.Date("2009-09-30"), 1, 0)) %>%
  mutate(covid_shock_2020 = ifelse(date >= as.Date("2020-09-30") & date <= as.Date("2020-09-30"), 1, 0))
  #mutate(en_shock_2021 = ifelse(date >= as.Date("2021-06-30") & date <= as.Date("2022-03-30"), 1, 0)) %>%
  #mutate(en_shock_pers_2023 = ifelse(date >= as.Date("2023-06-30") & date <= as.Date("2023-06-30"), 1, 0))


str(df_cpi)


# MODEL 3 SPECIFICATION

serv_m3 <- lm(services_qoq ~
                services_L1 +
                infl_exp_L1 + 
                wage_qoq +
                ois_rate_L1+
                en_shock_1991_s+
                covid_shock_2020,
              data = df_cpi)

summary(serv_m3)

durbinWatsonTest(serv_m3)

# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(serv_m3)

bgtest(serv_m3, order = 1) 

outlierTest(serv_m3)  # Bonferroni p-values for outliers

# Plotting and saving residuals

resid_serv_m3 <- resid(serv_m3)
lag.plot(resid_serv_m3, diag.col = "forest green"
         , main = "Lag Scatter Plot - SERVICES MODEL 3")
df_cpi <- add_residuals(df_cpi, 
                       serv_m3, 
                       var = "resid_serv_m3") 


## SERVICES MODEL 3 PRECOVID PERFORMANCE

df_cpi_pc <- 
  df_cpi_pc %>%
  mutate(infl_exp_L1 = lag(infl_exp, 1),
         services_L1 = lag(services_qoq, 1),
         bank_rate_L1 = lag(bank_rate, 1),
         ois_rate_L1 = lag(ois_rate, 1))

serv_m3_pc <- lm(services_qoq ~
                services_L1 +
                infl_exp_L1 + 
                wage_qoq +
                bank_rate_L1,
                data = df_cpi_pc)

summary(serv_m3_pc)

# test of residuals

durbinWatsonTest(serv_m3_pc)

# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(serv_m3_pc)

bgtest(serv_m3_pc, order = 1) 

outlierTest(serv_m3_pc)  # Bonferroni p-values for outliers

# Plotting and saving residuals

resid_serv_m3_pc <- resid(serv_m3_pc)
lag.plot(resid_serv_m3_pc, diag.col = "forest green"
         , main = "Lag Scatter Plot - SERVICES MODEL 3 PRECOVID")
df_cpi_pc <- add_residuals(df_cpi_pc, 
                        serv_m3_pc, 
                        var = "resid_serv_m3_pc") 



#### --MODELLING FOOD-- ####

## FOOD - OLS MODEL 1 ####

CG_model_1 <- lm(core_gds_qoq ~ 
                   infl_exp + 
                   wage_qoq, 
                 data = df_cpi)

summary(CG_model_1)

durbinWatsonTest(CG_model_1) 
# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(CG_model_1)

bgtest(CG_model_1, order = 1) 

# Breush-Godfrey test. 
#Null hypothesis 
#(H₀): No serial correlation (errors are independent)
# p > 0.05 → Fail to reject H₀ → No evidence of autocorrelation.

residual_m1 <- resid(CG_model_1)
lag.plot(residual_m1, diag.col = "forest green"
         , main = "Lag Scatter Plot - MODEL 1")

df_m1 <- add_residuals(df_cpi, CG_model_1, var = "resid") 


str(df_cpi)

food_m1 <- lm(food_at_qoq ~ 
                infl_exp + 
                wage_qoq +
                eer_qoq,
              data = df_cpi)

summary(food_m1)

durbinWatsonTest(food_m1) 
# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(food_m1)

bgtest(food_m1, order = 2) 

# Breush-Godfrey test. 
#Null hypothesis 
#(H₀): No serial correlation (errors are independent)
# p > 0.05 → Fail to reject H₀ → No evidence of autocorrelation.

resid_food_m1 <- resid(food_m1)
lag.plot(resid_food_m1, diag.col = "forest green"
         , main = "Lag Scatter Plot - FOOD MODEL 1")

df_cpi <- add_residuals(df_cpi,food_m1, var = "resid_food_m1") 


## FOOD - OLS MODEL 2 = MODEL 1 + LAGS ####

# Creating  LAGS 

df_cpi <- 
  df_cpi %>%
  mutate(food_L1 = lag(food_at_qoq, 1),
         food_L2 = lag(food_at_qoq, 2),
         infl_exp_L1 = lag(infl_exp, 1),
         cpif_L1 = lag(cpif_qoq, 1),
         wage_L1 = lag(wage_qoq,1),
         wage_L2 = lag(wage_qoq,2),
         pmdef_L1 = lag(pmdef_qoq,1),
         pmdef_L2 = lag(pmdef_qoq,2),
         eer_L1 = lag(eer_qoq,1),
         energy_L1 = lag(energy_qoq,1),
         energy_L2 = lag(energy_qoq,2))

df_cpi <-na.omit(df_cpi)

# Specifying MODEL 2

food_m2 <- lm(food_at_qoq ~
                infl_exp +
                eer_L1 +
                pmdef_L2,
                data = df_cpi)

summary(food_m2)


durbinWatsonTest(food_m2) 
# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(food_m2)

bgtest(food_m2, order = 1) 

# Breush-Godfrey test. 
#Null hypothesis 
#(H₀): No serial correlation (errors are independent)
# p > 0.05 → Fail to reject H₀ → No evidence of autocorrelation.

resid_food_m2 <- resid(food_m2)
lag.plot(resid_food_m2, diag.col = "forest green"
         , main = "Lag Scatter Plot - FOOD MODEL 2")

df_cpi <- add_residuals(df_cpi,food_m2, var = "resid_food_m2")


## FOOD - OLS MODEL 3 - MOD2 + DUMMIES ####

# IDENTIFYING OUTLIERS IN RESIDUALS

## Approach_1

# Compute standardised residuals
std_resid_food_m2 <- rstandard(food_m2)
# Flag outliers (e.g., |residual| > 2)
outliers_food_m2 <- which(abs(std_resid_food_m2) > 2)
df_cpi[outliers_food_m2,"food_at_qoq"]
df_cpi[outliers_food_m2,"date"]


# Approach_2

cooks_food_m2 <- cooks.distance(food_m2)
# Flag influential observations (rule of thumb: > 4/n)
threshold_food_m2 <- 4 / nrow(df_cpi)
which(cooks_food_m2 > threshold_food_m2)


# Approach_3

outlierTest(food_m2)  # Bonferroni p-values for outliers

## Creating dummies 

# 1. Dummies for energy price shocks and policy tightening under ERM -1991Q2
# 2. Dummies for GFC 2009Q3-2009Q4
# 3. Dummies for POST_COVID and Russia-Ukraine war 2021Q2-2022Q2

df_cpi <- 
  df_cpi %>%
  #mutate(en_shock_1991_f = ifelse(date >= as.Date("1991-06-30") & date <= as.Date("1991-12-30"), 1, 0)) %>%
  mutate(shock_1992_f = ifelse(date >= as.Date("1992-09-30") & date <= as.Date("1992-09-30"), 1, 0)) %>%
  mutate(GFC_2008_f = ifelse(date >= as.Date("2008-06-30") & date <= as.Date("2008-06-30"), 1, 0)) %>%
  #mutate(covid_shock_2021_f = ifelse(date >= as.Date("2021-06-30") & date <= as.Date("2021-06-30"), 1, 0)) %>%
  mutate(en_shock_2023_f = ifelse(date >= as.Date("2023-06-30") & date <= as.Date("2023-06-30"), 1, 0))

str(df_cpi)

# MODEL 3 SPECIFICATION

food_m3 <- lm(food_at_qoq ~ 
                infl_exp + 
                eer_L1 +
                pmdef_L2+
                shock_1992_f +
                GFC_2008_f +
                en_shock_2023_f,
                data = df_cpi)

summary(food_m3)

durbinWatsonTest(food_m3)

# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(food_m3)

bgtest(food_m3, order = 1) 

outlierTest(food_m3)  # Bonferroni p-values for outliers

# Plotting and saving residuals

resid_food_m3 <- resid(food_m3)
lag.plot(resid_food_m3, diag.col = "forest green"
         , main = "Lag Scatter Plot - FOOD MODEL 3")
df_cpi <- add_residuals(df_cpi, 
                        food_m3, 
                        var = "resid_food_m3") 


## MODEL 3 PRECOVID PERFORMANCE
df_cpi_pc <- 
  subset(df_cpi, date <= "2019-12-31")

food_m3_pc <- lm(food_at_qoq ~ 
                infl_exp + 
                  eer_L1 +
                  pmdef_L2+
                shock_1992_f +
                GFC_2008_f,
              data = df_cpi_pc)

summary(food_m3_pc)

durbinWatsonTest(food_m3_pc)

# Rule of Thumb

# 0 to 1.5 → Strong positive autocorrelation.
#1.5 to 2.5 → Little to no autocorrelation (ideal range).
#2.5 to 4 → Strong negative autocorrelation

dwtest(food_m3_pc)

bgtest(food_m3_pc, order = 1) 

outlierTest(food_m3_pc)  # Bonferroni p-values for outliers

# Plotting and saving residuals

resid_food_m3_pc <- resid(food_m3_pc)
lag.plot(resid_food_m3_pc, diag.col = "forest green"
         , main = "Lag Scatter Plot - FOOD MODEL 3 PRECOVID")
df_cpi <- add_residuals(df_cpi, 
                        food_m3_pc, 
                        var = "resid_food_m3_pc") 




### ERROR CORRECTION MODEL - SERVICES AND FOOD ####

#CHECKING CONDITIONS

#CPI in levels
df_cpi_level <- 
  cpi_tbl %>%
  dplyr::select(Date, cpi, core_gds, energy, services, food_at, ois_rate, bank_rate, infl_exp, wage, pmdef, eer, cpif) %>%
  slice_min(n = 141, order_by = Date)# I am selecting back data period

str(df_cpi_level)

'#Plotting line charts

cpi_g1_level<- 
  df_cpi_level %>%
  dplyr::select(c(Date,cpi,
                  services,
                  core_gds,
                  food_at)) %>% 
  pivot_longer(cols = -Date, names_to = "Series", values_to = "Value")

# Plotting first group of series 

ggplot(cpi_g1_level, aes(x = Date, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Series, ncol = 2) +  # Adjust ncol for layout (e.g., 4 columns)
  labs(title = "Headline CPI and their major components - levels",
       x = "Date", y = "Value") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))'


#CPI in log-levels

df_cpi_log <-
  df_cpi_level%>%
  mutate(across(c(cpi, core_gds, 
                energy, services, food_at, 
                ois_rate, 
                infl_exp, wage,
                pmdef, eer, cpif),
              ~ log(.), # log is function to calculate natural logarithm
              .names =("ln_{.col}")))

#Plotting line charts

cpi_g1_log<- 
  df_cpi_log %>%
  dplyr::select(c(Date,ln_cpi,
           ln_services,
           ln_core_gds,
           ln_food_at)) %>% 
  pivot_longer(cols = -Date, names_to = "Series", values_to = "Value")

# Plotting first group of series 

ggplot(cpi_g1_log, aes(x = Date, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Series, ncol = 2) +  # Adjust ncol for layout (e.g., 4 columns)
  labs(title = "Headline CPI and their major components - log levels",
       x = "Date", y = "Value") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))




# Transforming data to one period percentage Change

df_cpi_percent <-
  df_cpi_level %>%
  mutate(across(c(cpi, core_gds, 
                  energy, services, food_at, wage, 
                  pmdef, eer, cpif),
                ~ Delt(., type = "log")*100, # percent change using log diff
                .names =("{.col}_qoq")))%>%
  slice(-1)

# Creating lags
df_cpi_percent <- 
  df_cpi_percent %>%
  mutate(core_gds_L1 = lag(core_gds_qoq, 1),
         services_L1 = lag(services_qoq, 1),
         food_L1 = lag(food_at_qoq, 1),
         infl_exp_L1 = lag(infl_exp, 1),
         wage_L1 = lag(wage_qoq,1),
         pmdef_L1 = lag(pmdef_qoq, 1),
         pmdef_L2 = lag(pmdef_qoq, 2),
         pmdef_L3 = lag(pmdef_qoq, 3))

df_cpi_percent <-na.omit(df_cpi_percent)

# creating dummies

#Changing date format before creating dummies

df_cpi_tbl <- 
  df_cpi_percent %>% 
  as_tibble() %>%
  tibble(date = seq.Date(from = as.Date("1991-06-30"),
                         to   = as.Date("2025-06-30"),
                         by   = "3 months"))

df_cpi_tbl <-
  df_cpi_tbl %>%
  #mutate(D_shock_1991 = ifelse(date >= as.Date("1991-06-30") & date <= as.Date("1991-06-30"), 1, 0)) %>%
  mutate(D_GFC_2008 = ifelse(date >= as.Date("2009-06-30") & date <= as.Date("2009-09-30"), 1, 0)) %>%
  mutate(D_shock_2021 = ifelse(date >= as.Date("2021-06-30") & date <= as.Date("2022-03-30"), 1, 0)) %>%
  mutate(D_shock_pers_2023 = ifelse(date >= as.Date("2023-06-30") & date <= as.Date("2023-06-30"), 1, 0))


#Splitting sample: Training / Test

t0 <- which(df_cpi_tbl$Date == as.Date("2019-12-31"))

train <- df_cpi_percent[1:t0, ]

#Checking partial autocorrelation

pacf(df_cpi_log$ln_services)
pacf(df_cpi_log$ln_food_at)
pacf(df_cpi_log$ln_core_gds)

#Checking unit root

adf.test(df_cpi_log$ln_services, k =4)
adf.test(df_cpi_log$ln_food_at, k =4)
adf.test(df_cpi_log$ln_core_gds, k =4)
adf.test(df_cpi_log$ln_wage, k =4)

# Cointegration test
cpi_coint <- 
  df_cpi_log %>%
  dplyr::select(ln_services, ln_food_at, ln_wage, ln_eer, infl_exp)

ca.po(cpi_coint, demean = "trend", type = "Pz", lag = "long")

summary(coint_test)

view(coint_test)

coint_test@testreg

# ECM specification

xeq <- df %>% 
  dplyr::select(services,food_at, exp_infl, wage,cpif) %>%
  as.data.frame(xeq)

xst <- df %>% 
  dplyr::select(services,food_at, exp_infl, wage,cpif) %>%
  as.data.frame(xtr)

y <- df %>% 
  dplyr::select(services) %>%
  as.data.frame(y)  

serv_ecm1 <- ecm(y, xeq, xtr, includeIntercept = TRUE)


# ECM - testing results
sresid <- studres(serv_ecm1)
hist(sresid, freq = FALSE,
     main = "Distribution of Studentized Residuals")
ncvTest(serv_ecm1)
spreadlevelPlot(serv_ecm1)
durbinWatsonTest(serv_ecm1)


