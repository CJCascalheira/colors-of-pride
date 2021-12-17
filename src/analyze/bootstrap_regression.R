# Dependencies
library(psych)
library(car)
library(MASS)
library(KScorrect)
library(tidyverse)
library(rio)
library(sjPlot)
library(boot)

# Remove scientific notation
options(scipen = 999)

# Reproducibility
set.seed(1234567)

# Import - frequent problems with polydrug use
freq_polydrug_use <- read_csv("data/cleaned/freq_polydrug_use.csv")

# Import data to reduce size of raw data frame
scop_a1 <- read_csv("data/cleaned/cleaned_eds_orientation.csv")
scop_a2 <- read_csv("data/cleaned/cleaned_eds_combined.csv")

# Import - numeric annual income
scop_b <- import("data/raw/scop_final.sav") %>%
  as_tibble() %>%
  # Drop old record ID
  select(-record_id) %>%
  # Create new numeric record ID
  mutate(record_id = 1:nrow(.)) %>%
  # Move record ID to the front of the data frame
  select(record_id, everything()) %>%
  # Recode drugs variable since this data is raw
  mutate(across(drugs1:drugs10, ~ recode(., `1` = 6, `2` = 5, `3` = 4, 
                                         `4` = 3, `5` = 2, `6` = 1))) %>%
  # Reduce size to fit data frame for ONLY sexual orientation and gender identity
  filter(record_id %in% scop_a1$record_id)

# Import - numeric annual income
scop_c <- import("data/raw/scop_final.sav") %>%
  as_tibble() %>%
  # Drop old record ID
  select(-record_id) %>%
  # Create new numeric record ID
  mutate(record_id = 1:nrow(.)) %>%
  # Move record ID to the front of the data frame
  select(record_id, everything()) %>%
  # Reduce size to fit combined EDS data frame
  filter(record_id %in% scop_a2$record_id)

# Import data - sexual orientation EDS only
scop_orientation <- scop_a1 %>%
  mutate(
    # Numeric annual income
    income_ctrl = scop_b$annual_income,
    # 1 = Non-White, 0 = White
    race_ctrl = recode(race, "non-lat white" = 0, .default = 1),
    # 1 = yes, 0 = no
    hiv_ctrl = recode(hiv_result, "yes" = 1, .default = 0)
  ) %>%
  # Add variable that is better approximation of polydrug use
  left_join(freq_polydrug_use, by = "record_id") %>%
  filter(!is.na(freq_polydrug_use))

# Import data - gender identity EDS only
scop_gender <- read_csv("data/cleaned/cleaned_eds_gender.csv") %>%
  mutate(
    # Numeric annual income
    income_ctrl = scop_b$annual_income,
    # 1 = Non-White, 0 = White
    race_ctrl = recode(race, "non-lat white" = 0, .default = 1),
    # 1 = yes, 0 = no
    hiv_ctrl = recode(hiv_result, "yes" = 1, .default = 0)
  ) %>%
  # Add variable that is better approximation of polydrug use
  left_join(freq_polydrug_use, by = "record_id") %>%
  filter(!is.na(freq_polydrug_use))

# Import data - both gender identity AND sexual orientation EDS
scop_combined <- scop_a2 %>%
  mutate(
    # Numeric annual income
    income_ctrl = scop_c$annual_income,
    # 1 = Non-White, 0 = White
    race_ctrl = recode(race, "non-lat white" = 0, .default = 1),
    # 1 = yes, 0 = no
    hiv_ctrl = recode(hiv_result, "yes" = 1, .default = 0)
  ) %>%
  # Add variable that is better approximation of polydrug use
  left_join(freq_polydrug_use, by = "record_id") %>%
  filter(!is.na(freq_polydrug_use))

# POWER ANALYSIS INFO FOR G*POWER -----------------------------------------

# F test - R^2 deviation from 0

# Effect size of Model 4 (see Cohen, 1988)
(.439) / (1 - .439)

# Alpha
0.05

# Sample size
nrow(scop_combined)

# Number of predictors
11

# PRE-PROCESS -------------------------------------------------------------

# Initial sizes
nrow(scop_orientation)
nrow(scop_gender)
nrow(scop_combined)

# Scale all variables to return standardized coefficients
scop_1 <- scop_combined %>%
  select(freq_polydrug_use, race_ctrl, age, income_ctrl, hiv_ctrl, food_ran_out, homeless_exp, unwanted, 
         eds_gender_total, eds_orientation_total, intracomm_eds, mspss_total, total_coping) %>%
  lapply(scale) %>%
  as.data.frame()

# FUNCTIONS ---------------------------------------------------------------

# To interpret the output
help(summary.boot)

# Function to calculate R^2 
# https://www.statmethods.net/advstats/bootstrapping.html
rsq_function <- function(formula, data, indices) {
  # Allows boot to select sample
  d <- data[indices, ]
  
  # Fit regression model
  fit <- lm(formula, data = d)
  
  # Return R-squared of model
  return(summary(fit)$r.square)
}

# Function to calculate AIC
get_aic <- function(formula, data, indices) {
  # Allows boot to select sample
  d <- data[indices, ]
  
  # Fit regression model
  fit <- lm(formula, data = d)
  
  # Calculate AIC
  aic <- nrow(data) * log((sum(fit$residuals^2)) / (nrow(data))) + (2 * length(fit$coefficients))
  
  # Return the AIC
  return(aic)
}

# Function to calculate F statistic for change in R^2
# Cohen et al. (2003, p. 171)
# https://www.danielsoper.com/statcalc/formulas.aspx?id=14
get_f_value <- function(r2_a, r2_b, model_a, model_b) {
  # Specify n
  n <- length(model_a$fitted.values)
  
  # Count the number of predictors
  k_a <- length(model_a$coefficients)
  k_b <- length(model_b$coefficients)
  
  # Calculate the effect size, f^2, from equation 5.5.1
  f_sqrd <- (r2_b - r2_a) / (1 - r2_b)
  
  # Calculate degrees of freedom term from equation 5.5.2A
  df_term <- (n - k_a - k_b - 1) / (k_b)
  
  # Calculate the F-value
  f_value <- f_sqrd * df_term
  
  # Return the F-value
  return(f_value)
}

# MODEL 1 - DEMOGRAPHIC COVARIATES ----------------------------------------

# Model 1
model_1 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl, data = scop_combined)

# Bootstrap estimation
model_1_boot <- Boot(model_1, R = 5000, method = "case")

# Summarize the model
model_1_sumry <- summary(model_1_boot)
model_1_sumry

# Confidence intervals for model estimates
confint(model_1_boot, level = .95, type = "norm")

# Get t-statistics for each coefficient
model_1_tstat <- data.frame(
  # Pull the coefficients and the standard errors
  coeff = model_1_sumry$bootMed, 
  se = model_1_sumry$bootSE
  ) %>%
  # Calculate the standard t-statistic
  mutate(t_stat = coeff / se) %>%
  # Calculate the p-value of the t-statistic
  mutate(t_p_value = 2 * pt(q = abs(t_stat), df = (nrow(scop_combined) - 1), lower.tail = FALSE))
model_1_tstat

# Get R^2
model_1_r2 <- boot(data = scop_combined, statistic = rsq_function, R = 5000, 
     formula = freq_polydrug_use ~ race_ctrl + age + income_ctrl)
model_1_r2
boot.ci(model_1_r2, type = "bca")

# Get AIC
model_1_aic <- boot(data = scop_combined, statistic = get_aic, R = 5000, 
                   formula = freq_polydrug_use ~ race_ctrl + age + income_ctrl)
model_1_aic

# MODEL 2 - SYNDEMIC COVARIATES -------------------------------------------

# Model 2
model_2 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out, data = scop_combined)

# Bootstrap estimation
model_2_boot <- Boot(model_2, R = 5000, method = "case")

# Summarize the model
model_2_sumry <- summary(model_2_boot)
model_2_sumry

# Confidence intervals for model estimates
confint(model_2_boot, level = .95, type = "norm")

# Get t-statistics for each coefficient
model_2_tstat <- data.frame(
  # Pull the coefficients and the standard errors
  coeff = model_2_sumry$bootMed, 
  se = model_2_sumry$bootSE
) %>%
  # Calculate the standard t-statistic
  mutate(t_stat = coeff / se) %>%
  # Calculate the p-value of the t-statistic
  mutate(t_p_value = 2 * pt(q = abs(t_stat), df = (nrow(scop_combined) - 1), lower.tail = FALSE))
model_2_tstat

# Get R^2
model_2_r2 <- boot(data = scop_combined, statistic = rsq_function, R = 5000, 
                   formula = freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                     # Syndemic covariates
                     hiv_ctrl + homeless_exp + food_ran_out)
model_2_r2
boot.ci(model_2_r2, type = "bca")

# Get AIC
model_2_aic <- boot(data = scop_combined, statistic = get_aic, R = 5000, 
                   formula = freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                     # Syndemic covariates
                     hiv_ctrl + homeless_exp + food_ran_out)
model_2_aic

# MODEL 3 - DEHUMANIZATION ------------------------------------------------

# Model 3
model_3 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out +
                # Dehumanization variables
                eds_gender_total + eds_orientation_total + unwanted, data = scop_combined)

# Bootstrap estimation
model_3_boot <- Boot(model_3, R = 5000, method = "case")

# Summarize the model
model_3_sumry <- summary(model_3_boot)
model_3_sumry

# Confidence intervals for model estimates
confint(model_3_boot, level = .95, type = "norm")

# Get t-statistics for each coefficient
model_3_tstat <- data.frame(
  # Pull the coefficients and the standard errors
  coeff = model_3_sumry$bootMed, 
  se = model_3_sumry$bootSE
) %>%
  # Calculate the standard t-statistic
  mutate(t_stat = coeff / se) %>%
  # Calculate the p-value of the t-statistic
  mutate(t_p_value = 2 * pt(q = abs(t_stat), df = (nrow(scop_combined) - 1), lower.tail = FALSE))
model_3_tstat

# Get R^2
model_3_r2 <- boot(data = scop_combined, statistic = rsq_function, R = 5000, 
                   formula = freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                     # Syndemic covariates
                     hiv_ctrl + homeless_exp + food_ran_out +
                     # Dehumanization variables
                     eds_gender_total + eds_orientation_total + unwanted)
model_3_r2
boot.ci(model_3_r2, type = "bca")

# Get R^2
model_3_aic <- boot(data = scop_combined, statistic = get_aic, R = 5000, 
                   formula = freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                     # Syndemic covariates
                     hiv_ctrl + homeless_exp + food_ran_out +
                     # Dehumanization variables
                     eds_gender_total + eds_orientation_total + unwanted)
model_3_aic

# MODEL 4 - SOCIAL SUPPORT ------------------------------------------------

# Model 4
model_4 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out +
                # Dehumanization variables
                eds_gender_total + eds_orientation_total + unwanted +
                # Protective factors
                mspss_total + brs_total, data = scop_combined)

# Bootstrap estimation
model_4_boot <- Boot(model_4, R = 5000, method = "case")

# Summarize the model
model_4_sumry <- summary(model_4_boot)
model_4_sumry

# Confidence intervals for model estimates
confint(model_4_boot, level = .95, type = "norm")

# Get t-statistics for each coefficient
model_4_tstat <- data.frame(
  # Pull the coefficients and the standard errors
  coeff = model_4_sumry$bootMed, 
  se = model_4_sumry$bootSE
) %>%
  # Calculate the standard t-statistic
  mutate(t_stat = coeff / se) %>%
  # Calculate the p-value of the t-statistic
  mutate(t_p_value = 2 * pt(q = abs(t_stat), df = (nrow(scop_combined) - 1), lower.tail = FALSE))
model_4_tstat

# Get R^2
model_4_r2 <- boot(data = scop_combined, statistic = rsq_function, R = 5000, 
                   formula = freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                     # Syndemic covariates
                     hiv_ctrl + homeless_exp + food_ran_out +
                     # Dehumanization variables
                     eds_gender_total + eds_orientation_total + unwanted +
                     # Protective factors
                     mspss_total + brs_total)
model_4_r2
boot.ci(model_4_r2, type = "bca")

# Get R^2
model_4_aic <- boot(data = scop_combined, statistic = get_aic, R = 5000, 
                   formula = freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                     # Syndemic covariates
                     hiv_ctrl + homeless_exp + food_ran_out +
                     # Dehumanization variables
                     eds_gender_total + eds_orientation_total + unwanted +
                     # Protective factors
                     mspss_total + brs_total)
model_4_aic

# MODEL COMPARISON --------------------------------------------------------

# Model 1 vs. Model 2
f_value_1_2 <- get_f_value(r2_a = model_1_r2$t0, r2_b = model_2_r2$t0,
            model_a = model_1, model_b = model_2)

# df numerator: k_b
length(model_2$coefficients)

# df denominator: n - k_a - k_b - 1
nrow(scop_combined) - length(model_1$coefficients) - length(model_2$coefficients) - 1

# Critical F-value from Cohen et al. (2013, pp. 648 - 649)
2.06
f_value_1_2

# Change in R^2
model_2_r2$t0 - model_1_r2$t0

###########################################################################

# Model 2 vs. Model 3
f_value_2_3 <- get_f_value(r2_a = model_2_r2$t0, r2_b = model_3_r2$t0,
            model_a = model_2, model_b = model_3)

# df numerator: k_b
length(model_3$coefficients)

# df denominator: n - k_a - k_b - 1
nrow(scop_combined) - length(model_2$coefficients) - length(model_3$coefficients) - 1

# Critical F-value from Cohen et al. (2013, pp. 648 - 649)
1.88
f_value_2_3

# Change in R^2
model_3_r2$t0 - model_2_r2$t0

###########################################################################

# Model 3 vs. Model 4
f_value_3_4 <- get_f_value(r2_a = model_3_r2$t0, r2_b = model_4_r2$t0,
            model_a = model_3, model_b = model_4)

# df numerator: k_b
length(model_4$coefficients)

# df denominator: n - k_a - k_b - 1
nrow(scop_combined) - length(model_3$coefficients) - length(model_4$coefficients) - 1

# Critical F-value from Cohen et al. (2013, pp. 648 - 649)
1.84
f_value_3_4

# Change in R^2
model_4_r2$t0 - model_3_r2$t0
