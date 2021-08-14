# Dependencies
library(psych)
library(car)
library(KScorrect)
library(tidyverse)
library(rio)
library(sjPlot)
library(lmtest)

# Remove scientific notation
options(scipen = 999)

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
  filter(!is.na(freq_polydrug_use)) %>%
  # Add gender variable for interaction
  mutate(
    # 0 = cisgender; 1 = TGNB
    tgnb_ctrl = if_else(str_detect(gender, regex("woman|man", ignore_case = TRUE)), 0, 1),
    # 1 = cis woman; 0 = not woman
    woman_ctrl = if_else(str_detect(gender, regex("woman", ignore_case = TRUE)), 1, 0)
  )

# Check
nrow(scop_combined)

# DEFINE MODELS -----------------------------------------------------------

# Model 1
model_1 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl, 
              data = scop_combined)

# Model 2
model_2 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out, 
              data = scop_combined)

# Model 3
model_3 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out + 
                # Dehumanization variables
                eds_gender_total + eds_orientation_total + unwanted, 
              data = scop_combined)

# Model 4
model_4 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out + 
                # Dehumanization variables
                eds_gender_total + eds_orientation_total + unwanted +
                # Protective factors
                mspss_total, 
              data = scop_combined)

# ASSUMPTIONS - NORMALITY -------------------------------------------------

# Age
hist_age <- scop_combined %>%
  ggplot(aes(x = age)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_age

# Kolmogorov-Smirnov (KS) Test
LcKS(scop_combined$age, "pnorm", nreps = 1999)

# Income
hist_income <- scop_combined %>%
  ggplot(aes(x = income_ctrl)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_income

# Kolmogorov-Smirnov (KS) Test
LcKS(scop_combined$income_ctrl, "pnorm", nreps = 1999)

# Discrimination
hist_eds_sexor <- scop_combined %>%
  ggplot(aes(x = eds_orientation_total)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_eds_sexor

# Discrimination
hist_eds_gi <- scop_combined %>%
  ggplot(aes(x = eds_gender_total)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_eds_gi

# Kolmogorov-Smirnov (KS) Test
LcKS(scop_combined$eds_orientation_total, "pnorm", nreps = 1999)
LcKS(scop_combined$eds_gender_total, "pnorm", nreps = 1999)

# Social support
hist_mspss <- scop_combined %>%
  ggplot(aes(x = mspss_total)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_mspss

# Kolmogorov-Smirnov (KS) Test
LcKS(scop_combined$mspss_total, "pnorm", nreps = 1999)

# Frequency problematic drug use
hist_drug <- scop_combined %>%
  ggplot(aes(x = freq_prob_drug_use)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_drug

# Kolmogorov-Smirnov (KS) Test
LcKS(scop_combined$freq_prob_drug_use, "pnorm", nreps = 1999)

# Skew and kurtosis > -2 & < 2; M =~ Md
scop_combined %>%
  select(age, income_ctrl, mspss_total, eds_orientation_total, freq_prob_drug_use) %>%
  describe() %>%
  select(skew, kurtosis)

# NORMAL DISTRIBUTION OF RESIDUALS ----------------------------------------

# Q-Q plot of variables
qqPlot(scop_combined$age)
qqPlot(scop_combined$income_ctrl)
qqPlot(scop_combined$eds_orientation_total)
qqPlot(scop_combined$eds_gender_total)
qqPlot(scop_combined$mspss_total)
qqPlot(scop_combined$freq_prob_drug_use)

# RELATIONSHIP: STANDARDIZED RESIDUALS VS. PREDICTED VARIABLE -------------

# Are the data homoskedastic?

####### MODEL 1 

# Create plots
par(mfrow = c(2, 2))
plot(model_1)

# Create a plot of standarized residuals vs. fitted values only
block_1_plot <- scop_combined %>%
  ggplot(aes(model_1$fitted.values, rstandard(model_1))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Fitted Values", y = "Residuals", title = "Block 1") +
  theme_bw()
block_1_plot

# Histogram of standardized residuals
par(mfrow = c(1, 1))
hist(rstandard(model_1))

# Breush-Pagan test
bptest(model_1)

####### MODEL 2 

# Create plots
par(mfrow = c(2, 2))
plot(model_2)

# Create a plot of standarized residuals vs. fitted values only
block_2_plot <- scop_combined %>%
  ggplot(aes(model_2$fitted.values, rstandard(model_2))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Fitted Values", y = "Residuals", title = "Block 2") +
  theme_bw()
block_2_plot

# Histogram of standardized residuals
par(mfrow = c(1, 1))
hist(rstandard(model_2))

# Breush-Pagan test
bptest(model_2)

####### MODEL 3

# Create plots
par(mfrow = c(2, 2))
plot(model_3)

# Create a plot of standarized residuals vs. fitted values only
block_3_plot <- scop_combined %>%
  ggplot(aes(model_3$fitted.values, rstandard(model_3))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Fitted Values", y = "Residuals", title = "Block 3") +
  theme_bw()
block_3_plot

# Histogram of standardized residuals
par(mfrow = c(1, 1))
hist(rstandard(model_3))

# Breush-Pagan test
bptest(model_3)

####### MODEL 4

# Create plots
par(mfrow = c(2, 2))
plot(model_4)

# Create a plot of standarized residuals vs. fitted values only
block_4_plot <- scop_combined %>%
  ggplot(aes(model_4$fitted.values, rstandard(model_4))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Fitted Values", y = "Residuals", title = "Block 4") +
  theme_bw()
block_4_plot

# Histogram of standardized residuals
par(mfrow = c(1, 1))
hist(rstandard(model_4))

# Breush-Pagan test
bptest(model_4)

# MULTICOLLINEARITY -------------------------------------------------------

# Toleranace < 0.2 = likely a problem and 
# Tolerance < 0.1 = bigger problem
# VIF > 10 = problem

####### MODEL 1 

# Check the variance inflation factor (VIF)
vif(model_1)

# Tolerance
1 / vif(model_1)

####### MODEL 2

# Check the variance inflation factor (VIF)
vif(model_2)

# Tolerance
1 / vif(model_2)

####### MODEL 3

# Check the variance inflation factor (VIF)
vif(model_3)

# Tolerance
1 / vif(model_3)

####### MODEL 4

# Check the variance inflation factor (VIF)
vif(model_4)

# Tolerance
1 / vif(model_4)


# AUTOCORRELATION ---------------------------------------------------------

# ACF (autocorrelation function). The first correlogram to investigate. 
acf(model_4$residuals)

# PACF (partial autocorrelation function) is the next correlogram to look at.
pacf(model_4$residuals)

# Breusch-Godfrey test
bgtest(model_4)

# Durbin-Watson test. Close to 2 = no evidence of autocorrelation. 
dwtest(model_4)

# SUMMARY -----------------------------------------------------------------

# Some of the variables are not normally distributed. There is evidence of heteroskedasticity and autocorrelation. 

# Therefore, bootstrapping or transformations are required. Model respecification may help
# with the violation of homoskedasticity. After trying several different models, however, 
# heteroskedasticity remained an issue. 

# EXPORT FOR JESSIE -------------------------------------------------------

# Save file
# write_csv(scop_combined, file = "data/cleaned/scop_combined.csv")
