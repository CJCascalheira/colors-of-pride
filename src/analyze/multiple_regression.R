# Dependencies
library(psych)
library(car)
library(KScorrect)
library(tidyverse)
library(rio)
library(sjPlot)

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
  filter(!is.na(freq_polydrug_use))

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

# MODEL 0 - TEST FOR POTENTIAL MODERATION - SOCIAL SUPPORT ----------------

# Model 0 - SO EDS + Social Support
model_0a <- lm(freq_polydrug_use ~ eds_orientation_total + mspss_total, data = scop_combined)
summary(model_0a)

#### Moderation
model_0a_mod <- lm(freq_polydrug_use ~ eds_orientation_total + mspss_total + 
                     (eds_orientation_total * mspss_total), data = scop_combined)
summary(model_0a_mod)
anova(model_0a, model_0a_mod)

# Model 0 - GI EDS + Social Support
model_0b <- lm(freq_polydrug_use ~ eds_gender_total + mspss_total, data = scop_combined)
summary(model_0b)

#### Moderation
model_0b_mod <- lm(freq_polydrug_use ~ eds_gender_total + mspss_total +
                     (eds_gender_total * mspss_total), data = scop_combined)
summary(model_0b_mod)
anova(model_0b, model_0b_mod)

# Model 0 - Unwanted sexual contact + Social Support
model_0c <- lm(freq_polydrug_use ~ unwanted + mspss_total, data = scop_combined)
summary(model_0c)
# No case for moderation due to non-significant unwanted sexual experience 

# MODEL 00 - TEST FOR POTENTIAL MODERATION - COPING -----------------------

# Model 0 - SO EDS + Coping Capacity
model_00a <- lm(freq_polydrug_use ~ eds_orientation_total + total_coping, data = scop_combined)
summary(model_00a)

#### Moderation
model_00a_mod <- lm(freq_polydrug_use ~ eds_orientation_total + total_coping + 
                     (eds_orientation_total * total_coping), data = scop_combined)
summary(model_00a_mod)
anova(model_00a, model_00a_mod)

# Model 0 - GI EDS + Coping Capacity
model_00b <- lm(freq_polydrug_use ~ eds_gender_total + total_coping, data = scop_combined)
summary(model_00b)

#### Moderation
model_00b_mod <- lm(freq_polydrug_use ~ eds_gender_total + total_coping +
                     (eds_gender_total * total_coping), data = scop_combined)
summary(model_00b_mod)
anova(model_00b, model_00b_mod)

# Model 0 - Unwanted sexual contact + Coping Capacity
model_00c <- lm(freq_polydrug_use ~ unwanted + total_coping, data = scop_combined)
summary(model_00c)
# No case for moderation due to non-significant unwanted sexual experience 

# MODEL 1 - DEMOGRAPHIC COVARIATES ----------------------------------------

# Model 1
model_1 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl, data = scop_combined)

# Get standardized coefficients 
model_1_std <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl, data = scop_1)

# Summarize the model
summary(model_1)
summary(model_1_std)

# MODEL 2 - SYNDEMIC COVARIATES -------------------------------------------

# Model 2
model_2 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out, data = scop_combined)

# Get standardized coefficients 
model_2_std <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                    # Syndemic covariates
                    hiv_ctrl + homeless_exp + food_ran_out, data = scop_combined)

# Summarize the model
summary(model_2)
summary(model_2_std)

# MODEL 3 - DEHUMANIZATION ------------------------------------------------

# Model 3
model_3 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out +
                # Dehumanization variables
                eds_gender_total + eds_orientation_total + unwanted, data = scop_combined)

# Get standardized coefficients 
model_3_std <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                    # Syndemic covariates
                    hiv_ctrl + homeless_exp + food_ran_out +
                    # Dehumanization variables
                    eds_gender_total + eds_orientation_total + unwanted, data = scop_combined)

# Summarize the model
summary(model_3)
summary(model_3_std)

# MODEL 4 - SOCIAL SUPPORT ------------------------------------------------

# Model 4
model_4 <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                # Syndemic covariates
                hiv_ctrl + homeless_exp + food_ran_out +
                # Dehumanization variables
                eds_gender_total + eds_orientation_total + unwanted +
                # Protective factors
                mspss_total, data = scop_combined)

# Get standardized coefficients 
model_4_std <- lm(freq_polydrug_use ~ race_ctrl + age + income_ctrl +
                    # Syndemic covariates
                    hiv_ctrl + homeless_exp + food_ran_out +
                    # Dehumanization variables
                    eds_gender_total + eds_orientation_total + unwanted +
                    # Protective factors
                    mspss_total, data = scop_combined)

# Summarize the model
summary(model_4)
summary(model_4_std)

# MODEL COMPARISON --------------------------------------------------------

####### MODEL COMPARISON
anova(model_1, model_2, model_3, model_4)
tab_model(model_1, model_2, model_3, model_4)

# Confidence intervals
confint(model_2)
