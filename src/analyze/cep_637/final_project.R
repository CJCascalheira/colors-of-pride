# Dependencies
library(psych)
library(car)
library(KScorrect)
library(tidyverse)
library(rio)
library(sjPlot)

# Remove scientific notation
options(scipen = 999)

# Import data - cleaned
scop_a <- read_csv("data/cleaned/cleaned_cep-637.csv") %>%
  # Drop heterosexual folx
  filter(orientation != "heterosexual") %>%
  # Remove NA for annual income
  filter(!is.na(annual_income))

# Import
scop_b <- import("data/raw/scop_final.sav") %>%
  as_tibble() %>%
  # Drop old record ID
  select(-record_id) %>%
  # Create new numeric record ID
  mutate(record_id = 1:nrow(.)) %>%
  # Move record ID to the front of the dataframe
  select(record_id, everything()) %>%
  filter(record_id %in% scop_a$record_id)

# Recode variables
scop_c <- scop_a %>%
  mutate(
    income_ctrl = scop_b$annual_income,
    # 1 = Non-White, 0 = White
    race_ctrl = recode(race, "non-lat white" = 0, .default = 1)
  )

# Remove people who never experienced discrimination or drug use
scop <- scop_c %>%
  filter(freq_prob_drug_use != 1)

# Scale all variables to return standardized coefficients
scop_1 <- scop %>%
  select(freq_prob_drug_use, race_ctrl, age, income_ctrl, unwanted, eds_orientation_total, mspss_total) %>%
  lapply(scale) %>%
  as.data.frame()

# N
nrow(scop)

# MODELS ------------------------------------------------------------------

# Model 1
model_1 <- lm(freq_prob_drug_use ~ race_ctrl + age + income_ctrl, data = scop)

# Get standardized coefficients 
model_1_std <- lm(freq_prob_drug_use ~ race_ctrl + age + income_ctrl, data = scop_1)

# Model 2
model_2 <- lm(freq_prob_drug_use ~ race_ctrl + age + income_ctrl + unwanted + 
                eds_orientation_total + mspss_total, data = scop)

# Get standardized coefficients 
model_2_std <- lm(freq_prob_drug_use ~ race_ctrl + age + income_ctrl + unwanted + 
                    eds_orientation_total + mspss_total, data = scop_1)

####### Model 1

# Summarize the model
summary(model_1)
summary(model_1_std)

####### Model 2

# Summarize the model
summary(model_2)
summary(model_2_std)

####### MODEL COMPARISON
anova(model_1, model_2)
tab_model(model_1, model_2)

# Confidence intervals
confint(model_2)

# ANALYZE DEMOGRAPHICS ----------------------------------------------------

# Age
scop %>%
  summarize(
    M = mean(age),
    SD = sd(age)
  )

# Sexual Orientation
scop %>%
  mutate(
    orientation = recode(orientation, "same-gender loving" = "gay/lesbian", "lesbian" = "gay/lesbian", "gay" = "gay/lesbian", "bisexual" = "plurisexual", "pansexual" = "plurisexual", "heteroflexible" = "plurisexual")
  ) %>%
  count(orientation) %>%
  arrange(desc(n)) %>%
  mutate(perc = (n / nrow(scop)) * 100)

# Gender
scop %>%
  count(gender) %>%
  arrange(desc(n)) %>%
  mutate(perc = (n / nrow(scop)) * 100)

# Race
scop %>%
  mutate(
    race = recode(race, "latinx/AI" = "latinx/multi")
  ) %>%
  count(race) %>%
  arrange(desc(n)) %>%
  mutate(perc = (n / nrow(scop)) * 100)

# Income
scop %>%
  count(annual_income) %>%
  arrange(desc(n)) %>%
  mutate(perc = (n / nrow(scop)) * 100)

# ASSUMPTIONS - NORMALITY -------------------------------------------------

# Age
hist_age <- scop %>%
  ggplot(aes(x = age)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_age
ggsave("src/analyze/cep_637/plots/age.png", plot = hist_age)

# Kolmogorov-Smirnov (KS) Test
LcKS(scop$age, "pnorm", nreps = 1999)

# Income
hist_income <- scop %>%
  ggplot(aes(x = income_ctrl)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_income
ggsave("src/analyze/cep_637/plots/income.png", plot = hist_income)

# Kolmogorov-Smirnov (KS) Test
LcKS(scop$income_ctrl, "pnorm", nreps = 1999)

# Discrimination
hist_eds <- scop %>%
  ggplot(aes(x = eds_orientation_total)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_eds
ggsave("src/analyze/cep_637/plots/eds.png", plot = hist_eds)

# Kolmogorov-Smirnov (KS) Test
LcKS(scop$eds_orientation_total, "pnorm", nreps = 1999)

# Social support
hist_mspss <- scop %>%
  ggplot(aes(x = mspss_total)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_mspss
ggsave("src/analyze/cep_637/plots/mspss.png", plot = hist_mspss)

# Kolmogorov-Smirnov (KS) Test
LcKS(scop$mspss_total, "pnorm", nreps = 1999)

# Frequency problematic drug use
hist_drug <- scop %>%
  ggplot(aes(x = freq_prob_drug_use)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
hist_drug
ggsave("src/analyze/cep_637/plots/drug.png", plot = hist_drug)

# Kolmogorov-Smirnov (KS) Test
LcKS(scop$freq_prob_drug_use, "pnorm", nreps = 1999)

# Skew and kurtosis > -2 & < 2; M =~ Md
scop %>%
  select(age, income_ctrl, mspss_total, eds_orientation_total, freq_prob_drug_use) %>%
  describe() %>%
  select(skew, kurtosis)

# NORMAL DISTRIBUTION OF RESIDUALS ----------------------------------------

# Q-Q plot of variables
qqPlot(scop$age)
qqPlot(scop$income_ctrl)
qqPlot(scop$eds_orientation_total)
qqPlot(scop$mspss_total)
qqPlot(scop$freq_prob_drug_use)

# RELATIONSHIP: STANDARDIZED RESIDUALS VS. PREDICTED VARIABLE -------------

####### MODEL 1 

# Create plots
par(mfrow = c(2, 2))
plot(model_1)

# Create a plot of standarized residuals vs. fitted values only
block_1_plot <- scop %>%
  ggplot(aes(model_1$fitted.values, rstandard(model_1))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Fitted Values", y = "Residuals", title = "Block 1") +
  theme_bw()
block_1_plot
ggsave("src/analyze/cep_637/plots/block_1.png", plot = block_1_plot)

# Histogram of standardized residuals
par(mfrow = c(1, 1))
hist(rstandard(model_1))

####### MODEL 2 

# Create plots
par(mfrow = c(2, 2))
plot(model_2)

# Create a plot of standarized residuals vs. fitted values only
block_2_plot <- scop %>%
  ggplot(aes(model_2$fitted.values, rstandard(model_2))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Fitted Values", y = "Residuals", title = "Block 2") +
  theme_bw()
block_2_plot
ggsave("src/analyze/cep_637/plots/block_2.png", plot = block_2_plot)

# Histogram of standardized residuals
par(mfrow = c(1, 1))
hist(rstandard(model_2))

# MULTICOLLINEARITY -------------------------------------------------------

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
