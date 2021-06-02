# Dependencies
library(psych)
library(tidyverse)
library(rio)

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

# ANALYZE DEMOGRAPHICS ----------------------------------------------------

# Age
scop_combined %>%
  summarize(
    M = mean(age),
    SD = sd(age)
  )

# Number of drugs people have had problems with
scop_b %>%
  select(record_id, drugs1:drugs10) %>%
  filter(record_id %in% scop_combined$record_id) %>%
  # If at least some problem, mark as 1; if no problem, mark as 0
  mutate(across(drugs1:drugs10, ~ recode(., `1` = 0, .default = 1))) %>%
  # Count the number of drugs with which each participant has a problem
  gather("drugs", "number", -record_id) %>%
  group_by(record_id) %>%
  summarize(sum = sum(number)) %>%
  count(sum)

# Sexual Orientation
scop_combined %>%
  mutate(
    orientation = recode(orientation, "same-gender loving" = "gay/lesbian", "lesbian" = "gay/lesbian", "gay" = "gay/lesbian", "bisexual" = "plurisexual", "pansexual" = "plurisexual", "heteroflexible" = "plurisexual")
  ) %>%
  count(orientation) %>%
  arrange(desc(n)) %>%
  mutate(perc = (n / nrow(scop_combined)) * 100)

# Gender
scop_combined %>%
  count(gender) %>%
  arrange(desc(n)) %>%
  mutate(perc = (n / nrow(scop_combined)) * 100)

# Race
scop_combined %>%
  mutate(
    race = recode(race, "latinx/AI" = "latinx/multi")
  ) %>%
  count(race) %>%
  arrange(desc(n)) %>%
  mutate(perc = (n / nrow(scop_combined)) * 100)

# Income
scop_combined %>%
  count(annual_income) %>%
  arrange(desc(n)) %>%
  mutate(perc = (n / nrow(scop_combined)) * 100)