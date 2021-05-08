# Dependencies
library(tidyverse)
library(rio)

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

# N
nrow(scop_a)

# Recode variables
scop <- scop_a %>%
  mutate(
    income_ctrl = scop_b$annual_income,
    # 1 = Non-White, 0 = White
    race_ctrl = recode(race, "non-lat white" = 0, .default = 1)
  )

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

# ASSUMPTIONS -------------------------------------------------------------


