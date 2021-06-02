# Dependencies
library(tidyverse)
library(rio)

# Import
scop <- import("data/raw/scop_final.sav") %>%
  as_tibble() %>%
  # Drop old record ID
  select(-record_id) %>%
  # Create new numeric record ID
  mutate(record_id = 1:nrow(.)) %>%
  # Move record ID to the front of the dataframe
  select(record_id, everything())
scop

# PREPARE DATASET ---------------------------------------------------------

# Total participants
nrow(scop)

# Drop participants
scop_adults <- scop %>%
  # Remove if no consent
  filter(consent_for_study != 0) %>% 
  # n = 2
  # Remove if younger than 18
  filter(age >= 18) %>%
  # n = 151
  # Remove is missing sexual orientation
  filter(!(is.na(orientation)))
  # n = 3

# Missing data for EEDS, drugs variables for first 56 participants, so drop
scop_adults1 <- scop_adults[-c(1:56), ]

# New total participants
nrow(scop_adults1)

# Select variables for dehumanization project
scop_1 <- scop_adults1 %>%
  select(record_id, age, birth, intersex, trans, starts_with("gender__"), 
         other_gender, perception_scale, orientation, other_orientation,
         hiv_result, starts_with("race"), other_race, education, annual_income, 
         homeless_exp, food_ran_out, res1:res21, starts_with("res_scale"),
         starts_with("helpful"), trans_age, age_orientation, age_lgbtq,
         starts_with("gender_prob"), starts_with("orientation_prob"), ptsd,
         starts_with("hiv_prob"), sud, hiv, starts_with("drugs"), injection, 
         addiction, starts_with("drug_prob"), addict_diagnosis, addict_treatment, 
         starts_with("drug_affects"), poz_undet, unwanted, 
         starts_with("assualtant"), recent_assault) %>%
  # Recode variables
  mutate(
    # Demographic variables
    birth = recode(birth, `1` = "female", `0` = "male", `99` = "other", 
                   `88` = "unsure", `77` = "prefer not to answer"),
    intersex = recode(intersex, `1` = "yes", `0` = "no", `88` = "unsure", `77` = "prefer not to answer"),
    trans = recode(trans, `1` = "yes", `0` = "no", `88` = "unsure", `77` = "prefer not to answer"),
    gender___1 = recode(gender___1, `1` = "man", `0` = "not_applicable"),
    gender___2 = recode(gender___2, `1` = "woman", `0` = "not_applicable"),
    gender___3 = recode(gender___3, `1` = "transgender", `0` = "not_applicable"),
    gender___4 = recode(gender___4, `1` = "genderqueer", `0` = "not_applicable"),
    gender___5 = recode(gender___5, `1` = "agender", `0` = "not_applicable"),
    gender___6 = recode(gender___6, `1` = "nonbinary", `0` = "not_applicable"),
    gender___7 = recode(gender___7, `1` = "gender nonconforming", `0` = "not_applicable"),
    gender___99 = recode(gender___99, `1` = "not_listed", `0` = "not_applicable"),
    orientation = recode(orientation, `1` = "lesbian", `2` = "gay", `3` = "bisexual", 
                         `4` = "pansexual", `5` = "same-gender loving", `6` = "asexual",
                         `7` = "queer", `8` = "heterosexual", `9` = "heteroflexible", `99` = "not listed"),
    hiv_result = recode(hiv_result, `1` = "yes", `0` = "no", `88` = "unsure"),
    # Race variables - two different types to consider
    race_expansive = recode(race_expansive, `1` = "AI/AN", `2` = "asian", `3` = "black", 
                            `4` = "latinx", `5` = "latinx/AI", `6` = "latinx/multi", `7` = "non-lat white",
                            `8` = "other"),
    race_expansive = if_else(is.na(race_expansive), "other", race_expansive),
    race___1 = recode(race___1, `1` = "AI/AN", `0` = "not_applicable"),
    race___2 = recode(race___2, `1` = "asian", `0` = "not_applicable"),
    race___3 = recode(race___3, `1` = "south asian", `0` = "not_applicable"),
    race___4 = recode(race___4, `1` = "black", `0` = "not_applicable"),
    race___5 = recode(race___5, `1` = "latinx", `0` = "not_applicable"),
    race___6 = recode(race___6, `1` = "white", `0` = "not_applicable"),
    race___99 = recode(race___99, `1` = "not_listed", `0` = "not_applicable"),
    # More demographics
    education = recode(education, `0` = "some high school", `1` = "some high school",
                       `2` = "high school / GED", `3` = "some college", `4` = "bachelor",
                       `5` = "master's", `6` = "doctorate"),
    annual_income = recode(annual_income, `0` = "< $10k", `1` = "$10 - 19k",
                       `2` = "$20 - 29k", `3` = "$30 - 39k", `4` = "$40 - 49k",
                       `5` = "$50 - 74k", `6` = "$75 - 100k", `7` = "> $100k"),
    homeless_exp = recode(homeless_exp, `0` = "no", `1` = "yes, currently", 
                          `2` = "yes, past year", `3` = "yes, > year ago"),
    sud = recode(sud, `1` = "yes", `0` = "no"),
    hiv = recode(hiv, `1` = "yes", `0` = "no"),
    injection = recode(injection, `1` = "yes", `0` = "no", `77` = "prefer not to answer"),
    addiction = recode(addiction, `1` = "yes", `0` = "no"),
    addict_diagnosis = recode(addict_diagnosis, `1` = "yes", `0` = "no"),
    addict_treatment = recode(addict_treatment, `1` = "yes", `0` = "no"),
    poz_undet = recode(poz_undet, `1` = "yes", `0` = "no", `88` = "unsure"),
    unwanted = recode(unwanted, `1` = "yes", `0` = "no"),
    recent_assault = recode(recent_assault, `1` = "yes", `0` = "no")
  ) %>%
  # Fix direction of scales to match published articles
  mutate(
    # MPSS = higher scores indicate more social support
    res1 = recode(res1, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res2 = recode(res2, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res3 = recode(res3, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res4 = recode(res4, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res5 = recode(res5, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res6 = recode(res6, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res7 = recode(res7, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res8 = recode(res8, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res9 = recode(res9, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res10 = recode(res10, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res11 = recode(res11, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    res12 = recode(res12, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1),
    # BRS = higher scores indicate more resilience
    res_scale1 = recode(res_scale1, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    res_scale2 = recode(res_scale2, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    res_scale3 = recode(res_scale3, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    res_scale4 = recode(res_scale4, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    res_scale5 = recode(res_scale5, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    res_scale6 = recode(res_scale6, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    # EEDS gender = higher scores indicate more discrimination 
    gender_prob_1 = recode(gender_prob_1, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_2 = recode(gender_prob_2, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_3 = recode(gender_prob_3, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_4 = recode(gender_prob_4, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_5 = recode(gender_prob_5, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_6 = recode(gender_prob_6, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_7 = recode(gender_prob_7, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_8 = recode(gender_prob_8, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_9 = recode(gender_prob_9, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_10 = recode(gender_prob_10, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_11 = recode(gender_prob_11, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_12 = recode(gender_prob_12, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_13 = recode(gender_prob_13, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_14 = recode(gender_prob_14, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_15 = recode(gender_prob_15, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_16 = recode(gender_prob_16, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_17 = recode(gender_prob_17, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_18 = recode(gender_prob_18, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    gender_prob_19 = recode(gender_prob_19, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    # EEDS orientation = higher scores indicate more discrimination 
    orientation_prob1 = recode(orientation_prob1, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob2 = recode(orientation_prob2, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob3 = recode(orientation_prob3, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob4 = recode(orientation_prob4, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob5 = recode(orientation_prob5, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob6 = recode(orientation_prob6, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob7 = recode(orientation_prob7, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob8 = recode(orientation_prob8, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob9 = recode(orientation_prob9, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob10 = recode(orientation_prob10, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob11 = recode(orientation_prob11, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob12 = recode(orientation_prob12, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob13 = recode(orientation_prob13, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob14 = recode(orientation_prob14, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob15 = recode(orientation_prob15, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob16 = recode(orientation_prob16, `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    orientation_prob17 = recode(orientation_prob17, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
  ) %>%
  # Adjust the drug scoring to show that higher scores indicate more frequent problematic use
  mutate_at(c("drugs1", "drugs2", "drugs3", "drugs4", "drugs5", "drugs6", "drugs7", 
              "drugs8", "drugs9", "drugs10"), funs(recode(., `1` = 6, `2` = 5, `3` = 4, 
                                                         `4` = 3, `5` = 2, `6` = 1, 
                                                         .default = NaN))) %>%
  # Adjust the drug problems scoring to show that higher scores indicate more frequent use
  mutate_at(c("drug_prob1", "drug_prob2", "drug_prob3", "drug_prob4", "drug_prob5", "drug_prob6", "drug_prob7", 
              "drug_prob8", "drug_prob9"), funs(recode(., `1` = 6, `2` = 5, `3` = 4, 
                                                          `4` = 3, `5` = 2, `6` = 1, 
                                                          .default = NaN))) %>%
  # Reverse score
  mutate(
    res_scale2 = recode(res_scale2, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    res_scale4 = recode(res_scale4, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    res_scale6 = recode(res_scale6, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
  )

# CHECK CELL SIZES OF MAIN VARIABLES --------------------------------------

# Number of transgender people
scop_1 %>%
  count(trans)

# Explore race variable
scop_1 %>%
  count(race_expansive)

# Number of HIV+ participants
scop_1 %>%
  count(hiv_result)

# Number of participants by sexual orientation
scop_1 %>%
  count(orientation)

# Number of SUD
scop_1 %>%
  count(sud)

# Number of PTSD
scop_1 %>%
  count(ptsd)

# Number of addiction
scop_1 %>%
  count(addiction)

# Number of unwanted sexual contact
scop_1 %>%
  count(unwanted)

# RECODE DEMOGRAPHICS -----------------------------------------------------

# Gender variables
scop_gender <- scop_1 %>%
  select(record_id, starts_with("gender___")) %>%
  # Drop other column
  select(-gender___99) %>%
  # Unite all the columns
  unite(gender_united, gender___1:gender___7, sep = "_", remove = TRUE, na.rm = TRUE) %>%
  mutate(
    gender_united = str_remove_all(gender_united, "not_applicable"),
    gender_united = str_remove_all(gender_united, "_")
  )

# Check the unique combinations
unique(scop_gender$gender_united)
table(scop_gender$gender_united)

# Create vector of single gender categories
one_gender <- c("man", "woman", "transgender", "genderqueer", "agender", 
                "nonbinary", "gender nonconforming", "")

# Reassign to gender categories
clean_gender <- scop_gender %>%
  filter(!(gender_united %in% one_gender)) %>%
  mutate(
    # Code nonbinary folx
    nonbinary = if_else(str_detect(gender_united, regex("nonbinary", ignore_case = TRUE)), 
                        "nonbinary", NA_character_),
    # Code binary transgender folx
    transgender = if_else(is.na(nonbinary) & str_detect(gender_united, regex("manwoman|transgender", ignore_case = TRUE)), "transgender", NA_character_),
    # Code gender nonconforming
    nonconforming = if_else(is.na(nonbinary) & is.na(transgender) & str_detect(gender_united, regex("nonconforming", ignore_case = TRUE)), "gender nonconforming", NA_character_),
    # Recode genderqueer / agender
    genderqueer = if_else(is.na(nonbinary) & is.na(transgender) & is.na(nonconforming) & str_detect(gender_united, regex("genderqueer|agender", ignore_case = TRUE)), "genderqueer", NA_character_)
  ) %>%
  # United all columns
  select(-gender_united) %>%
  unite(gender_united, nonbinary:genderqueer, na.rm = TRUE)

# Combine the gender variables
scop_gender_1 <- scop_gender %>%
  filter(!(record_id %in% clean_gender$record_id)) %>%
  bind_rows(clean_gender) %>%
  # Replace unknown gender
  mutate(
    gender_united = if_else(gender_united == "", "unknown", gender_united)
  ) %>%
  # Rename the gender column
  rename(gender = gender_united)

# Clean up gender in main data frame
scop_2 <- scop_1 %>%
  # Drop gender variables and other demographic variables not needed
  select(-starts_with("gender___"), -other_gender, -other_orientation, -starts_with("race___"), -RACE, -RACE2, -other_race) %>%
  # Recode race variable
  rename(race = race_expansive) %>%
  # Add new gedner variable
  left_join(scop_gender_1) %>%
  # Rearrange columns
  select(record_id, age, birth, gender, orientation, race, hiv_result, everything())

# SAVE CLEANED DEMOGRAPHICS -----------------------------------------------

write_csv(scop_2, "data/cleaned/cleaned_demographics.csv")
