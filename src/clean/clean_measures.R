# Dependencies
library(ltm)
library(tidyverse)
library(rio)
library(RBtest)

# Import cleaned demographics
scop_pre <- read_csv("data/cleaned/cleaned_demographics.csv") %>%
  # Remove NA for annual income
  filter(!is.na(annual_income)) %>%
  # Select variables for this project
  select(record_id:annual_income, food_ran_out, homeless_exp, res1:res21, starts_with("res_scale"), unwanted,
         starts_with("orientation_prob"), starts_with("gender_prob"), starts_with("hiv_prob"),
         starts_with("helpful"), starts_with("drugs"), starts_with("drug_affect"), starts_with("aces")) %>%
  mutate(
    # Recode the unwanted sexual contact variable to binary
    unwanted = recode(unwanted, "yes" = 1, "no" = 0),
    # Recode food insecurity to binary where 1 (yes it ran out in past 12 months) or 0 (no)
    food_ran_out = recode(food_ran_out, `1` = 1, `2` = 1, `3` = 0),
    # Recode homeless to binary where 1 (at least once) and 0 (never)
    homeless_exp = recode(homeless_exp, "no" = 0, .default = 1)
  ) %>%
  # Recode the helpful variables so higher scores indicate that an activity was extremely helpful
  mutate(across(helpful1:helpful16, ~ recode(., `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1))) %>%
  # Remove people who did not know whether their food ran out
  filter(food_ran_out != 4)

# Import final data set
scop_combined <- read_csv("data/cleaned/scop_combined.csv")

# SCORE COPING ACTIVITIES -------------------------------------------------

# Score coping variables- weighted sum
# Total number of activities, weighted by activity helpfulness
total_coping <- scop_pre %>%
  select(record_id, starts_with("helpful")) %>%
  # Replace missing values with 0
  # Zero = person did not engage with that coping activity
  mutate(across(helpful1:helpful16, ~replace(., is.na(.), 0))) %>%
  # Convert to long format
  gather(key = "coping_variable", value = "how_helpful", -record_id) %>%
  # Create new variable: 1 = activity used, 0 = activity not used
  mutate(activity_used = if_else(how_helpful == 0, 0, 1)) %>%
  # Replace missing values (i.e., 66) with 0
  mutate(how_helpful = if_else(how_helpful == 66, 0, how_helpful)) %>%
  # Calculate score for each participant
  group_by(record_id) %>%
  # Weighted sum
  summarize(
    total_coping = sum(activity_used * how_helpful)
  )

# Join the data frames
scop <- scop_pre %>%
  select(-starts_with("helpful")) %>%
  left_join(total_coping)
  
# SCORE HIV DISCRIMINATION ------------------------------------------------

# Clean HIV EDS
scop_hiv <- scop %>%
  select(record_id, starts_with("hiv_prob")) %>%
  # Drop intracommunity variables
  select(-ends_with("___5")) %>%
  gather("item_option", "value", -record_id) %>%
  # Extract the item names
  mutate(
    item_name = str_extract(item_option, regex("hiv_prob\\d{1,2}", ignore_case = TRUE))
  ) %>%
  group_by(record_id, item_name) %>%
  # Get score for each item per participant
  summarize(value = sum(value)) %>%
  # Drop any values that are not 1 through 4. as expected from codebook
  filter(value %in% c(1, 2, 3, 4)) %>%
  ungroup() %>%
  # Return to original form
  spread("item_name", "value")
scop_hiv

# Join data frames
scop_eds <- scop %>%
  select(-starts_with("hiv_prob"), -contains("___")) %>%
  left_join(scop_hiv)

# This sample size will be too small, so it is better to use HIV as a covariate
# I kept this code due to the novel coding approach
# This coding may be useful to future studies (i.e., Ryan's work)

# MISSING DATA: COMMON DATA -----------------------------------------------

# Select common variables
scop_common <- scop_eds %>%
  select(record_id, age, trans, orientation, res1:res21, starts_with("res_scale"), 
         unwanted, starts_with("drugs"), starts_with("drug_affect")) 

# Check for complete cases
scop_common_complete <- scop_common[complete.cases(scop_common), ]

# Test for missing data - resilience scales 
scop_common_missing <- RBtest(scop_common_complete)

# Types of missing variables
# -1 = complete variables
# 0 = MCAR (missing completely at random)
# 1 = MAR (missing at random)
table(scop_common_missing$type)

# Select data for analyses
scop_1a <- scop_eds %>%
  # Filter for common complete variables
  filter(record_id %in% scop_common_complete$record_id) %>%
  select(record_id:hiv_result, education:homeless_exp, res1:res21, starts_with("res_scale"), unwanted,
       starts_with("orientation_prob"), starts_with("gender_prob_"), starts_with("drugs"), 
       starts_with("drug_affect"), starts_with("aces"),
       total_coping)

# INTRACOMMUNITY SCORE ----------------------------------------------------

# Check intracommunity prejudice
intracomm_eds <- scop %>%
  # Select the discrimination variables from within the community
  select(record_id, ends_with("___5")) %>%
  # Drop HIV variables
  select(-starts_with("hiv")) %>%
  gather(key = "eds", value = "score", -record_id) %>%
  filter(score == 1) %>%
  spread(key = "eds", value = "score") %>%
  mutate(intracomm_eds = rep(1, nrow(.))) %>%
  select(record_id, intracomm_eds)

# Select other record ids
no_intra_eds <- anti_join(scop, intracomm_eds) %>%
  select(record_id) %>%
  mutate(
    intracomm_eds = rep(0, nrow(.))
  )

# Create new intracommunity EDS variable for main dataframe
intracomm_eds_1 <- intracomm_eds %>%
  bind_rows(no_intra_eds)

# Add intracommunity EDS to dataframe
scop_1 <- left_join(scop_1a, intracomm_eds_1)

# CALCULATE SCORES: COMMON ------------------------------------------------

# ACES to control
aces_total <- scop_1 %>%
  select(record_id, starts_with("aces")) %>%
  gather(key = "aces", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(aces_total = sum(score))

# Score Multidimensional Scale of Perceived Social Support (Zimet et al., 1988, 1990)
# Higher scores indicate more perceived social support
mspss_total <- scop_1 %>%
  select(record_id, res1:res12) %>%
  gather(key = "res", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(mspss_total = mean(score))

# Score Brief Resilience Scale (Smith et al., 2008)
# Higher scores indicate more resilience
brs_total <- scop_1 %>%
  select(record_id, res_scale1:res_scale6) %>%
  gather(key = "res_scale", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(brs_total = mean(score))

# Mean score "drugs": problematic drug use frequency
# Higher scores indicate more frequent problematic drug use
freq_prob_drug_use <- scop_1 %>%
  select(record_id, drugs1:drugs10) %>%
  gather(key = "freq_prob_drug_use", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(freq_prob_drug_use = mean(score))

# Mean score "drugs": problematic drug use frequency
# Higher scores indicate more frequent problematic polydrug use
freq_polydrug_use <- scop_1 %>%
  select(record_id, drugs1:drugs10) %>%
  filter(
    # Alcohol problem AND problem with at least one other drug
    (
      # At least some problem with alcohol
      (drugs1 > 1) &
        # AND at least some problem with one other drug
        (
          drugs2 > 1 | drugs3 > 1 | drugs4 > 1 | drugs5 > 1 | drugs6 > 1 | 
            drugs7 > 1 | drugs8 > 1 | drugs9 > 1 | drugs10 > 1 
        )
    ) | # OR
    # No alcohol problem AND problem with at least one other drug
    (
      # Never a problem with alcohol
      (drugs1 == 1) &
        # AND at least some problem with one other drug
        (
          drugs2 > 1 | drugs3 > 1 | drugs4 > 1 | drugs5 > 1 | drugs6 > 1 | 
            drugs7 > 1 | drugs8 > 1 | drugs9 > 1 | drugs10 > 1
        )
    )
  ) %>%
  gather(key = "freq_polydrug_use", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(freq_polydrug_use = mean(score))

# Total score "drug_affects": Drug Abuse Screening Test (DAST-10)
# Higher scores indicate more severe problems related to drug abuse (NO ALCOHOL INCLUDED)
drug_abuse_severity <- scop_1 %>%
  select(record_id, drug_affects1:drug_affects10) %>%
  gather(key = "drug_abuse_severity", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Total score
  summarise(drug_abuse_severity = sum(score))

# Combine the main df with total scores
scop_2 <- scop_1 %>%
  # Drop individual items
  select(-starts_with("res"), -starts_with("drugs"), -starts_with("drug_aff"), -starts_with("aces")) %>%
  left_join(mspss_total) %>%
  left_join(brs_total) %>%
  left_join(freq_prob_drug_use) %>%
  left_join(drug_abuse_severity) %>%
  left_join(aces_total)
scop_2

# CALCULATE SCORES: SEPARATE ----------------------------------------------

# Separate the data sets
sex_or_data <- scop_2 %>%
  select(-starts_with("gender_prob"), -starts_with("hiv_prob"))
gender_data <- scop_2 %>%
  select(-starts_with("orientation_prob"), -starts_with("hiv_prob"))

# Check for complete cases
sex_or_complete <- sex_or_data[complete.cases(sex_or_data), ]
gender_complete <- gender_data[complete.cases(gender_data), ]

# Is there overlap between these two data sets?
sex_or_complete %>%
  filter(record_id %in% gender_complete$record_id) %>%
  # Yes, but there is a loss of ~70 folx 
  # How many participants are left when main outcome is limited
  filter(freq_prob_drug_use != 1) %>%
  nrow()

# Check counts separately
sex_or_complete %>%
  filter(freq_prob_drug_use != 1) %>%
  nrow()

gender_complete %>%
  filter(freq_prob_drug_use != 1) %>%
  nrow()

# Check scores of EDS for sexual orientation
sex_orient_eds <- sex_or_complete %>%
  select(record_id, starts_with("orientation_prob")) %>%
  gather(key = "item", value = "value", -record_id)
unique(sex_orient_eds$value) # EDS is 1 to 4, as expected

# Check scores of EDS for gender identity
gender_eds <- gender_complete %>%
  select(record_id, starts_with("gender_prob")) %>%
  gather(key = "item", value = "value", -record_id)
unique(gender_eds$value) # EDS is 1 to 4, as expected

# Score the Expanded Everyday Discrimination Scale (Williams et al., 2008)
# Used the full version from Stone et al. (2020) due to superior distribution 
# Higher scores indicate more frequent experiences of discrimination
eds_orientation_total <- sex_or_complete %>%
  select(record_id, orientation_prob1:orientation_prob17) %>%
  gather(key = "orientation_eds", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(eds_orientation_total = mean(score))

# Combine main df with total score - sexual orientation EDS
scop_eds_orientation <- scop_2 %>%
  # Drop individual items
  select(-starts_with("orientation_prob"), -starts_with("gender_prob")) %>%
  left_join(eds_orientation_total)
scop_eds_orientation

# Score EDS for gender identity
eds_gender_total <- gender_complete %>%
  select(record_id, gender_prob_1:gender_prob_19) %>%
  gather(key = "gender_eds", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(eds_gender_total = mean(score))

# Combine main df with total score - gender identity EDS
scop_eds_gender <- scop_2 %>%
  # Drop individual items
  select(-starts_with("orientation_prob"), -starts_with("gender_prob")) %>%
  left_join(eds_gender_total)
scop_eds_gender

# Create combined data set
scop_eds_combined <- scop_2 %>%
  # Drop individual items
  select(-starts_with("orientation_prob"), -starts_with("gender_prob")) %>%
  left_join(eds_gender_total) %>%
  left_join(eds_orientation_total)

# Check for complete cases
scop_eds_combined_1 <- scop_eds_combined[complete.cases(scop_eds_combined), ]

# CRONBACH'S ALPHA --------------------------------------------------------

# EDS-GI
gender_complete %>%
  select(gender_prob_1:gender_prob_19) %>%
  cronbach.alpha()

# EDS-SO
sex_or_complete %>%
  select(orientation_prob1:orientation_prob17) %>%
  cronbach.alpha()

# MSPSS
scop_1 %>%
  select(res1:res12) %>%
  cronbach.alpha()

# BRS
scop_1 %>%
  select(res_scale1:res_scale6) %>%
  cronbach.alpha()

# Frequent problematic polysubstance use
scop_1 %>%
  select(drugs1:drugs10) %>%
  filter(
    # Alcohol problem AND problem with at least one other drug
    (
      # At least some problem with alcohol
      (drugs1 > 1) &
        # AND at least some problem with one other drug
        (
          drugs2 > 1 | drugs3 > 1 | drugs4 > 1 | drugs5 > 1 | drugs6 > 1 | 
            drugs7 > 1 | drugs8 > 1 | drugs9 > 1 | drugs10 > 1 
        )
    ) | # OR
      # No alcohol problem AND problem with at least one other drug
      (
        # Never a problem with alcohol
        (drugs1 == 1) &
          # AND at least some problem with one other drug
          (
            drugs2 > 1 | drugs3 > 1 | drugs4 > 1 | drugs5 > 1 | drugs6 > 1 | 
              drugs7 > 1 | drugs8 > 1 | drugs9 > 1 | drugs10 > 1
          )
      )
  ) %>%
  cronbach.alpha()

# SELECT ITEMS FOR FACTOR ANALYSIS ----------------------------------------

# FPPU
fppu <- scop_1 %>%
  select(record_id, drugs1:drugs10) %>%
  filter(
    # Alcohol problem AND problem with at least one other drug
    (
      # At least some problem with alcohol
      (drugs1 > 1) &
        # AND at least some problem with one other drug
        (
          drugs2 > 1 | drugs3 > 1 | drugs4 > 1 | drugs5 > 1 | drugs6 > 1 | 
            drugs7 > 1 | drugs8 > 1 | drugs9 > 1 | drugs10 > 1 
        )
    ) | # OR
      # No alcohol problem AND problem with at least one other drug
      (
        # Never a problem with alcohol
        (drugs1 == 1) &
          # AND at least some problem with one other drug
          (
            drugs2 > 1 | drugs3 > 1 | drugs4 > 1 | drugs5 > 1 | drugs6 > 1 | 
              drugs7 > 1 | drugs8 > 1 | drugs9 > 1 | drugs10 > 1
          )
      )
  )

# EDS-GI
eds_gi <- gender_complete %>%
  select(record_id, gender_prob_1:gender_prob_19)

# EDS-SO
eds_so <- sex_or_complete %>%
  select(record_id, orientation_prob1:orientation_prob17)

# Combine the data
scop_factor_analysis <- scop_combined %>%
  select(record_id) %>%
  left_join(fppu) %>%
  left_join(eds_gi) %>%
  left_join(eds_so)

# SAVE NEW DATASET --------------------------------------------------------

write_csv(scop_eds_orientation, "data/cleaned/cleaned_eds_orientation.csv")
write_csv(scop_eds_gender, "data/cleaned/cleaned_eds_gender.csv")
write_csv(scop_eds_combined_1, "data/cleaned/cleaned_eds_combined.csv")
write_csv(freq_polydrug_use, "data/cleaned/freq_polydrug_use.csv")
write_csv(scop_factor_analysis, "data/cleaned/scop_factor_analysis.csv")
