# Dependencies
library(tidyverse)
library(rio)
library(RBtest)

# Import
scop <- read_csv("data/cleaned/cleaned_demographics.csv")

# REPLACE MISSING DATA FOR SCALES -----------------------------------------

# Select the measures and a few demographic variables for testing missing values
scop_scales <- scop %>%
  select(record_id, age, trans, orientation, res1:res21, starts_with("res_scale"), unwanted,
         starts_with("orientation_prob"), starts_with("drugs"), starts_with("drug_affect"), 
         -contains("___"))
names(scop_scales)

# Check for complete cases
scop_scales_complete <- scop_scales[complete.cases(scop_scales), ]

# Test for missing data - resilience scales 
scop_scales_missing <- RBtest(scop_scales_complete)

# Types of missing variables
# -1 = complete variables
# 0 = MCAR (missing completely at random)
# 1 = MAR (missing at random)
table(scop_scales_missing$type)

# Check scores of EDS for sexual orientation
sex_orient_eds <- scop_scales_complete %>%
  select(record_id, starts_with("orientation_prob")) %>%
  gather(key = "item", value = "value", -record_id)
unique(sex_orient_eds$value)

# EDS is 1 to 4, as expected

# Subset the data frame based on complete cases
scop_2 <- scop %>%
  filter(record_id %in% scop_scales_complete$record_id) %>%
  # select variables for the analysis
  select(record_id:hiv_result, education, annual_income, res1:res21, starts_with("res_scale"), unwanted,
         starts_with("orientation_prob"), starts_with("drugs"), starts_with("drug_affect"), 
         -contains("___")) %>%
  # Recode the unwanted sexual contact variable to binary
  mutate(
    unwanted = recode(unwanted, "yes" = 1, "no" = 0)
  )

# CALCULATE SCORES --------------------------------------------------------

# Score Multidimensional Scale of Perceived Social Support (Zimet et al., 1988, 1990)
# Higher scores indicate more perceived social support
mspss_total <- scop_2 %>%
  select(record_id, res1:res12) %>%
  gather(key = "res", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(mspss_total = mean(score))

# Score Brief Resilience Scale (Smith et al., 2008)
# Higher scores indicate more resilience
brs_total <- scop_2 %>%
  select(record_id, res_scale1:res_scale6) %>%
  gather(key = "res_scale", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(brs_total = mean(score))

# Score the Expanded Everyday Discrimination Scale (Williams et al., 2008)
# Used the full version from Stone et al. (2020) due to superior distribution 
# Higher scores indicate more frequent experiences of discrimination
eds_orientation_total <- scop_2 %>%
  select(record_id, orientation_prob1:orientation_prob17) %>%
  gather(key = "orientation_eds", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(eds_orientation_total = mean(score))

# Mean score "drugs": problematic drug use frequency
# Higher scores indicate more frequent problematic drug use
freq_prob_drug_use <- scop_2 %>%
  select(record_id, drugs1:drugs10) %>%
  gather(key = "freq_prob_drug_use", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(freq_prob_drug_use = mean(score))

# Total score "drug_affects": Drug Abuse Screening Test (DAST-10)
# Higher scores indicate more severe problems related to drug abuse (NO ALCOHOL INCLUDED)
drug_abuse_severity <- scop_2 %>%
  select(record_id, drug_affects1:drug_affects10) %>%
  gather(key = "drug_abuse_severity", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Total score
  summarise(drug_abuse_severity = sum(score))

# Combine the main df with total scores
scop_3 <- scop_2 %>%
  left_join(mspss_total) %>%
  left_join(brs_total) %>%
  left_join(eds_orientation_total) %>%
  left_join(freq_prob_drug_use) %>%
  left_join(drug_abuse_severity) %>%
  # Drop individual items
  select(-starts_with("res"), -starts_with("orientation_prob"), 
         -starts_with("drugs"), -starts_with("drug_aff"))

# SAVE NEW DATASET --------------------------------------------------------

write_csv(scop_3, "data/cleaned/cleaned_cep-637.csv")
