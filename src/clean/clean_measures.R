# Dependencies
library(tidyverse)
library(rio)
library(RBtest)

# Import
scop <- read_csv("data/cleaned/cleaned_demographics.csv")

# REPLACE MISSING DATA FOR SCALES -----------------------------------------

# Select the measures and a few demographic variables for testing missing values
scop_scales <- scop %>%
  select(record_id, age, trans, orientation, res1:res21, starts_with("res_scale"), starts_with("gender_prob_"), 
         starts_with("orientation_prob"), starts_with("drugs"), starts_with("drug_affect"), -contains("___"))
names(scop_scales)

# Test for missing data
scop_scales_missing <- RBtest(scop_scales)

# Types of missing variables
# -1 = complete variables
# 0 = MCAR (missing completely at random)
# 1 = MAR (missing at random)
table(scop_scales_missing$type)

# CALCULATE SCORES --------------------------------------------------------

# Check intracommunity prejudice
intracomm_eds <- scop %>%
  select(record_id, ends_with("___5")) %>%
  select(-gender___5, -race___5) %>%
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
scop_2 <- left_join(scop, intracomm_eds_1)

# Score Multidimensional Scale of Perceived Social Support (Zimet et al., 1988, 1990)
scop_2 %>%
  select(record_id, res1:res12) %>%
  gather(key = "res", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(mspss_total = mean(score))

# Score Brief Resilience Scale (Smith et al., 2008)
scop_2 %>%
  select(record_id, res_scale1:res_scale6) %>%
  gather(key = "res_scale", value = "score", -record_id) %>%
  group_by(record_id) %>%
  # Mean score
  summarise(mspss_total = mean(score))