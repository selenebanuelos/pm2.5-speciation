## Author: Selene Banuelos
## Date: 9/18/2025
## Description: Calculate allostatic load scores for all timepoints

### load libraries
library(dplyr)
library(purrr)

# import data ##################################################################
# blood analyte measurements and high-risk status (sd method) data
analytes <- read.csv('data-processed/pearls/analyte-highrisk-sd.csv')

# blood pressure percentile data
bp_percentile <- read.csv('data-processed/pearls/blood-pressure-percentiles.csv')

# resting heart rate data
rhr <- read.csv('data-processed/pearls/rhr-percentiles.csv')

# data wrangling ###############################################################
# join all data together, keeping only high risk indicator variables
biomarkers <- list(analytes, bp_percentile, rhr) %>%
    # keep only vars of interest
    map(.,
        ~select(.x, c('pearls_id', 'visitnum', contains('high_risk')))
        ) %>%
    # join together by pearls_id
    reduce(full_join, by = c('pearls_id', 'visitnum'))

# calculate AL scores (only for complete cases) ################################
al_scores <- biomarkers %>%
    # remove IL-10, IL-1B, IL-6, MPO, IGFBP-3
    select(-contains(c('IL.10', 'IL.1B', 'IL.6', 'MPO', 'IGFBP.3'))) %>%
    # remove participants missing any biomarkers
    filter(complete.cases(.)) %>%
    # count high risk biomarkers to creat AL score
    mutate(AL_score = rowSums(across(contains('high_risk')))) %>%
    # reorder cols
    select(pearls_id, visitnum, AL_score, everything())
    
# output #######################################################################
write.csv(al_scores,
          'data-processed/pearls/AL-scores.csv',
          row.names = FALSE)