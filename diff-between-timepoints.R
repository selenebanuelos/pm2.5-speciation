## Author: Selene Banuelos
## Date: 9/18/2025
## Description: Summarize differences in PM2.5 concentration, PM2.5 component
## concentrations, and allostatic load scores between timepoints 2 and 4
## Will be using year-averaged exposure concentrations

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen = 999)

# read in data #################################################################
# time-weighted total PM2.5 and component data
load('data-processed/pearls/time-weighted-averages.RData')

# allostatic load score data
al_scores <- read.csv('data-processed/pearls/AL-scores.csv') %>%
    # keep only AL scores and remove high risk categorizations
    select(pearls_id, visitnum, AL_score)

# demographics data
demo <- read.csv('data-raw/pearls_dataset_2022-07-08.csv')

# data wrangling ###############################################################
# combine t2 and t4 pollution data
pm <- t2 %>%
    rbind(t4) %>%
    # keep only year-averaged exposure concentrations
    filter(averaged_over == 'year') %>%
    select(-averaged_over) %>%
    # strip '_ugm3' suffix from col names
    rename_with(~stringr::str_remove(., '_ugm3$'))

# combine pm2.5 and component concentrations with AL scores
pm_al_all <- full_join(al_scores, pm, by = c('pearls_id', 'visitnum')) 

# how many participants have outcome and exposure at both time points? #########
complete_cases <- pm_al_all %>% 
    # widen data, append timepoint as suffix to each var
    pivot_wider(.,
                names_from = visitnum,
                values_from = c(3:11),
                names_glue = '{.value}_{visitnum}'
    ) %>%
    filter(complete.cases(.))

nrow(complete_cases) 
# 151 participants

# how much do exposure concentrations and outcome score vary between visits? ###
differences <- pm_al_all %>%
    # reformat data to facilitate difference calculation
    pivot_longer(.,
                 !c(pearls_id, visitnum),
                 names_to = 'variable',
                 values_to = 'measure') %>%
    pivot_wider(., 
                names_from = visitnum,
                names_prefix = 't',
                values_from = measure) %>%
    # filter for complete cases 
    filter(complete.cases(.)) %>%
    # calculate absolute difference in measurement between the two visits
    mutate(difference = abs(t2-t4))

# calculate summary statistics for differences in values for exposures & outcome
diff_stats <- differences %>%
    select(variable, difference) %>%
    group_by(variable) %>%
    summarize(median_diff = median(difference),
              min_diff = min(difference),
              max_diff = max(difference),
              q25 = quantile(difference, 0.25),
              q75 = quantile(difference, 0.75)
              )

# how much time between the two time points? ###################################
time_elapsed <- demo %>%
    select(pearls_id, visitnum, form_date_exam_r) %>%
    # convert date variable from char to date type
    mutate(exam_date = as.Date(form_date_exam_r, format = '%m/%d/%Y')) %>%
    select(-form_date_exam_r) %>%
    filter(visitnum == 2 | visitnum == 4) %>%
    # spread data wider to facilitate calculation
    pivot_wider (.,
                 names_from = visitnum,
                 names_glue = '{.value}_{visitnum}',
                 values_from = exam_date
                 ) %>%
    mutate(time_elapsed = exam_date_4 - exam_date_2)

# calculate summary statistics for time elapsed between timepoints
time_stats <- time_elapsed %>%
    # filter for complete cases 
    filter(complete.cases(.)) %>%
    summarise(median_time = median(time_elapsed),
              min_time = min(time_elapsed),
              max_time = max(time_elapsed),
              q25 = quantile(time_elapsed, 0.25),
              q75 = quantile(time_elapsed, 0.75)
              )

# data visualization ###########################################################
# variables with difference on larger scale
large <- c('AL_score', 'pm2.5', 'om')

# histogram of exposures and outcome difference between time points
smaller_scale <- differences %>%
    # remove variables with differences of larger scale to improve visualization
    filter(!variable %in% large) %>%
    ggplot( 
       aes(x = difference,
           fill = variable)
       ) +
    geom_histogram(
        bins = 50) +
    facet_wrap(~variable) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(
        title = 'Difference in Concentrations Between Timepoints',
        x = 'Difference (ug/m^3)',
        y = 'Count of participants with given difference'
    )

# histogram of exposures and outcome difference between time points
larger_scale <- differences %>%
    # remove variables with differences of smaller scale to improve visualization
    filter(variable %in% large) %>%
    ggplot( 
        aes(difference,
            fill = variable)
    ) +
    geom_histogram(
        bins = 50) +
    facet_wrap(~variable) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(
        title = 'Difference in Concentrations or AL Scores Between Timepoints',
        subtitle = 'AL score difference shown in points, air pollution shown in ug/m^3',
        x = 'Difference',
        y = 'Count of participants with given difference'
    )

# histogram of time elapsed between two timepoints
time <- time_elapsed %>%
    filter(complete.cases(.)) %>%
    mutate(time_elapsed = as.numeric(time_elapsed)) %>%
    ggplot(
        aes(time_elapsed)
        )+
    geom_histogram(
        bins = 40,
        alpha = 0.3) +
    scale_x_continuous(n.breaks = 15) +
    theme_minimal() +
    labs(
        title = 'Time Elapsed Between T2 and T4',
        x = 'Days Elapsed',
        y = 'Count of participants with given difference'
    )

# output #######################################################################
# save histograms
ggsave(file = 'figures/measures-diff-timepoints-1.png', 
       smaller_scale)
ggsave(file = 'figures/measures-diff-timepoints-2.png', 
       larger_scale)
ggsave(file = 'figures/time-elapsed-timepoints.png', 
       time)