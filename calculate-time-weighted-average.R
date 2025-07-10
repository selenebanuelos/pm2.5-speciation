## Author: Selene Banuelos
## Date: 6/1/2025
## Description: Calculate time-weighted average 12-month, 1-month, 
## and 2 week pollution concentration prior to each participant's 
## biospecimen collection for all chemical components of PM2.5 using biweekly 
## average data
##
## Note 1: calculating time-weighted concentrations using Austin's equation from 
## De la Rosa (2025) DOI: 10.1097/PSY.0000000000001276

### load libraries & settings
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
options(scipen = 999) # don't use scientific notation

### import custom functions
source('code/functions.R') # month_interval(), year_interval(), days_overlap()

### read in data
# define variable name for participant IDs
id_var <- 'pearls_id'

# import dataset with date of exam where biospecimen were collected
demo <- read.csv('data-raw/pearls_dataset_2022-07-08.csv')
  
# load mapped residential pollution data
load('data-processed/pearls/map-residential-pollution-biweekly.RData')

# define timepoint of interest
timepoint = 2

### data cleaning
# convert exam date to date object for calculation of exposure time period
exam_date <- demo %>%
  filter(visitnum == timepoint) %>% # only keep timepoint 2 data
  filter(!is.na(form_date_exam_r)) %>% # remove entries from no-shows to exam
  mutate(exam_date = as.Date(form_date_exam_r, format = '%m/%d/%Y')) %>%
  dplyr::select(pearls_id, exam_date)

# put all mapped pollution data into list of data frames
pollution_data <- list(bc, dust, nh4, no3, om, pm, so4, ss)

# transform pollution data into long format
pollution_long <- map(pollution_data,
                      ~pivot_longer(
                          data = .x, # apply to each df in list
                          cols = !pearls_id,
                          names_to = c('pollutant', 
                                       'exposure_period_start', 
                                       'exposure_period_end'
                          ),
                          # break original variable name at hyphens into 3 groups
                          names_pattern = '([^-]+)_([^-]+)_([^-]+)', 
                          # convert ordinal dates to date objects
                          names_transform = list(
                              exposure_period_start = ~as.Date(.x, format = '%Y%j'),
                              exposure_period_end = ~as.Date(.x, format = '%Y%j')
                          ),
                          values_to = 'ug_m3'
                      )
) %>%
    bind_rows(.) # combine all pollution data into one data frame

### implementation
### year-prior =================================================================
# calculate average time-weighted exposure year prior to biospecimen collection
year_avg_exposure <- left_join(exam_date, pollution_long, by = 'pearls_id') %>%
  
  # calculate interval for exposure period
  mutate(exposure_int = interval(exposure_period_start, exposure_period_end)) %>%
  
  # calculate interval for year leading up to exam date
  mutate(period_prior_int = year_prior(exam_date)) %>%
  
  # calculate time weighed average exposure over the year prior to exam date
  time_weighted_avg(., 'year', id_var)

### month-prior ================================================================
# calculate average time-weighted exposure month prior to biospecimen collection
month_avg_exposure <- left_join(exam_date, pollution_long, by = 'pearls_id') %>%
  
  # calculate interval for exposure period
  mutate(exposure_int = interval(exposure_period_start, exposure_period_end)) %>%
  
  # calculate interval for month leading up to exam date
  mutate(period_prior_int = month_prior(exam_date)) %>%

  # calculate time weighed average exposure over the month prior to exam date
  time_weighted_avg(., 'month', id_var)
  
### 14 days-prior ================================================================
# calculate moving time-weighted average exposure 14 days prior to biospecimen collection
two_week_avg_exposure <- left_join(exam_date, pollution_long, by = 'pearls_id') %>%
  
  # calculate interval for exposure period
  mutate(exposure_int = interval(exposure_period_start, exposure_period_end)) %>%
  
  # calculate interval for 14 days leading up to exam date
  mutate(period_prior_int = two_week_prior(exam_date)) %>%
  
  # calculate time weighed average exposure over the 14 days prior to exam date
  time_weighted_avg(., 'two_week', id_var)

### data cleaning
# transform year-prior time weighted average pollution data into wide format
year_wide <- year_avg_exposure %>%
  dplyr::select(pearls_id, pollutant, year_average) %>%
  distinct(.) %>% # remove identical rows
  pivot_wider(
    id_cols = pearls_id,
    names_from = pollutant,
    names_glue = '{pollutant}_ugm3',
    values_from = year_average
  ) %>%
  mutate(visitnum = timepoint, # using 'visitnum' to match original varname
         averaged_over = 'year')

# transform month-prior time weighted average pollution data into wide format
month_wide <- month_avg_exposure %>%
  dplyr::select(pearls_id, pollutant, month_average) %>%
  distinct(.) %>% # remove identical rows
  pivot_wider(
    id_cols = pearls_id,
    names_from = pollutant,
    names_glue = '{pollutant}_ugm3',
    values_from = month_average
  ) %>%
  mutate(visitnum = timepoint,
         averaged_over = 'month')

# transform 14 day-prior time weighted average pollution data into wide format
two_week_wide <- two_week_avg_exposure %>%
  dplyr::select(pearls_id, pollutant, two_week_average) %>%
  distinct(.) %>% # remove identical rows
  pivot_wider(
    id_cols = pearls_id,
    names_from = pollutant,
    names_glue = '{pollutant}_ugm3',
    values_from = two_week_average
  ) %>%
  mutate(visitnum = timepoint,
         averaged_over = '14 days')

### output
# merge data from all time periods together & give dataframe identifying name
t2 <- rbind(year_wide, month_wide, two_week_wide)

# remove all objects/functions except for final dataframe
rm(list = setdiff(ls(), 't2'))

# load existing .RData 
load('data-processed/pearls/calculate-time-weighted-average.RData')

# save data from current environment with existing .RData
save.image('data-processed/pearls/calculate-time-weighted-average.RData')

### end