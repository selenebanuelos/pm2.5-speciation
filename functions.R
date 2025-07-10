## Author: Selene Banuelos
## Date: 5/8/2025
## Description: custom functions used in other scripts in this project

### map_pollution()
# function that maps pollution data to given geocoded addresses
map_pollution <- function(data_path, # path to data files for pollutant of interest
                          file_name, # name of single file or list of file names
                          ids, # name of variable with IDs
                          geocoded_addresses = addresses, # df with coordinates
                          date_pattern, # string: regex to extract date
                          pollutant) # string: pollutant being mapped
{
  # define path to file
  file_path <- paste0(data_path,file_name)
  
  # import NetCDF or ASCII data as raster
  pollution_data <- raster(file_path)
  
  # extract the date from file name using given pattern 
  date <- stri_extract_last_regex(file_name, date_pattern) %>%
    # replace any hyphens with underscores to make dates uniform
    stri_replace_all_fixed(., '-', '_') 
  
  # create variable name for pollution concentration data 
  pollutant_var <- paste0(pollutant,'_', date)
  
  # use coordinates to map pollutant measurements to participant addresses
  geocoded_addresses %>%
    mutate(!!sym(pollutant_var) := # use string in 'pollutant' object as new var name
             # extract pollutant measurements at address coordinates
             raster::extract(pollution_data, 
                             geocoded_addresses[, c('Longitude', 'Latitude')]
             )
    ) %>%
    dplyr::select(id_var, !!sym(pollutant_var)) # raster also has select()
}

### year_prior() 
# calculate time interval that spans entire year leading up to given day
year_prior <- function(date) # date of interest (date object)
{
  # if exam date is 1/25/2015, we want average exposure over 1/25/2014-1/24/2015
  # this is an average over start of 1/25/14 to the end of 1/24/15 (inclusive of
  # the entirety of 1/24/15 to avg over full 365 days)
  
  # identify day before the exam
  day_before <- date - days(1)
  
  # identify first day of year leading up to exam date
  first_day <- date - years(1)
  
  # return interval for year leading up to exam date
  interval(first_day, day_before)
}

### month_prior()
# calculate time interval that spans entire month leading up to given day
month_prior <- function(date) # date of interest (date object)
{
  # if exam date is 1/25/2015, we want average exposure over 12/25/2014-1/24/2015
  # this is an average over start of 12/25/14 to the end of 1/24/15 (inclusive of
  # the entirety of 1/24/15 to avg over full month)
  
  # identify day before the exam date
  day_before <- date - days(1)
  
  # identify first day of the month leading up to exam date
  first_day <- add_with_rollback(date, months(-1))
  
  # return interval for month leading up to exam date
  interval(first_day, day_before) 
}

### two_week_prior()
# calculate time interval that spans entire 14 days leading up to given day
two_week_prior <- function(date) # date of interest (date object)
{
  # if exam date is 1/25/2015, we want average exposure over 1/11/2015-1/24/2015
  # this is an average over start of 1/11/2015 to the end of 1/24/15 (inclusive of
  # the entirety of 1/24/15 to avg over full 14 days)
  
  # identify day before the exam date
  day_before <- date - days(1)
  
  # identify first day of month leading up to exam date
  first_day <- date - days(14)
  
  # return interval for month leading up to exam date
  interval(first_day, day_before) 
}

### time_weighted_avg()
# calculate the time weighted average over a given time interval
time_weighted_avg <- function(df, 
                              period, # ('year' or 'month' or 'two_week')
                              ids # variable name of subject ids
                              ) 
{
  # define column name for time-weighted average
  time_period_avg = paste0(period, '_average')
  
  # define columns to group by when calculating average exposure
  group_vars <- c(ids, 'pollutant')
  
  # df has the following 2 vars: exposure_int, period_prior_int
  # exposure_int = interval object spanning pollution data time period
  # period_prior_int = interval object spanning desired time prior to exam date
  df %>%
    # calculate # of days the time period prior overlaps with each exposure period
    mutate(days_within_exposure = days_overlap(exposure_int, period_prior_int)) %>%
    
    # calculate total # of days in exposure period (+1 for inclusion of last day)
    mutate(exposure_period_days = time_length(exposure_int, 'days') + 1) %>%
    
    # calculate the time-weighted exposure for each exposure period
    mutate(time_weighted_exposure = ug_m3*(days_within_exposure/exposure_period_days)) %>%
    
    # calculate average time-weighted exposure
    group_by(across(all_of(group_vars))) %>%
    mutate(!!time_period_avg := 
             sum(time_weighted_exposure) / (sum(days_within_exposure/exposure_period_days))) %>%
    ungroup(.)
}
  
### days_overlap()
# calculate number of days that overlap between two time intervals
days_overlap <- function(interval1=df$exposure_int, # beginning to end of exposure period
                         interval2=df$period_prior_int) # beginning to end of period leading up to exam
{
  # check if intervals overlap
  overlap <- lubridate::intersect(interval1, interval2) # interval object
  
  # return count of days in overlap (+ 1 to include last day in interval)
  case_when(
    is.na(overlap) ~ 0, # no overlap between intervals
    !is.na(overlap) ~  time_length(overlap, unit = 'days') + 1 # overlap
  )
}