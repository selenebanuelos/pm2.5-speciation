## Author: Selene Banuelos
## Date: 6/6/2025
## Description: script is used to do miscellaneous data wrangling for project

### load libraries and set options
library(dplyr)
options(scipen = 999) # turn off scientific notation

# ==============================================================================
# merge time-averaged pollution concentration prior to biospecimen collection
# import all time-weighted average concentration data
load('data-processed/reach/calculate-time-weighted-average.RData')

# specify id variable name for this dataset
id_var <- 'pearls_id'
  
# combine total concentrations for each species and total pm2.5
avg_conc <- rbind(t2, t4) %>%
  arrange(!!sym(id_var)) %>% # use string stored in 'id_var' as arg for arrange()
  select(id_var, visitnum, averaged_over, everything())

# combine contribution from biomass burning for each species and total pm2.5
biomass <- rbind(t2_biomass, t4_biomass) %>%
  arrange(!!sym(id_var)) %>% 
  select(id_var, visitnum, averaged_over, everything())

# save as .csv
write.csv(x = avg_conc,
          file = 'data-processed/pearls/pm-species-avg-conc.csv',
          row.names = FALSE)

write.csv(x = biomass,
          file = 'data-processed/pearls/pm-species-avg-conc-biomass-burning.csv',
          row.names = FALSE)

# ==============================================================================
# merge all residentially mapped pollution data between March 2016-December 2019
# for all participants
# define variable name for participant IDs
id_var <- 'pearls_id'

# load mapped residential pollution data
load('data-processed/pearls/map-residential-pollution-monthly.RData')

# put all mapped pollution data into list of data frames
pollution_data <- list(bc, dust, nh4, no3, om, pm, so4, ss)

# transform pollution data into long format
pollution_long <- purrr::map(pollution_data,
                      ~tidyr::pivot_longer(
                          data = .x, # apply to each df in list
                          cols = !pearls_id,
                          names_to = c('pollutant', 
                                       'year', 
                                       'month'
                          ),
                          # break original variable name into 3 groups()
                          names_pattern = '([^_]+)_(\\d{4})_\\d{4}_([A-Z]{3})', 
                          values_to = 'ug_m3'
                      )
) %>%
    bind_rows(.) # combine all pollution data into one data frame

# save as .csv
write.csv(x = pollution_long,
          file = 'data-processed/pearls/monthly-pollution-long.csv',
          row.names = FALSE)

# ==============================================================================
# merge all biomass burning contributions to pollution data between 
# March 2016-December 2019 for all participants
# define variable name for participant IDs
id_var <- 'pearls_id'

# load mapped residential pollution data
load('data-processed/pearls/map-residential-pollution-monthly-biomass-burning.RData')

# put all mapped pollution data into list of data frames
pollution_data <- list(bc, dust, nh4, no3, om, pm, so4, ss)

# transform pollution data into long format
pollution_long <- purrr::map(pollution_data,
                      ~tidyr::pivot_longer(
                          data = .x, # apply to each df in list
                          cols = !pearls_id,
                          names_to = c('pollutant', 
                                       'year', 
                                       'month'
                          ),
                          # break original variable name into 3 groups()
                          names_pattern = '([^_]+)_(\\d{4})_([A-Z]{3})', 
                          values_to = 'ug_m3'
                      )
) %>%
    bind_rows(.) # combine all pollution data into one data frame

# save as .csv
write.csv(x = pollution_long,
          file = 'data-processed/pearls/monthly-pollution-long-biomass.csv',
          row.names = FALSE)
### end