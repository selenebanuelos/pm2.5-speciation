## Author: Selene Banuelos
## Date: 6/2/2025
## Description: Map PM2.5 composition data from Washington University in 
## St. Louis's Atmospheric Compostion Analysis Group to PEARLS participant 
## geocoded residential addresses. These are biweekly mean estimated mass 
## concentrations reported in ug/m^3 at 0.01x0.01 degree resolution for 
## particulate matter total (PM2.5), sulfate (SO4), nitrate (NO3), 
## ammonium (NH4), organic matter (OM), black carbon (BC), mineral dust (DUST), 
## and sea salt (SS).
## 
## Note: data can be ASCII or NetCDF
## Data: https://wustl.app.box.com/s/tfyt4uyuzbt4hbnw7bhos16aep9b5u7g/folder/251943064650 (xNorthAmerica folders)

### load libraries
library(raster)
library(dplyr)
library(stringi)
library(purrr) # reduce()

### import custom functions
source('code/functions.R') # map_pollution()

### read in data
# define variable name for participant IDs
id_var <- 'pearls_id'

# import geocoded residential addresses
# must have coordinates stored in 2 variables named 'Latitude' and 'Longitude'
addresses <- read.csv('data-processed/pearls/pearls-geocoded-addresses.csv') %>%
  #filter(address_sub != "") %>% # remove participants missing address
  dplyr::select(c(id_var, 'Latitude', 'Longitude'))

# define folder paths for biweekly air pollution data
bc_path <- ('data-raw/biweekly/blackcarbon/') # bc = black carbon
dust_path <- ('data-raw/biweekly/dust/')
no3_path <- ('data-raw/biweekly/no3/') 
ss_path <- ('data-raw/biweekly/seasalt/') # ss = sea salt
pm_path <- ('data-raw/biweekly/pm2.5/') # pm = pm2.5
om_path <- ('data-raw/biweekly/organicmatter/') # om = organic matter
ss_path <- ('data-raw/biweekly/seasalt/') # ss = sea salt
so4_path <- ('data-raw/biweekly/so4/')
nh4_path <- ('data-raw/biweekly/nh4/')


# create list of all file names in data directories
bc_files <- list.files(bc_path)
dust_files <- list.files(dust_path)
no3_files <- list.files(no3_path)
ss_files <- list.files(ss_path)
pm_files <- list.files(pm_path)
om_files <- list.files(om_path)
so4_files <- list.files(so4_path)
nh4_files <- list.files(nh4_path)

### implementation
# map biweekly air pollution data to each participant's residential address
pm <- map(.x = pm_files, # apply function to list of files
          ~map_pollution( # specify function to be applied
            data_path = pm_path, # path where files stored
            file_name = .x, # apply to each file in list
            ids = id_var,
            date_pattern = '\\d{7}-\\d{7}',
            pollutant = 'pm2.5'
          )
) %>%
  reduce(., left_join, by = id_var) # join all files into one df

bc <- map(bc_files, 
          ~map_pollution(
            bc_path, 
            .x, 
            id_var,
            date_pattern = '\\d{7}-\\d{7}', 
            pollutant = 'bc'
          )
) %>%
  reduce(., left_join, by = id_var)

dust <- map(dust_files, 
            ~map_pollution(
              dust_path, 
              .x, 
              id_var,
              date_pattern = '\\d{7}-\\d{7}', 
              pollutant = 'dust'
            )
) %>%
  reduce(., left_join, by = id_var)

no3 <- map(no3_files, 
           ~map_pollution(
             no3_path, 
             .x,
             id_var,
             date_pattern = '\\d{7}-\\d{7}', 
             pollutant = 'no3'
           )
) %>%
  reduce(., left_join, by = id_var)

ss <- map(ss_files, 
          ~map_pollution(
            ss_path, 
            .x,
            id_var,
            date_pattern = '\\d{7}-\\d{7}', 
            pollutant = 'ss'
          )
) %>%
  reduce(., left_join, by = id_var)

om <- map(om_files,
          ~map_pollution(
            om_path, 
            .x,
            id_var,
            date_pattern = '\\d{7}-\\d{7}', 
            pollutant = 'om'
          )
) %>%
  reduce(., left_join, by = id_var)

so4 <- map(so4_files, 
           ~map_pollution(
             so4_path, 
             .x,
             id_var,
             date_pattern = '\\d{7}-\\d{7}', 
             pollutant = 'so4'
           )
) %>%
  reduce(., left_join, by = id_var)

nh4 <- map(nh4_files, 
           ~map_pollution(
             nh4_path, 
             .x,
             id_var,
             date_pattern = '\\d{7}-\\d{7}', 
             pollutant = 'nh4'
           )
) %>%
  reduce(., left_join, by = id_var)

# all pollutants are reported in ug/m^3 units

### output
# remove all objects/functions that are no longer needed
rm(list = ls(pattern = "_files"))
rm(list = ls(pattern = "_path"))
rm(addresses, id_var)
rm(list = lsf.str()) # remove all functions

# save environment to reload objects for later use
save.image('data-processed/pearls/map-residential-pollution-biweekly.RData')

### end