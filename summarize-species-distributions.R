## Author: Selene Banuelos
## Date: 7/2/2025
## Description: summarize and visualize PM2.5 species distributions

### load packages 
library(tidyverse)

### load data 

# month average mapped exposure concentrations across March 2016 - Dec 2019 for 
# all species
monthly_pollution <- read.csv('data-processed/pearls/monthly-pollution-long.csv')

# biomass burning contributions to month average mapped exposure concentrations 
# across March 2016 - Dec 2019 for all species
monthly_biomass <- read.csv('data-processed/pearls/monthly-pollution-long-biomass.csv')

### data wrangling

# calculate average residentially mapped concentration per month of every year 
# for every PM2.5 species across all participants
month_avg <- monthly_pollution %>%
    # calculate avg concentration per species per month of every year
    group_by(pollutant, year, month) %>%
    summarise(month_avg = mean(ug_m3, na.rm = TRUE)) %>%
    ungroup(.)

# calculate percentage mass fraction of species to total PM2.5 mass
perc_mass_frac <- month_avg %>%
    group_by(year, month) %>%
    # partial pivot wider: pivot total pm2.5 data from long format to wide (so 
    # not included in calculation for percent mass faction of total)
    filter(pollutant == 'pm2.5') %>%
    pivot_wider(names_from = pollutant,
                values_from = month_avg) %>%
    inner_join(
        .,
        filter(month_avg, pollutant != 'pm2.5')
    ) %>%
    rename(month_total_pm2.5 = 'pm2.5') %>%
    # calculate percentage mass fraction of chemical species to total PM2.5 for
    # each month of each year
    mutate(month_sum_species = sum(month_avg),
           perc_of_sum_species = (month_avg / month_sum_species)
    ) %>%
    ungroup(.) %>%
    # convert all upper case month abbreviations to title case
    mutate(month = stringr::str_to_title(month),
           # factor months to display in order in plot
           month = factor(month, levels = rev(month.abb))
    )

# calculate biomass burning contributions to average residentially mapped 
# concentration per month of every year for every PM2.5 species
month_avg_biomass <- monthly_biomass %>%
    group_by(pollutant, year, month) %>%
    summarise(month_avg = mean(ug_m3, na.rm = TRUE)) %>%
    ungroup(.) 

biomass_total_pm <- month_avg_biomass %>%
    # keep only contributions to total PM2.5 concentrations
    filter(pollutant == 'pm2.5') %>%
    rename(month_avg_biomass = month_avg) %>%
    full_join(.,
              filter(month_avg, pollutant == 'pm2.5'),
              by = c('year', 'month')
              ) %>%
    select(-contains('pollutant')) %>%
    rename(totalpm2.5_month_avg = month_avg) %>%
    mutate(month_avg_nonbiomass = totalpm2.5_month_avg - month_avg_biomass) %>%
    select(-totalpm2.5_month_avg) %>%
    pivot_longer(., 
                 cols = c(month_avg_biomass, month_avg_nonbiomass),
                 names_to = 'source',
                 names_prefix = 'month_avg_',
                 values_to = 'concentration'
                 ) %>%
    mutate(month = str_to_title(month),
           month = factor(month, levels = rev(month.abb))
    )

# calculate percentage of biomass burning contributions to total PM2.5 concentrations
perc_biomass_total_pm <- month_avg_biomass %>%
    pivot_wider(.,
                names_from = pollutant,
                values_from = month_avg
                ) %>%
    mutate(percent_biomass = (biomass_burning / pm2.5),
           percent_other = (1 - percent_biomass)
           ) %>%
    pivot_longer(.,
                cols = c(percent_biomass, percent_other),
                names_to = 'pollutant',
                names_prefix = 'percent_',
                values_to = 'percent'
                ) %>%
    mutate(pollutant = case_when(pollutant == 'biomass' ~ 'Biomass Burning',
                                 pollutant == 'other' ~ 'Total PM2.5'
                                 )
           )

### data visualization

# horizontal stacked bar plots that shows all mapped concentrations stacked by 
# month and PM2.5 species for every year
ggplot(data = perc_mass_frac, 
       aes(x = month, 
           y = perc_of_sum_species, 
           fill = pollutant
           )
       ) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    facet_wrap(~ year, ncol = 1) +
    labs(
        title = 'Monthly Percentage Mass Fraction of PM2.5 Species by Year',
        x = NULL,
        y = 'Percentage (%)'
        ) +
    theme_minimal()

# horizontal stacked bar plots that shows biomass burning contribution to 
# residentially mapped total PM2.5 concentrations by month for every year
ggplot(data = biomass_total_pm,
       aes(x = month,
           y = concentration,
           fill = source
           )
       ) +
    geom_bar(stat = 'identity') +
    # make bar plots horizontal by flipping x and y
    coord_flip() + 
    facet_wrap(~ year, ncol = 1) +
    labs(
        title = 'Contribution from Biomass Burning to total PM2.5 Concentrations by Year',
        x = NULL,
        y = 'Concentration ug/m^3'
    ) +
    # create break in conc axis
    ggbreak::scale_y_break(c(14.5, 36.5)) + 
    theme_minimal()

# consider changing color pallete to Viridis for more color blind friendly figure
### ouput

### end

