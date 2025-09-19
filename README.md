### Description

Project focused on analysis of PM2.5 chemical species and relationship with adverse health outcomes. The data used for this project was created by Washington University in St. Louis's Atmospheric Compostion Analysis Group and can be found here: https://sites.wustl.edu/acag/datasets/surface-pm2-5/

These data are mean estimated mass concentrations reported in ug/m^3 at 0.01x0.01 degree resolution for total PM2.5, sulfate (SO4), nitrate (NO3), ammonium (NH4), organic matter (OM), black carbon (BC), mineral dust (DUST), and sea salt (SS).

### Table of Contents

**map-residential-pollution.R:** maps raw pollution data (netCDF) to geocoded addresses 

**calculate-time-weighted-average.R:** calculates average mapped pollution concentrations 2-week, 1-month, 1-year prior to given date

**summarize-species-distributions.R:** create visualizations of distributions of PM2.5 species as well as contribution of biomass burning to species and total PM2.5 concentrations

**data-wrangling.R:** miscellaneous data wrangling

**functions.R:** custom functions used across different scripts for project

**diff-between-timepoints.R** summarizes differences in PM2.5 concentration, PM2.5 component concentrations, and allostatic load scores between timepoints 2 and 4
