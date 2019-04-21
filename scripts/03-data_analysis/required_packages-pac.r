#### required_packages-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script downloads packages required by data preparation and analysis.
# Run this prior to any other scripts.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 15 July 2018
#####################################################################################

# list of required packages as strings
required_packages = c(
  'plyr',
  'dplyr',
  'stringr',
  'data.table',
  'lme4',
  'ggplot2',
  'pander',
  'gridExtra',
  'plotrix',
  'gtable',
  'viridis',
  'jsonlite',
  'tidyr',
  'tibble',
  'RCurl',
  'signal',
  'tseriesChaos',
  'nonlinearTseries',
  'crqa',
  # 'beepr',
  'quantmod'
)

# install missing packages (adapted from <http://stackoverflow.com/a/4090208>)
missing_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}


