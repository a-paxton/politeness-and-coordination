#### required_packages-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script downloads packages required by data preparation and analysis.
# Run this prior to any other scripts.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 20 July 2020
#####################################################################################

# list of required packages as strings
required_packages = c(
  'dplyr',
  'stringr',
  'data.table',
  'lme4',
  'ggplot2',
  'pander',
  'grid',
  'gridExtra',
  'plotrix',
  'gtable',
  'viridis',
  'tidyr',
  'tibble',
  'RCurl',
  'TTR',
  'biwavelet',
  'doParallel',
  'foreach',
  'MASS',
  'parallel'
)

# install missing packages (adapted from <http://stackoverflow.com/a/4090208>)
missing_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos='http://cran.us.r-project.org')
}
