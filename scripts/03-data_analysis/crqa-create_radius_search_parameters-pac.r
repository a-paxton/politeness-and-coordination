#### crqa-create_radius_search_parameters-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script sets the parameters for our radius search for cross-recurrence
# quantification analysis.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 11 June 2020
#####################################################################################

#### 1. Preliminaries ####

# read in libraries and functions
source('./scripts/03-data_analysis/libraries_and_functions-pac.r')

# prep workspace and libraries
invisible(lapply(c('tseriesChaos',
                   'nonlinearTseries',
                   'crqa',
                   'quantmod',
                   'cowplot'),
                 require,
                 character.only = TRUE))

# read in conversation_df dataset
conversation_df = read.table('./data/pac-filtered_movement_data.csv',
                             sep=',',header=TRUE)

# specify the radii that we'll check
radius.list = seq(.01, 22, .01)

# create parameter search
radius_grid_search = expand.grid(radius.list,
                                 unique(conversation_df$participant_id),
                                 unique(conversation_df$task)) %>%
  rename(radius = Var1,
         participant_id = Var2,
         task = Var3) %>%
  rownames_to_column(var = "job_number")

# save to file
write.table(x = radius_grid_search,
            file="./data/crqa_parameters/crqa_radius_parameters.csv",
            sep=',',
            col.names=TRUE,
            row.names=FALSE)
