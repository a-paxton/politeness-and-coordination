#### continuous_rqa_parameters-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script combines the parameters identified over the previous steps 
# with the movement data, making it easier to run CRQA with the identified 
# parameters in the next section of the Rmarkdown.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 17 June 2020
#####################################################################################

#### 1. Preliminaries ####

# read in libraries and functions
source('./scripts/03-data_analysis/libraries_and_functions-pac.r')

# prep workspace and libraries
suppressPackageStartupMessages(lapply(c('tseriesChaos',
                                        'nonlinearTseries',
                                        'crqa',
                                        'quantmod',
                                        'cowplot'),
                                      require,
                                      character.only = TRUE))

# read in conversation_df dataset
conversation_df = read.table('./data/pac-filtered_movement_data.csv',
                             sep=',',header=TRUE)

# spread data to wide form
conversation_df = spread(conversation_df, interlocutor, movement) %>%
  rename(movement_0 = '0',
         movement_1 = '1')

#### 2. Read in identified parameters ####

# read in identified radius data
parameter_df = read.table('./data/crqa_parameters-pac.csv',
                          sep=',',header=TRUE) %>%
  
  # only keep those who were able to find a suitable radius
  dplyr::filter(from_target < 1) %>%
  
  # remove unneeded variables
  dplyr::select(-job_number)

# merge parameters with movement data
conversation_df = full_join(conversation_df, parameter_df,
                            by=c('participant_id' = "current_participant",
                                 'partner_type' = "current_partner",
                                 'task' = "current_task")) %>%
  
  # remove those who weren't in the parameter dataframe
  drop_na()

# save to files
write.table(conversation_df,'./data/crqa_data_and_parameters-pac.csv',
            sep=',',row.names=FALSE,col.names=TRUE)
