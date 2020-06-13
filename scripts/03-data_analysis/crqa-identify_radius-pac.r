#### crqa-identify_radius-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script identifies the target radius from the output files of the
# radius exploration step.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 12 June 2020
#####################################################################################

#### 1. Preliminaries ####

# read in libraries and functions
source('./scripts/03-data_analysis/libraries_and_functions-pac.r')

# prep workspace and libraries
invisible(lapply(c('dplyr',
                 'data.table'),
                 require,
                 character.only = TRUE))

#### 2. Identify target radii ####

# identify file names files
friend_radius_exploration_files = list.files('data/crqa_parameters/radius_exploration/parallel_test-radius_exploration-friend/',
                                      pattern = '*.csv',
                                      full.names = TRUE)
prof_radius_exploration_files = list.files('data/crqa_parameters/radius_exploration/parallel_test-radius_exploration-prof/',
                                      pattern = '*.csv',
                                      full.names = TRUE)
radius_exploration_files = c(friend_radius_exploration_files,
                             prof_radius_exploration_files)

# quickly bind all of the resulting CSVs into a single datatable
# (with thanks to https://stackoverflow.com/a/21156513)
radius_results <- lapply(radius_exploration_files, fread, sep=",")
radius_dt <- rbindlist( radius_results )

# grab line with smallest distance RR from target
selected_radii = setDT(radius_dt)[, min(from_target),
               by = list(current_participant,
                         current_partner,
                         current_task)]
selected_radii = setnames(selected_radii, "V1", "from_target")

# combine with the larger dataset so that we get the associated parameters
# (with thanks to https://stackoverflow.com/a/41838383)
selected_radii = radius_dt[selected_radii,
                           on = .(current_participant,
                                  current_partner,
                                  current_task,
                                  from_target),
                           mult = "first"]

#### 6. Export chosen radii for all conversations ####

# save to files
write.table(selected_radii,
            './data/crqa_parameters-pac.csv',
            sep=',', row.names=FALSE, col.names=TRUE)
