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

#### 3. Figure out if we need to keep searching ####

# identify who's beyond our tolerance
recheck_radii_df = data.frame(selected_radii) %>%
  dplyr::filter(from_target >= 1)

# specify the radii that we'll check
old_maximum = max(selected_radii$chosen_radius)
radius_list = seq(old_maximum+.01, old_maximum+3, .01)
radius_recheck_search = expand.grid(radius_list,
                                 unique(recheck_radii_df$current_participant)) %>%

  # convert to dataframe and rename variables
  #data.frame(.) %>%
  rename(radius = Var1,
         current_participant = Var2) %>%

  # combine to winnow to only the ones we need
  left_join(recheck_radii_df, radius_grid_search,
            by = c('current_participant')) %>%
  select(-job_number) %>%

  # update job job_numbers
  group_by(current_partner) %>%
  mutate(job_number = row_number() + max(radius_dt$job_number)) %>%
  ungroup() %>%

  # rename variables so that the sbatch code doesn't throw errors
  rename(participant_id = current_participant,
         partner_type = current_partner,
         task = current_task,
         ami.selected = chosen_delay,
         embed.selected = chosen_embed) %>%

  # order variables according to append
  dplyr::select(radius, participant_id, task, partner_type,
               ami.selected, embed.selected, job_number)

# create friend df
friend_search_df = radius_recheck_search %>% ungroup() %>%
  dplyr::filter(partner_type == "Friend")

# create prof df
prof_search_df = radius_recheck_search %>% ungroup() %>%
  dplyr::filter(partner_type == "Prof")

# save friend to file
write.table(x = friend_search_df,
            file="./data/crqa_parameters/crqa-radius_search-friend.csv",
            sep=',',
            col.names=FALSE,
            row.names=FALSE,
            append=TRUE)

# save prof to file
write.table(x = prof_search_df,
            file="./data/crqa_parameters/crqa-radius_search-prof.csv",
            sep=',',
            col.names=FALSE,
            row.names=FALSE,
            append=TRUE)

#### 4. Export chosen radii for all conversations ####

# save to files
write.table(selected_radii,
            './data/crqa_parameters-pac.csv',
            sep=',', row.names=FALSE, col.names=TRUE)
