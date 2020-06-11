#! /usr/bin/env Rscript

##### Parallelizing RQA radius search: Professor partner #####

# With thanks to https://github.com/FredHutch/slurm-examples/blob/master/centipede/example.R
# and https://sph.umich.edu/biostat/computing/cluster/examples/r.html.

# grab the environment variables passed from sbatch
n =as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
search_start_value = as.numeric(Sys.getenv('search_start_value'))
search_end_value = as.numeric(Sys.getenv('search_end_value'))
print(paste0('Search start value:', search_start_value))
print(paste0('Search end value:', search_end_value))

# load libraries
library(crqa)
library(dplyr)
library(tidyverse)

# set seed
set.seed(999)

# read in conversation_df dataset
conversation_df = read.table('./data/pac-filtered_movement_data.csv',
                             sep=',',header=TRUE)

# spread data to wide form
conversation_df = tidyr::spread(conversation_df, interlocutor, movement) %>%
  rename(movement_0 = '0',
         movement_1 = '1')

# read in the RQA parameters
amis = read.table('./data/crqa_parameters/ami_calculations-pac.csv',
                  sep=',', header=TRUE)
fnn.merged = read.table('./data/crqa_parameters/fnn_calculations-pac.csv',
                        sep=',', header=TRUE)

# join everything
conversation_df = full_join(conversation_df, amis,
                            by=c('participant_id',
                                 'partner_type',
                                 'task',
                                 'condition',
                                 'participant_gender')) %>%
  full_join(., fnn.merged,
            by=c('participant_id',
                 'partner_type',
                 'task')) %>%
  select(-ami.loc0, -ami.loc1, -embed.0, -embed.1)

# rescale movement time series
conversation_df_crqa = conversation_df %>% ungroup() %>%
  dplyr::select(participant_id, task, partner_type,
                movement_0, movement_1, ami.selected, embed.selected) %>%
  group_by(participant_id, task, partner_type) %>%
  mutate(rescale.movement_0 = scale(movement_0),
         rescale.movement_1 = scale(movement_1)) %>%
  ungroup()

# identify radius for calculations
radius.list = seq(search_start_value,
                  search_end_value,
                  by=.1)

# create a grid
radius_grid_search = expand.grid(radius.list,
                                 unique(conversation_df_crqa$participant_id),
                                 unique(conversation_df$task))

# figure out current parameters
current_test = radius_grid_search[n,]
chosen_radius = as.numeric(current_test$Var1)
current_participant = current_test$Var2
current_task = current_test$Var3
current_partner = "Prof"

# print updates
print(paste0("Running: ",
             "Participant ", current_participant, ", ",
             "Prof, ",
             "Task ", current_task))

# cull the dataframe to the current set
next.conv = conversation_df_crqa %>% ungroup() %>%
  dplyr::filter(participant_id == current_participant &
                  task == current_task &
                  partner_type == current_partner)

# quit if we couldn't identify an appropriate lag
if (unique(next.conv$embed.selected)[1] == 'Inf'){
  print(paste0("No suitable embedding dimension identified."))

} else {

  # identify parameters
  chosen_delay = as.numeric(unique(next.conv$ami.selected))
  chosen_embed = as.numeric(unique(next.conv$embed.selected))
  print(paste0('Chosen delay: ', chosen_delay))
  print(paste0('Chosen embedding dimension: ', chosen_embed))
  print(paste0('Chosen radius: ', chosen_radius))

  # run CRQA
  rec_analysis = crqa(next.conv$rescale.movement_0,
                      next.conv$rescale.movement_1,
                      delay = chosen_delay,
                      embed = chosen_embed,
                      r = chosen_radius,
                      normalize = 0,
                      rescale = 2,
                      mindiagline = 2,
                      minvertline = 2,
                      tw = 0,
                      whiteline = FALSE,
                      recpt = FALSE)

  # grab metrics
  rr = round(rec_analysis$RR, 5)
  from_target = abs(rr - 5)
  print(paste0("Distance from RR target: ",from_target))

  # save metrics to file
  write.table(cbind.data.frame(current_participant,
                               current_task,
                               current_partner,
                               chosen_delay,
                               chosen_embed,
                               chosen_radius,
                               rr,
                               from_target),
              paste('./data/crqa_parameters/radius_calculations/radius_calculations-mean_scaled-',
                    current_participant,'_',
                    current_task, '_',
                    current_partner, '-r',
                    chosen_radius,
                    '-pac.csv', sep=''),
              sep=',',row.names=FALSE,col.names=TRUE)
}

# print space
print(paste0(""))
