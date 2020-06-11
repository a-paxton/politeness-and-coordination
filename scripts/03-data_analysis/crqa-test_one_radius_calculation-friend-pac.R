#! /usr/bin/env Rscript

##### Parallelizing RQA radius search: Friend partner #####

# With thanks to https://github.com/FredHutch/slurm-examples/blob/master/centipede/example.R
# and https://sph.umich.edu/biostat/computing/cluster/examples/r.html.

# grab the environment variables passed from sbatch
n =as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1)
    stop("Missing argument for job_number")
n <- as.integer(args[1])
print(paste0('Job number:', n))

dir_output <- Sys.getenv("SCRATCH_DIR", unset = NA)
dir_output <- ifelse(is.na(dir_output), ".", dir_output)
file_output <- file.path(dir_output, paste0(n, ".RData"))
message("Output file: ", file_output)

# load libraries
suppressPackageStartupMessages({
library(crqa)
library(dplyr)
library(tidyverse)
})

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
                                 'task')) %>%
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

# choose radius input parameters for calculations
param <- suppressMessages(read_csv("./scripts/03-data_analysis/crqa_radius_parameters.csv")) %>%
      dplyr::filter(job_number == n)

# figure out current parameters
chosen_radius = param$radius
current_participant = param$participant_id
current_task = param$task
current_partner = "Friend"

# print updates
print(paste0("Running: ",
             "Participant ", current_participant, ", ",
             "Friend, ",
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

 result <- cbind.data.frame(job_number = n,
 	                    current_participant,
                               current_task,
                               current_partner,
                               chosen_delay,
                               chosen_embed,
                               chosen_radius,
                               rr,
                               from_target)
  # Save the entire session.
  save.image(file = file_output)
}

# print space
print(paste0(""))
