#! /usr/bin/env Rscript

##### Parallelizing RQA radius search: "Friend" partner #####

# With thanks to https://github.com/FredHutch/slurm-examples/blob/master/centipede/example.R,
# https://sph.umich.edu/biostat/computing/cluster/examples/r.html,
# and Pariksheet Nanda (University of Connecticut).

# grab the environment variables passed from sbatch
n = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

# figure out which job we're running
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1)
  stop("Missing argument for job_number")
n <- as.integer(args[1])
print(paste0('Job number: ', n))

# create new output directory
dir_output <- Sys.getenv("OUTPUT_DIR", unset = NA)
dir_output <- ifelse(is.na(dir_output), ".", dir_output)
Rdata_output <- file.path(dir_output, paste0(n, ".RData"))
csv_output <- file.path(dir_output, paste0(n, ".csv"))
message("Output CSV file: ", csv_output)

# load libraries (quietly)
suppressPackageStartupMessages({
  library(crqa)
  library(dplyr)
  library(tidyverse)
})

# set seed
set.seed(999)

# read in raw dataset
conversation_df = read.table('./data/pac-filtered_movement_data.csv',
                             sep=',',header=TRUE) %>%

  # spread data to wide form
  tidyr::spread(interlocutor, movement) %>%
  rename(movement_0 = '0',
         movement_1 = '1') %>%

  # rescale movement time series
  dplyr::select(participant_id, task, partner_type,
                movement_0, movement_1) %>%
  group_by(participant_id, task, partner_type) %>%
  mutate(rescale.movement_0 = scale(movement_0),
         rescale.movement_1 = scale(movement_1)) %>%
  ungroup()

# read in the RQA parameters
friend_search_df = suppressMessages(
  read_csv('./data/crqa_parameters/crqa-radius_search-friend.csv')
) %>%

  # filter to this job's target parameters
  dplyr::filter(job_number == n)

# figure out current parameters
chosen_radius = friend_search_df$radius
chosen_delay = friend_search_df$ami.selected
chosen_embed = friend_search_df$embed.selected
current_participant = friend_search_df$participant_id
current_task = friend_search_df$task
current_partner = "Friend"

# cull the dataframe to the current set
next.conv = conversation_df %>% ungroup() %>%
  dplyr::filter(participant_id == current_participant &
                  task == current_task &
                  partner_type == current_partner)

# print updates
print(paste0("Running: ",
             "Participant ", current_participant, ", ",
             "Partner Friend, ",
             "Task ", current_task))
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

# create a new dataframe for output
result_df <- cbind.data.frame(job_number = n,
                              current_participant,
                              current_task,
                              current_partner,
                              chosen_delay,
                              chosen_embed,
                              chosen_radius,
                              rr,
                              from_target)

# save the entire session
save.image(file = Rdata_output)

# save the result dataframe
write.table(x = result_df,
            file=csv_output,
            sep=',',
            col.names=TRUE,
            row.names=FALSE)

# print space
print(paste0(""))
