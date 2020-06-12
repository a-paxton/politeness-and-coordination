#### crqa-continuous_parameters-embedding_dim-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script identifies the embedding dimension parameter for continuous cross-recurrence analysis
# by through the Cao method, implemented through the `nonlinearTseries` function, 
# `estimateEmbeddingDim`.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 10 June 2020
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

# spread data to wide form
conversation_df = spread(conversation_df, interlocutor, movement) %>%
  rename(movement_0 = '0',
         movement_1 = '1')

# read in AMI dataframe for delay
amis = read.table('./data/crqa_parameters/ami_calculations-pac.csv',
                  sep=',',header=TRUE)

# merge data with delay
conversation_df = full_join(conversation_df, amis,
                            by=c('participant_id',
                                 'partner_type',
                                 'task'))

#### 2. Determining embedding dimension with Cao's method ####

# set parameters
true_max = 10
max_dim = true_max + 1

# create empty dataframe
embed_df = data.frame()

# cycle through each task
convo.dfs = split(conversation_df,
                  list(conversation_df$participant_id,
                       conversation_df$task,
                       conversation_df$partner_type))

next_conv = conversation_df %>% ungroup() %>%
  dplyr::filter(participant_id == 1,
                partner_type == "Friend",
                task == "Convo")

# cycle through each conversation
for (next_conv in convo.dfs){
  if (dim(data.frame(next_conv))[1] > 0){
    
    # clean up task data
    next_conv_df = data.frame(next_conv)
    
    # print update
    print(paste0("Beginning FNN calculations for ",
                 "Participant ", unique(next_conv_df$participant_id),", ",
                 "Task `", unique(next_conv_df$task),"`, ",
                 "Partner `",unique(next_conv_df$partner_type),"`"))
    
    # get the AMI value for that task
    next_ami = unique(next_conv_df$ami.selected)
    
    # using Cao's method to determine embedding dimension (good for noisy data)
    embed_0 = tryCatch({
      nonlinearTseries::estimateEmbeddingDim(next_conv_df$movement_0,
                                             time.lag = next_ami,
                                             max.embedding.dim = max_dim,
                                             do.plot = FALSE)
    }, error = function(error) { return(NA) })
    embed_1 = tryCatch({
      nonlinearTseries::estimateEmbeddingDim(next_conv_df$movement_1,
                                             time.lag = next_ami,
                                             max.embedding.dim = max_dim,
                                             do.plot = FALSE)
    }, error = function(error) { return(NA) })
    
    # bind everything to data frame and replace 0 with NA
    next_conv_df = next_conv_df %>% ungroup() %>%
      mutate(embed.0 = ifelse(embed_0 == 0,
                              NA,
                              embed_0)) %>%
      mutate(embed.1 = ifelse(embed_1 == 0,
                              NA,
                              embed_1)) %>%
      mutate(embed.selected = max(embed_0, embed_1, 
                                  na.rm = FALSE))
    embed_df = rbind.data.frame(embed_df, next_conv_df)
  }}

#### 3. Write to file ####

# grab only the info we need
embed_param_only_df = embed_df %>% ungroup() %>%
  select(participant_id, task, partner_type, contains('embed')) %>%
  distinct() %>%
  mutate(embed.selected = ifelse(embed.selected == 0,
                                 NA,
                                 embed.selected))

# write to file
write.table(embed_param_only_df,'./data/crqa_parameters/embed_calculations-pac.csv',
            sep=',',row.names=FALSE,col.names=TRUE)