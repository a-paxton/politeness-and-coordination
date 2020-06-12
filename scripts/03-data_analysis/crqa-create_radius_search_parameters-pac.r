#### crqa-create_radius_search_parameters-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script sets the parameters for our radius search for cross-recurrence
# quantification analysis. It removes any segments for which we were unable to
# identify suitable parameters.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 11 June 2020
#####################################################################################

#### 1. Preliminaries ####

# read in libraries and functions
source('./scripts/03-data_analysis/libraries_and_functions-pac.r')

# prep workspace and libraries
invisible(lapply(c('dplyr'),
                 require,
                 character.only = TRUE))

# read in conversation_df dataset
conversation_df = read.table('./data/pac-filtered_movement_data.csv',
                             sep=',',header=TRUE)

#### 2. Create radius list ####

# specify the radii that we'll check
radius.list = seq(.01, 22, .01)

# create parameter search
radius_grid_search = expand.grid(radius.list,
                                 unique(conversation_df$participant_id),
                                 unique(conversation_df$task),
                                 unique(conversation_df$partner_type)) %>%
  rename(radius = Var1,
         participant_id = Var2,
         task = Var3,
         partner_type = Var4)

#### 3. Combine with other parameters ####

# read in the RQA parameters
delay_df = read.table('./data/crqa_parameters/ami_calculations-pac.csv',
                      sep=',', header=TRUE)
embed_df = read.table('./data/crqa_parameters/embed_calculations-pac.csv',
                      sep=',',header=TRUE)

# join everything
parameter_df = full_join(conversation_df, delay_df,
                         by=c('participant_id',
                              'partner_type',
                              'task')) %>%
  full_join(., embed_df,
            by=c('participant_id',
                 'partner_type',
                 'task')) %>%
  
  # only take parameters for each
  select(-ami.loc0, -ami.loc1, 
         -embed.0, -embed.1,
         -frame, -t, -movement, -condition,
         -interlocutor, -participant_gender) %>%
  distinct() %>%
  
  # remove anyone who doesn't have a dimension
  drop_na(embed.selected)

# combine with grid search
radii_and_parameter_df = full_join(radius_grid_search,
                                   parameter_df,
                                   by=c('participant_id',
                                        'partner_type',
                                        'task'))

#### 4. Save separate partners to file ####

# create friend file
friend_search_df = radii_and_parameter_df %>% ungroup() %>%
  dplyr::filter(partner_type == "Friend") %>%
  rownames_to_column(var = "job_number")

# create prof file
prof_search_df = radii_and_parameter_df %>% ungroup() %>%
  dplyr::filter(partner_type == "Prof") %>%
  rownames_to_column(var = "job_number")

# save friend to file
write.table(x = friend_search_df,
            file="./data/crqa_parameters/crqa-radius_search-friend.csv",
            sep=',',
            col.names=TRUE,
            row.names=FALSE)

# save prof to file
write.table(x = prof_search_df,
            file="./data/crqa_parameters/crqa-radius_search-prof.csv",
            sep=',',
            col.names=TRUE,
            row.names=FALSE)
