#### continuous_rqa_parameters-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script explores the parameters for the continuous cross-recurrence analysis
# that we'll run over the movement data. Because this is a lengthy process,
# we create a series of files along the way that can be re-run in pieces if needed.
# This allows us to keep this file commented by default.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 18 July 2018
#####################################################################################

#### 1. Preliminaries ####

# read in libraries and functions
source('./scripts/03-data_analysis/libraries_and_functions-pac.r')

# prep workspace and libraries
invisible(lapply(c('tseriesChaos',
                   'nonlinearTseries',
                   'crqa',
                   # 'beepr',
                   'quantmod'), 
                 require, 
                 character.only = TRUE))

# read in conversation_df dataset
conversation_df = read.table('./data/pac-filtered_movement_data.csv',
                             sep=',',header=TRUE)

# spread data to wide form
conversation_df = spread(conversation_df, interlocutor, movement) %>%
  rename(movement_0 = '0',
         movement_1 = '1')

#### 2. Determine delay with average mutual information (AMI) ####

# set maximum AMI
ami.lag.max = 200

# get AMI lag for both participants in each conversation
conversation_df = conversation_df %>% ungroup() %>%
  group_by(participant_id, task, partner_type) %>%
  mutate(ami.loc0 = first_local_minimum(
    mutual(movement_0, lag.max = ami.lag.max, plot = FALSE))
    ) %>%
  mutate(ami.loc1 = first_local_minimum(
    mutual(movement_1, lag.max = ami.lag.max, plot = FALSE))
    ) %>%
  group_by(participant_id, task, partner_type, ami.loc0, ami.loc1) %>%
  distinct() %>%
  mutate(ami.selected = min(ami.loc1, ami.loc0, na.rm = TRUE)) %>%
  ungroup()

# write AMI information to file
amis = conversation_df %>%
  select(participant_id, task, partner_type, contains('ami')) %>%
  distinct()
write.table(amis,'./data/crqa_parameters/ami_calculations-pac.csv', sep=',',row.names=FALSE,col.names=TRUE)

##### 2b. Merging (if we already have amis) #####

# if we've already run it, load it in
amis = read.table('./data/crqa_parameters/ami_calculations-pac.csv', sep=',',header=TRUE)
conversation_df = full_join(conversation_df, amis,
                            by=c('participant_id',
                                 'partner_type',
                                 'task'))

#### 3. Determing embedding dimension with FNN ####

# set maximum percentage of false nearest neighbors
fnnpercent = 10

# create empty dataframe
fnn_dataframe = data.frame()

# cycle through each conversation
convo.dfs = split(conversation_df,
                  list(conversation_df$participant_id, 
                       conversation_df$task,
                       conversation_df$partner_type))
for (next_conv in convo.dfs){
  if (dim(data.frame(next_conv))[1] > 0){
  
    # clean up conversation data
    next_conv_df = data.frame(next_conv) 

    # print update
    print(paste0("Beginning FNN calculations for Participant ", unique(next_conv_df$participant_id),
                 ", Task `", unique(next_conv_df$task),"`, Partner `",unique(next_conv_df$partner_type),"`"))
    
    # get the AMI value for that conversation
    next_ami = unique(next_conv_df$ami.selected)
    
    # calculate false nearest neighbors for participant 0
    fnn_0 = false.nearest(next_conv_df$movement_0,
                          m = fnnpercent,
                          d = next_ami,
                          t = 0,
                          rt = 10,
                          eps = sd(next_conv_df$movement_0) / 10)
    fnn_0 = fnn_0[1,][complete.cases(fnn_0[1,])]
    threshold_0 = as.numeric(fnn_0[1]/fnnpercent)
  
    # calculate false nearest neighbors for participant 1
    fnn_1 = false.nearest(next_conv_df$movement_1,
                          m = fnnpercent,
                          d = next_ami,
                          t = 0,
                          rt = 10,
                          eps = sd(next_conv_df$movement_1) / 10)
    fnn_1 = fnn_1[1,][complete.cases(fnn_1[1,])]
    threshold_1 = as.numeric(fnn_1[1]/fnnpercent)
    
    # identify the largest dimension after a large drop for each participant
    embed_0 = max(as.numeric(which(diff(fnn_0) < -threshold_0))) + 1
    embed_1 = max(as.numeric(which(diff(fnn_1) < -threshold_1))) + 1
    
    # bind everything to data frame
    next_conv_df = next_conv_df %>% ungroup() %>%
      mutate(embed.0 = embed_0) %>%
      mutate(embed.1 = embed_1) %>%
      mutate(embed.selected = max(embed_0, embed_1))
    fnn_dataframe = rbind.data.frame(fnn_dataframe, next_conv_df)
}}

# rename it to save over the old conversation_df
conversation_df = fnn_dataframe

# save false nearest neighbor calculations to file
fnn.merged = conversation_df %>% ungroup() %>%
  select(participant_id, task, partner_type, contains('embed')) %>%
  distinct()
write.table(fnn.merged,'./data/crqa_parameters/fnn_calculations-pac.csv', sep=',',row.names=FALSE,col.names=TRUE)

##### 3b. Merging (if we already have fnns) #####

# if we've already run it, load it in
fnn.merged = read.table('./data/crqa_parameters/fnn_calculations-pac.csv', sep=',',header=TRUE)
conversation_df = full_join(conversation_df, fnn.merged,
                            by=c('participant_id',
                                 'partner_type',
                                 'task'))

#### 4. Determine optimal radius ####

# rescale by mean distance
conversation_df_crqa = conversation_df %>% ungroup() %>%
  dplyr::select(participant_id, task, partner_type,
                movement_0, movement_1, ami.selected, embed.selected) %>%
  group_by(participant_id, task, partner_type) %>%
  mutate(rescale.movement_0 = movement_0/mean(movement_0)) %>%
  mutate(rescale.movement_1 = movement_1/mean(movement_1))

# cycle through all conversations of all dyads
crqa.data = split(conversation_df_crqa,
                  list(conversation_df$participant_id,
                       conversation_df$task,
                       conversation_df$partner_type))
for (next.conv in crqa.data){
  
  # make sure we only proceed if we have data for the conversation
  if (dim(next.conv)[1] > 0){ 
    
    # reset `target` variables for new radius (above what RR can be)
    from.target = 101
    last.from.target = 102
    
    # identify parameters for the conversation
    chosen.delay = unique(next.conv$ami.selected)
    chosen.embed = unique(next.conv$embed.selected)
    
    # start the radius
    chosen.radius = .00
    
    # arbitrary starting point for `rr`
    rr = 100
    
    # if we're still improving, keep going  
    while ((from.target < last.from.target) | rr == 0){

      # keep the previous iteration's performance
      last.from.target = from.target
            
      # don't let it continue forever
      if (chosen.radius > 2) {
        break
      }
      
      # increment radius size
      chosen.radius = chosen.radius + .01
      
      # print update
      print(paste("Participant ", unique(next.conv$participant_id),
                  ", partner ",unique(next.conv$partner_type),
                  ", task ", unique(next.conv$task),
                  ": radius ",chosen.radius,sep=""))
      
      # run CRQA and grab recurrence rate (RR)
      rec_analysis = crqa(next.conv$rescale.movement_0, 
                          next.conv$rescale.movement_1,
                          delay = chosen.delay, 
                          embed = chosen.embed, 
                          r = chosen.radius,
                          normalize = 0, 
                          rescale = 0, 
                          mindiagline = 2,
                          minvertline = 2, 
                          tw = 0, 
                          whiteline = FALSE,
                          recpt=FALSE)
      rr = rec_analysis$RR
      
      # clear it so we don't take up too much memory (optional)
      rm(rec_analysis)
      
      # identify how far off the RR is from our target (5%)
      from.target = abs(rr - 5)
      
      # save individual radius calculations
      participant_id = unique(next.conv$participant_id)
      task = unique(next.conv$task)
      partner_type = unique(next.conv$partner_type)
      write.table(cbind.data.frame(participant_id,
                                   task,
                                   partner_type,
                                   chosen.delay,
                                   chosen.embed,
                                   chosen.radius,
                                   rr,
                                   from.target),
                  paste('./data/crqa_parameters/radius_calculations/radius_calculations-mean_scaled-r',
                        chosen.radius, '-', participant_id ,'_', task, '_', partner_type, '-pac.csv', sep=''), 
                  sep=',',row.names=FALSE,col.names=TRUE)
    }}}

# # let us know when it's finished
# beepr::beep("fanfare")

# concatenate the radius files
radius_files = list.files('./data/crqa_parameters/radius_calculations',
                          pattern='radius_calculations',
                          full.names = TRUE)
radius_selection = data.frame()
for (next_file in radius_files){
  radius_selection = rbind.data.frame(radius_selection,
                                      read.table(next_file, header = TRUE, sep = ','))
}
write.table(radius_selection,'./data/crqa_parameters/radius_calculations-mean_scaled-pac.csv', 
            sep=',', row.names=FALSE, col.names=TRUE)

#### 4b. Reload datasets (if we've already run things) ####

# rescale by mean distance
conversation_df_crqa = conversation_df %>% ungroup() %>%
  dplyr::select(participant_id, task, partner_type,
                movement_0, movement_1, ami.selected, embed.selected) %>%
  group_by(participant_id, task, partner_type) %>%
  mutate(rescale.movement_0 = movement_0/mean(movement_0)) %>%
  mutate(rescale.movement_1 = movement_1/mean(movement_1))

#### 5. Expand radius ####

# load back in the data
radius_selection = read.table('./data/crqa_parameters/radius_calculations-mean_scaled-pac.csv', 
                              sep=',',header=TRUE)

# get radius yielding closest to 5% (only the largest radius, if more than one)
radius_stats = radius_selection %>% ungroup() %>%
  group_by(participant_id, partner_type, task) %>%
  dplyr::filter(from.target==min(from.target)) %>%
  dplyr::arrange(participant_id, partner_type, task) %>%
  dplyr::arrange(desc(from.target)) %>%
  slice(1) %>%
  distinct()
 
# link conversation numbers to types
checking_numbers = conversation_df_crqa %>%
  ungroup() %>%
  dplyr::select(participant_id, task, partner_type) %>%
  distinct()
recheck_radii = radius_stats %>%
  dplyr::left_join(x=.,
                   y=checking_numbers,
                   by=c("participant_id"="participant_id",
                        "task"="task",
                        "partner_type"="partner_type")) %>%
  mutate(recheck.conv = paste(participant_id,
                              task,
                              partner_type,
                              sep='.')) %>%
  mutate(from.target = rr - 5)

# cycle through all conversations of all dyads to refine
recheck_conversation_df = dplyr::left_join(recheck_radii,
                                           conversation_df_crqa)

# split the data
crqa.data = split(recheck_conversation_df,
                  list(recheck_conversation_df$participant_id,
                       recheck_conversation_df$task,
                       recheck_conversation_df$partner_type))

# cycle through the conversations
recheck_radii = crqa.data[recheck_radii$recheck.conv]
for (next.conv in recheck_radii){

  # make sure we only proceed if we have data for the conversation
  if (dim(next.conv)[1] != 0){

    # figure out whether we need to have a larger or smaller radius
    original.from.target = unique(next.conv$from.target)
    chosen.radius = unique(next.conv$chosen.radius)
    rr = unique(next.conv$rr)

    # reset `target` variables for new radius (above what RR can be)
    from.target = abs(original.from.target)
    last.from.target = 102

    # identify parameters
    chosen.delay = unique(next.conv$ami.selected)
    chosen.embed = unique(next.conv$embed.selected)

    # if we haven't hit our target threshold or we're still improving, keep going
    while ((from.target > 1) | (from.target < last.from.target)){

      # set some bounds for our radius explorations
      if (chosen.radius < 0 | chosen.radius > 2) { break } # if we've already a bunch
      if (from.target > (last.from.target) + .5) { break } # if we're not improving

      # keep the previous iteration's performance
      last.from.target = from.target

      # update radius based on whether we overshot or undershot target RR
      if (original.from.target > 0){
        chosen.radius = chosen.radius - .001
      } else {
        chosen.radius = chosen.radius + .001
      }

      # print update
      print(paste("Participant ", unique(next.conv$participant_id),
                  ", partner ",unique(next.conv$partner_type),
                  ", task ", unique(next.conv$task),
                  ": radius ",chosen.radius,sep=""))

      # run CRQA and grab recurrence rate (RR)
      rec_analysis = crqa(next.conv$rescale.movement_0,
                          next.conv$rescale.movement_1,
                          delay = chosen.delay,
                          embed = chosen.embed,
                          r = chosen.radius,
                          normalize = 0,
                          rescale = 0,
                          mindiagline = 2,
                          minvertline = 2,
                          tw = 0,
                          whiteline = FALSE,
                          recpt=FALSE)
      rr = rec_analysis$RR

      # clear it so we don't take up too much memory (optional)
      rm(rec_analysis)

      # identify how far off the RR is from our target (5%)
      from.target = abs(rr - 5)

      # save individual radius calculations
      participant_id = unique(next.conv$participant_id)
      task = unique(next.conv$task)
      partner_type = unique(next.conv$partner_type)
      write.table(cbind.data.frame(participant_id,
                                   task,
                                   partner_type,
                                   chosen.delay,
                                   chosen.embed,
                                   chosen.radius,
                                   rr,
                                   from.target),
                  paste('./data/crqa_parameters/radius_calculations/radius_calculations-mean_scaled-r',
                        chosen.radius, '-', participant_id ,'_', task, '_', partner_type, '-pac.csv', sep=''),
                  sep=',',row.names=FALSE,col.names=TRUE)
    }}}

# # let us know when it's finished
# beepr::beep("fanfare")

# concatenate the radius files
radius_files = list.files('./data/crqa_parameters/radius_calculations',
                          pattern='radius_calculations',
                          full.names = TRUE)
radius_selection = data.frame()
for (next_file in radius_files){
  radius_selection = rbind.data.frame(radius_selection,
                                      read.table(next_file, header = TRUE, sep = ','))
}

# save the new version
write.table(radius_selection,'./data/crqa_parameters/radius_recheck_calculations-mean_scaled-pac.csv',
            sep=',', row.names=FALSE, col.names=TRUE)

#### 6. Export chosen radii for all conversations ####

# load in files
radius_selection = read.table('./data/crqa_parameters/radius_recheck_calculations-mean_scaled-pac.csv',
                              sep=',',header=TRUE)

# identify the target radii
radius_stats = radius_selection %>% ungroup() %>%
  group_by(participant_id, partner_type, task) %>%
  dplyr::filter(from.target==min(from.target)) %>%
  dplyr::arrange(participant_id, partner_type, task) %>%
  dplyr::arrange(desc(from.target)) %>%
  slice(1) %>%
  distinct()

# join the dataframes
conversation_df_crqa = full_join(radius_stats,
                                 conversation_df_crqa,
                                 by = c("participant_id",
                                        "task",
                                        "partner_type",
                                        "chosen.embed" = "embed.selected",
                                        "chosen.delay" = "ami.selected"))

# save to files
write.table(conversation_df_crqa,'./data/crqa_data_and_parameters-pac.csv', 
            sep=',',row.names=FALSE,col.names=TRUE)
write.table(radius_stats, './data/crqa_parameters-pac.csv',
            sep=',',row.names=FALSE,col.names=TRUE)