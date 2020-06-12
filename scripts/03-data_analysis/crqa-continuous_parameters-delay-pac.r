#### crqa-continuous_parameters-delay-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script identifies the delay parameter for continuous cross-recurrence analysis
# by through the average mutual information function, `ami`.
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

#### 2. Determine delay with average mutual information (AMI) ####

# set maximum AMI
ami.lag.max = 100

# get AMI lag for both participants in each task
amis = conversation_df %>% ungroup() %>%
  group_by(participant_id, task, partner_type) %>%
  
  # get current AMI values
  mutate(ami.loc0 = first_local_minimum(
    mutual(movement_0, lag.max = ami.lag.max, plot = FALSE))
  ) %>%
  mutate(ami.loc1 = first_local_minimum(
    mutual(movement_1, lag.max = ami.lag.max, plot = FALSE))
  ) %>%
  
  # figure out what we're keeping
  group_by(participant_id, task, partner_type, ami.loc0, ami.loc1) %>%
  distinct() %>%
  mutate(ami.selected = max(ami.loc1, ami.loc0, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # save only the values we want to keep
  select(-t, -contains('movement')) %>%
  distinct()

# double it to make sure what we have are sensible values
ami.lag.max.longer = ami.lag.max * 2
longer_amis = conversation_df %>% ungroup() %>%
  group_by(participant_id, task, partner_type) %>%
  
  # get current AMI values
  mutate(ami.loc0 = first_local_minimum(
    mutual(movement_0, lag.max = ami.lag.max.longer, plot = FALSE))
  ) %>%
  mutate(ami.loc1 = first_local_minimum(
    mutual(movement_1, lag.max = ami.lag.max.longer, plot = FALSE))
  ) %>%
  
  # figure out what we're keeping
  group_by(participant_id, task, partner_type, ami.loc0, ami.loc1) %>%
  distinct() %>%
  mutate(ami.selected = max(ami.loc1, ami.loc0, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # save only the values we want to keep
  select(-t, -contains('movement')) %>%
  distinct()

#### 3. Plot identified delays ####

# graph AMI values and demonstrate that the higher AMI cap is sensible
split_participants = split(conversation_df, list(conversation_df$participant_id))
for (next_partic in split_participants){
  
  # get participant information
  this_participant = unique(next_partic$participant_id)
  
  # split by partner
  split_partners = split(next_partic,
                         list(next_partic$partner_type))
  for (next_partner in split_partners){
    
    # get partner information
    this_partner_type = unique(next_partner$partner_type)
    
    # split by task
    split_tasks = split(next_partner, list(next_partner$task))
    for (next_task in split_tasks){
      
      # only proceed if we have task data
      if (dim(next_task)[1] > 0){
        
        # get task information
        this_task = unique(next_task$task)
        
        # identify delay
        temp_ami_0 = mutual(next_task$movement_0,
                            lag.max = ami.lag.max.longer,
                            plot = FALSE)
        hold_0_full = temp_ami_0
        temp_ami_1 = mutual(next_task$movement_1,
                            lag.max = ami.lag.max.longer,
                            plot = FALSE)
        hold_1_full = temp_ami_1
        temp_identified_delay_0 = first_local_minimum(temp_ami_0)
        temp_identified_delay_0_shorter = first_local_minimum(temp_ami_0[0:ami.lag.max])
        temp_identified_delay_1 = first_local_minimum(temp_ami_1)
        temp_identified_delay_1_shorter = first_local_minimum(temp_ami_1[0:ami.lag.max])
        
        # plot delay for participant
        temp_0_df = data.frame(ami = matrix(temp_ami_0),
                               delay = seq_along(temp_ami_0))
        temp_plot0 = ggplot(data=temp_0_df,aes(y=ami, x=delay)) +
          geom_line() +
          geom_line(data=temp_0_df,
                    aes(y=ma(ami,n=20), x=delay),
                    color="grey", alpha=.8) +
          geom_vline(xintercept=temp_identified_delay_0,
                     color="red") +
          geom_vline(xintercept=temp_identified_delay_0_shorter,
                     color="blue",
                     linetype='dashed') +
          coord_cartesian(ylim=c(-.5,2)) +
          xlab("Delay") +
          ylab("AMI") +
          ggtitle(paste0('Participant ', this_participant, ', ',
                         this_partner_type, ', ',
                         this_task))
        
        # plot delay for partner
        temp_1_df = data.frame(ami = matrix(temp_ami_1),
                               delay = seq_along(temp_ami_1))
        temp_plot1 = ggplot(data=temp_1_df,aes(y=ami, x=delay)) +
          geom_line() +
          geom_line(data=temp_1_df,
                    aes(y=ma(ami,n=20), x=delay),
                    color="grey",
                    alpha=.8) +
          geom_vline(xintercept=temp_identified_delay_1,
                     color="red") +
          geom_vline(xintercept=temp_identified_delay_1_shorter,
                     color="blue",
                     linetype='dashed') +
          coord_cartesian(ylim=c(-.5,2)) +
          xlab("Delay") +
          ylab("AMI") +
          ggtitle(paste0('Partner ', this_participant, ', ',
                         this_partner_type, ', ',
                         this_task))
        
        # assign to plots
        assign(paste("plot",this_participant,
                     "participant",this_partner_type,
                     this_task, sep="_"),
               temp_plot0)
        assign(paste("plot",this_participant,
                     "partner",this_partner_type,
                     this_task, sep="_"),
               temp_plot1)
        
      }
    }
  }
  
  # get list of all plots for the participant
  list_partic_friend_plots = ls(pattern=paste0("plot_",
                                               this_participant,
                                               "_participant_Friend"))
  list_partic_prof_plots = ls(pattern=paste0("plot_",
                                             this_participant,
                                             "_participant_Prof"))
  list_partner_friend_plots = ls(pattern=paste0("plot_",
                                                this_participant,
                                                "_partner_Friend"))
  list_partner_prof_plots = ls(pattern=paste0("plot_",
                                              this_participant,
                                              "_partner_Prof"))
  
  # construct aggregated plots
  temp_friend_plot = cowplot::plot_grid(
    plotlist=mget(c(list_partic_friend_plots,
                    list_partner_friend_plots)),
    nrow=2)
  temp_prof_plot = cowplot::plot_grid(
    plotlist=mget(c(list_partic_prof_plots,
                    list_partner_prof_plots)),
    nrow=2)
  
  # save
  cowplot::save_plot(plot = temp_friend_plot,
                     filename = paste0("./figures/amis/plot-",
                                       this_participant,
                                       "_aggregated_friend.png"),
                     base_width = 15,
                     base_height = 6,
                     base_aspect_ratio = .9)
  cowplot::save_plot(plot = temp_prof_plot,
                     filename = paste0("./figures/amis/plot-",
                                       this_participant,
                                       "_aggregated_prof.png"),
                     base_width = 15,
                     base_height = 6,
                     base_aspect_ratio = .9)
  
  # remove constituent plots
  rm(list = c(list_partic_friend_plots,
              list_partic_prof_plots,
              list_partner_friend_plots,
              list_partner_prof_plots))
  rm(list=ls(pattern="temp"))
}

# at this point, it's critical to go visually inspect the plots

#### 4. Export to file ####

# export just the AMI information to file
amis = amis %>% ungroup() %>%
  group_by(participant_id,
           task,
           partner_type) %>%
  select(-frame, -participant_gender, -condition) %>%
  slice(1)

# write AMI information to file
write.table(amis,'./data/crqa_parameters/ami_calculations-pac.csv',
            sep=',',row.names=FALSE,col.names=TRUE)
