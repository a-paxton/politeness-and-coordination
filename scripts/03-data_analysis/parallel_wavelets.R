#### parallel_wavelets.R  ####
#
# This script creates a function to run `biwavelet::wtc()` with the
# `parallel` function.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 16 July 2020
##############################

# create a function to be applied over a `split` df
parallel_wavelets <- function(dfs, 
                              raw_output_directory, 
                              processed_output_directory,
                              figure_directory) {
  
  # cycle through each subset
  foreach(i = seq_along(dfs), .errorhandling='stop') %dopar% {
    
    # grab the next dataframe
    df <- dfs[[i]]
    
    # only proceed if the next dataframe has an observation
    if (dim(df)[1]>0){
      
      # figure out the information for the dyad
      this_participant = unique(df$participant_id)
      this_partner_type = unique(df$partner_type_str)
      this_task = unique(df$task_str)
      
      # set up for analyses
      person_0 = dplyr::select(df,
                               t, movement_0)
      person_1 = dplyr::select(df,
                               t, movement_1)
      
      # calculate coherence
      coherence_wavelet = biwavelet::wtc(person_0,
                                         person_1,
                                         sig.level = .95)
      
      # print each list to a separate file
      for (next_list in names(coherence_wavelet)){
        write.table(x = coherence_wavelet[[next_list]],
                    file=paste0(raw_output_directory,'/',
                                'participant_', this_participant,'-',
                                'partner_', this_partner_type,'-',
                                'task_', this_task,'-',
                                'wavelet_',next_list,'.csv'),
                    sep=",")
      }
      
      # find significant regions
      significant_regions = coherence_wavelet$rsq >= coherence_wavelet$signif
      significant_regions[significant_regions==0] = NA
      
      # find phases and power levels for significant regions
      significant_phases = coherence_wavelet$phase * significant_regions
      significant_phases[significant_regions==0] = NA
      significant_power = coherence_wavelet$power.corr * significant_regions
      significant_power[significant_regions==0] = NA
      
      # find mean characteristics by scale during significant moments
      mean_sig_phase = data.frame(participant = this_participant,
                                  partner = this_partner_type,
                                  task = this_task,
                                  mean_sig_phase = base::rowMeans(significant_phases, 
                                                                  na.rm = TRUE))
      mean_sig_power = data.frame(participant = this_participant,
                                  partner = this_partner_type,
                                  task = this_task,
                                  mean_sig_phase = base::rowMeans(significant_power, 
                                                                  na.rm = TRUE))
      mean_sig_time_power = data.frame(participant = this_participant,
                                       partner = this_partner_type,
                                       task = this_task,
                                       mean_sig_phase = base::colMeans(significant_power, 
                                                                       na.rm = TRUE))
      
      # write processed metrics
      write.table(x = significant_regions,
                  file=paste0(processed_output_directory, '/',
                              'participant_', this_participant,'-',
                              'partner_', this_partner_type,'-',
                              'task_', this_task,'-',
                              'significant_regions.csv'),
                  sep=",")
      write.table(x = significant_phases,
                  file=paste0(processed_output_directory, '/',
                              'participant_', this_participant,'-',
                              'partner_', this_partner_type,'-',
                              'task_', this_task,'-',
                              'significant_phases.csv'),
                  sep=",")
      write.table(x = significant_power,
                  file=paste0(processed_output_directory, '/',
                              'participant_', this_participant,'-',
                              'partner_', this_partner_type,'-',
                              'task_', this_task,'-',
                              'significant_power.csv'),
                  sep=",")
      write.table(x = mean_sig_phase,
                  file=paste0(processed_output_directory, '/',
                              'participant_', this_participant,'-',
                              'partner_', this_partner_type,'-',
                              'task_', this_task,'-',
                              'mean_sig_phase.csv'),
                  row.names = FALSE,
                  sep=",")
      write.table(x = mean_sig_power,
                  file=paste0(processed_output_directory, '/',
                              'participant_', this_participant,'-',
                              'partner_', this_partner_type,'-',
                              'task_', this_task,'-',
                              'mean_sig_power.csv'),
                  row.names = FALSE,
                  sep=",")
      write.table(x = mean_sig_time_power,
                  file=paste0(processed_output_directory, '/',
                              'participant_', this_participant,'-',
                              'partner_', this_partner_type,'-',
                              'task_', this_task,'-',
                              'mean_sig_time_power.csv'),
                  row.names = FALSE,
                  sep=",")
      
      # save plots
      png(filename=paste0(figure_directory,'/',
                          'participant_', this_participant,'-',
                          'partner_', this_partner_type,'-',
                          'task_', this_task,'-',
                          'wavelet.png'),
          width = 7, height = 5, units = "in")
      par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
      plot(coherence_wavelet,
           plot.phase = TRUE,
           lty.coi = 1,
           col.coi = "grey",
           lwd.coi = 3,
           lwd.sig = 2,
           arrow.lwd = 0.03,
           arrow.len = 0.05,
           ylab = "Scale", xlab = "Period",
           plot.cb = TRUE,
           main = paste0("Wavelet Coherence:\n ",
                         'Participant ', this_participant,', ',
                         this_partner_type, ' Partner, ',
                         this_task, ' Task'))
      dev.off()
      
      # clear the biwavelet object to save some space
      rm(coherence_wavelet)
    }}
}