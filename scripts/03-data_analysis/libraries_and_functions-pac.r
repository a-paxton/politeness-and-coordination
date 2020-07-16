#### libraries_and_functions-pac.r: Part of `politeness_and_coordination.Rmd` ####
#
# This script loads libraries and creates a number of
# additional functions to facilitate data prep and analysis.
#
# Written by: A. Paxton (University of Connecticut)
# Date last modified: 16 July 2020
#####################################################################################

#### Load necessary packages ####

# list of required packages
required_packages = c(
  'dplyr',
  'stringr',
  'data.table',
  'lme4',
  'ggplot2',
  'pander',
  'grid',
  'gridExtra',
  'plotrix',
  'gtable',
  'viridis',
  'tidyr',
  'tibble',
  'RCurl',
  'TTR',
  'biwavelet',
  'doParallel',
  'foreach',
  'MASS',
  'parallel'
)

# load required packages
invisible(lapply(required_packages, require, character.only = TRUE))

#### Prevent scientific notation ####
options(scipen = 999)
options(digits=14)

#### Create global variables and useful global functions ####

# function to return p-values from t-values
pt = function(x) {return((1 - pnorm(abs(x))) * 2)}

# create a function to be applied over a `split` df
parallel_wavelets <- function(dfs) {
  
  # cycle through each subset
  foreach(
    i = seq_along(dfs)) %dopar% {
      
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
          MASS::write.matrix(x = coherence_wavelet[[next_list]],
                             file=paste0('./data/wavelet/',
                                         'participant_', this_participant,'-',
                                         'partner_', this_partner_type,'-',
                                         'task_', this_task,'-',
                                         'wavelet_',next_list,'.csv'),
                             sep=",")
        }
        
        # return the biwavelet object
        return(coherence_wavelet)
      }}
}

# specify raw sampling rate
raw_sampling_rate = 30

# specify downsampled sampling rate
downsampled_sampling_rate = 20

# specify how much we should trim off from the beginning and end of the conversation (in seconds)
trimmed_time = 5

# specify which variables will be factors
factor_variables = c('participant_id',
                     'participant_gender',
                     'task',
                     'partner_type')

#### Read in model output formatting functions from repo ####

# read in `pander_lme` at the correct commit
# thanks to https://github.com/opetchey/RREEBES/wiki/
pander_lme_url = "https://raw.githubusercontent.com/a-paxton/stats-tools/bee546f2c959cb6a5b9cad1f28d3afbae6e46c41/pander_lme.R"
pander_lme_file = getURL(pander_lme_url, ssl.verifypeer = FALSE)
eval(parse(text = pander_lme_file))

#### Crib other folks' functions ####

#' Adapted from rmd2rscript: script for converting .Rmd files to .R scripts
#'
#' Thanks to Kevin Keenan:
#' http://rstudio-pubs-static.s3.amazonaws.com/12734_0a38887f19a34d92b7311a2c9cb15022.html
#'
#' This function will read a standard R markdown source file and convert it to
#' an R script to allow the code to be run using the "source" function.
#'
#' The function is quite simplisting in that it reads a .Rmd file and adds
#' comments to non-r code sections, while leaving R code without comments
#' so that the interpreter can run the commands.

rmd2rscript <- function(infile, outname){
  
  # read the file
  flIn <- readLines(infile)
  
  # identify the start of code blocks
  cdStrt <- which(grepl(flIn, pattern = "```{r*", perl = TRUE))
  
  # identify the end of code blocks
  cdEnd <- sapply(cdStrt, function(x){
    preidx <- which(grepl(flIn[-(1:x)], pattern = "```", perl = TRUE))[1]
    return(preidx + x)
  })
  
  # define an expansion function
  # strip code block indacators
  flIn[c(cdStrt, cdEnd)] <- ""
  expFun <- function(strt, End){
    strt <- strt+1
    End <- End-1
    return(strt:End)
  }
  idx <- unlist(mapply(FUN = expFun, strt = cdStrt, End = cdEnd,
                       SIMPLIFY = FALSE))
  
  # add comments to all lines except code blocks
  comIdx <- 1:length(flIn)
  comIdx <- comIdx[-idx]
  for(i in comIdx){
    flIn[i] <- paste("#' ", flIn[i], sep = "")
  }
  
  # create an output file
  flOut <- file(paste(outname, "[rmd2r].R", sep = ""), "w")
  for(i in 1:length(flIn)){
    cat(flIn[i], "\n", file = flOut, sep = "\t")
  }
  close(flOut)
}
