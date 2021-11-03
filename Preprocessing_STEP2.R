#################################################################################
#################################################################################
#                                                                               #
#          PRE-PROCESSING R SCRIPT  - MAIN DISTRACTOR TASK                      #
#                         PRE-PREPROCESSED DATA                                 #
#                                                                               #
#################################################################################
#################################################################################

# this script assumes data is already pre-preprocessed

rm(list = ls())

#library(ObSortPreprocessing) # import analysis functions
library(devtools)
library(data.table)
library(tidyverse)

# get local ObsortPreprocessing package// change to real package once complete
load_all()

# path to data folder with data of MAIN task
path = "/Users/leah/Dropbox/ObjectSorting_LTM/Distractor_task/data/MAIN/"


#################################################
# IMPORTANT: do not modify df_main from here on #
#################################################

for(i in 1:3){

  load(paste0(path, "raw/T1/raw_combined_part", i,"_headRotation2.Rda"))

  # add movement metric no 2.
  df_main$head_euler_z2 <- c(df_main$head_euler_z[-(seq(1))], rep(NA, 1))
  df_main$headRotation_yaw <- df_main$head_euler_z - df_main$head_euler_z2

  # -------------------------------------------------------------------------- #
  # 3: SPLITTING SEQUENCES
  # -------------------------------------------------------------------------- #

  # ------------------------------- #
  # SEQUENCE DATA -> WM
  # --------------------------------#

  # --> A: get start/end frames of each sequence
  sequence_start <- sequenceFramesWM(df_main, cutoff = 2)

  # --> B: go through each sequence and summarize variables
  WM_dat <- SequencingWM(df_main, sequence_start, cutoff = 2)


  # --> C: save data
  # save preprocessed data frame in preprocessed folder
  save(WM_dat, file = paste0(path, "processed/","WM_dat_part",i,"_T1.Rda"))


  # ------------------------------ #
  # SEQUENCE DATA -> SEARCH
  # -------------------------------#

  # --> A: get start/end frames of each search
  #searchFrame <- searchFrames(df_main)

  # --> B: go through each search and summarize variables
  #SEARCH_dat <- SequencingSearch(df_main, searchFrame, WM_dat, cutoff = 2)

  # --> C: save preprocessed data frame in preprocessed folder
  #save(SEARCH_dat, file = paste0(path, "processed/","SEARCH_dat_part",i,"_T1.Rda"))


  # ------------------------------ #
  # SUMMARY:
  # -------------------------------#

  #SUMMARY_dat <- trialSummary(df_main, WM_dat, SEARCH_dat)
  #save(SUMMARY_dat, file = paste0(path, "processed/","SUMMARY_dat_part",i,"_T1.Rda"))

}

