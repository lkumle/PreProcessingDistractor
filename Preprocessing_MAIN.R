#################################################################################
#################################################################################
#                                                                               #
#          PRE-PROCESSING R SCRIPT  - MAIN DISTRACTOR TASK                      #
#                                                                               #
#################################################################################
#################################################################################

# This script handles pre-processing of the DistractorTask frame-by-frame VR data
#
# Input:
#   - sorting_MAIN_ - data frames for all subjects, sessions and timepoints
#
# Output:
#   - WM_dat.Rda (data frame with 1 row = 1 sequence)
#   - SEARCH_dat.Rda (data frame with 1 row = 1 search)
#   - SUMMARY_dat.Rda (data frame with 1 row = 1 display)


# --> to do:
#   - PLACEMENT_dat.Rda (data frame with 1 row = 1 placement)
#   - GRAB_dat.Rda (data frame with 1 row = 1 pickup)
#   - TARGET_TRIAL_dat.Rda (data frame with 1 row = 1 target)
#                           --> summarised over trial


# --> for overview of variables in all raw and pre-processed
#     data frames see Variables.xlsx


# ---------------------------------------------------------------------------- #
# 0: PREPARE
# ---------------------------------------------------------------------------- #
rm(list = ls())

#library(ObSortPreprocessing) # import analysis functions
library(devtools)
library(data.table)
library(tidyverse)

# get local ObsortPreprocessing package// change to real package once complete
load_all()

# path to data folder with data of MAIN task
path = "/Users/leah/Dropbox/ObjectSorting_LTM/Distractor_task/data/MAIN/"


# -------------------------------------------------------------------------- #
# 1: READING IN DATA
# -------------------------------------------------------------------------- #
# read in files --> get function from preprocessing file
data <- readData(path = paste0(path, "raw/T1/part1"), memory = F) #


# -------------------------------------------------------------------------- #
# 2: PRE-PRE-PROCESSING
# -------------------------------------------------------------------------- #

# ---> A: remove small data saving mistakes

data$timepoint[data$timepoint == 0] <- 1 # timepoint wrong in first two participants

test <- data[gazeGrabbedItem != "" & grabbed == F] # impossible inconsistency

`%!in%`<- Negate(`%in%`)
data$global <- 1:nrow(data)
data <- data[global %!in% test$global] # remove those rows from df_main


# ---> B: remove practice trials
df_main <- data[practice == "test"]

# ---> C: remove blinks & invalid gaze samples
df_main <- df_main[LeftEyeBlink == F & RightEyeBlink == F & GazeRayValid == T]

# --> D: remove trial 8 in each block for participant 1 & 2 in timepoint 1
df_main <- df_main[trial != 8]
df_main$oldDisp <- df_main$display # save old display counter

for(i in 1:2){ # remap display counter to make consistent with other subjects
  df_main[sub == i & timepoint == 1]$display <- plyr::mapvalues(df_main[sub == i & timepoint == 1]$display, # take display counters
                                                                from = unique(df_main[sub == i & timepoint == 1]$display), # old values
                                                                to = c(1:56)) # map to new values
}

#################################################
# IMPORTANT: do not modify df_main from here on #
#################################################
load("~/Dropbox/ObjectSorting_LTM/Distractor_task/data/MAIN/raw/T1/raw_combined_part1.Rda")

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
save(WM_dat, file = paste0(path, "processed/","WM_dat_part1_T1.Rda"))


# ------------------------------ #
# SEQUENCE DATA -> SEARCH
# -------------------------------#

# --> A: get start/end frames of each search
searchFrame <- searchFrames(df_main)

# --> B: go through each search and summarize variables
SEARCH_dat <- SequencingSearch(df_main, searchFrame, WM_dat, cutoff = 2)

# --> C: save preprocessed data frame in preprocessed folder
save(SEARCH_dat, file = paste0(path, "processed/","SEARCH_dat_part1_T1.Rda"))


# ------------------------------ #
# SUMMARY:
# -------------------------------#
load("~/Dropbox/ObjectSorting_LTM/Distractor_task/data/MAIN/processed/WM_dat_part1_T1.Rda")
load("~/Dropbox/ObjectSorting_LTM/Distractor_task/data/MAIN/processed/SEARCH_dat_part1_T1.Rda")


SUMMARY_dat <- trialSummary(df_main, WM_dat, SEARCH_dat)
save(SUMMARY_dat, file = paste0(path, "processed/","SUMMARY_dat_part1_T1.Rda"))















# ------------------------------ #
# SENSOMOTOR DECISIONS:
# -------------------------------#

# ------------ placement ------ #
#placementData <- SequencingPlacement(df_main)

#placementDat <- distancePlacements(df_main)

# ------------ identity -------- #
#grabData <- grabData(df_main, WM_dat, SEARCH_dat)









