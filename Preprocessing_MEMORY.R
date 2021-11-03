# --------------------------------------------------------------------------- #
# PREPROCESSING R SCRIPT  - DISTRACTOR TASK
# --------------------------------------------------------------------------- #
rm(list = ls())

library(ObSortPreprocessing) # import analysis functions
library(data.table)
library(tidyverse)
library(ggplot2)

# -------------------------------------------------------------------------- #
# 1: READING IN DATA
# -------------------------------------------------------------------------- #
# read in files --> get function from preprocessing file
path = "/Users/leah/Dropbox/ObjectSorting_LTM/Distractor_task/data/MEMORY/"

data <- readData(path = paste0(path, "raw"), memory = T, main = F)

# -------------------------------------------------------------------------- #
# 2: PRE-PRE-PROCESSING, CLeaning gaze
# -------------------------------------------------------------------------- #

# --> REMOVE BLINKS + INVALID GAZE SAMPLES
df_raw <- data[LeftEyeBlink == F & RightEyeBlink == F & GazeRayValid == T]

# --> remove practice trials
df_memory <- df_raw[practice == "test"]


# -------------------------------------------------------------------------- #
# 2: SEQUENCING
# -------------------------------------------------------------------------- #

# A) Summary statistics, correct features etc
MEM_dat <- SequencingMemory(df_memory , cutoff = 2)

save(MEM_dat, file = paste0(path, "processed/","MEM_dat.Rda"))




