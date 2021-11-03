
test <- function(){

  #-------------------- prepare
  target_data <- data.table(
    target = character(),
    sub = numeric(),
    timepoint = numeric(),
    practice = character(),
    block = numeric(),
    angle = numeric(),
    distractor = character(),
    placement = numeric(),
    trial = numeric(),
    display = numeric())


  # ------------------ go through subjects, sessions, timepoints!


  ## 1: for each trial, manage to read in targets + general info
  # read in session file, go through each row, store
  subj <- 1
  sess <- 1
  tp <- 1

  df_subset <- df_main[sub == subj &  session == sess & timepoint == tp]

  # load in stimulus lists
  setwd(paste0(getwd(),"/sessions"))
  file = paste0( "SUB" , subj , "_SESSION" , sess , "_T" , tp , ".txt")
  tData <- read.table(file, sep = ",")


  # -------------------------- go through all displays
  for(disp in unique(df_subset$display)){

    # df_display
    df_display <- df_subset[display == disp]

    # get target objects of current trial
    targets <- as.character(tData[disp,])
    targets <- targets[targets != "none"]






    # -------------- go through each target
    for(target in targets){



      # ----- store basic information
      single_target <- list(target, df_display$sub[1], df_display$timepoint[1],df_display$practice[1], df_display$block[1],
                            df_display$angle[1], df_display$load[1], df_display$placement[1], df_display$trial[1], disp)

      target_data <- rbindlist(list(target_data, single_target))


    }# end target in targets
  } # end display
}






