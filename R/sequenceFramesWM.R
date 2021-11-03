#' Get start and end of WM sequences. Sequences always starts with foxation on model.
#' Sequences ends when new model fixationis detected.
#'
#'  --> function makes sure to divide into subs/timepoint/session/phase/displays
#'     to avoid spill over effects
#'
#' returns nested list (for each display to avoid spill over!)
#' last frame number in each list is last frame + 1 (makes looping later easier)
#'
#' @param df_main combined data frame of main VR task
#' @param cutoff How many frames need to be between two fixations on model to count as two fixations?
#'
#' @return nested list. Each display is stored as a seperate list with global frame/row number of sequence start. Last value
#' is end frame of dispplay for easier looping later.
#'
#' @export

sequenceFramesWM <- function(df_main, cutoff = 10){


  # --------------------------- #
  # PREPARE
  # -------------------------- #
  sequence_start <- list() # empty list to store frames in
  before <- "start" # have a start value
  globalDisp <- 0 # get global display counter

  # add columns to mark start and end of sequences:
  df_main[, seq_start := NA]
  df_main[, seq_end := NA]
  #df_main[, frameGlobal := c(1:nrow(df_main))]

  # add progress bar
  displays <- length(unique(df_main[practice == "test"]$display))*length(unique(df_main$sub))*2*length(unique(df_main$timepoint))
  pb <- progress::progress_bar$new(total = displays, clear = T)


  # --------------------------- #
  # GET START OF SEQUENCES
  # -------------------------- #

  # -- 1: get combinations of subs/timepoint/session/phase/ to avoid nested loops
  subSets <- expand.grid(TP = unique(df_main$timepoint), sess = unique(df_main$session),
                        phase = unique(df_main$practice), subs = unique(df_main$sub))

  #subSets <- subSets[subSets$TP == 1,]


  # -- 2: loop through combinations
  for(n in 1:nrow(subSets)){


    # ------ get correct subset of data frame:
    df_subSet <- df_main[sub == subSets[n,]$subs &
                         timepoint == subSets[n,]$TP &
                          session == subSets[n,]$sess &
                           practice == subSets[n,]$phase]



    # MAKE MORE ROBUST: if df_subSet <- nrow () == 0: BREAK
    # make robust in case specific combination does not exist
    if(nrow(df_subSet) == 0){
      break
    }


    # ------- loop through all displays of subset
    # go through each display:
    for(dispI in 1:length(unique(df_subSet$display))){


      # advance progress bar
      if(df_subSet[n,]$practice == "test"){ pb$tick()}

      # --------- #
      # PREPARE
      # --------- #
      df_disp <- df_subSet[display == dispI]  # only look at current display!

      sequence_display <- c() # prepare storing frames
      globalDisp <- globalDisp + 1 # count displays!

      # ----------- #
      # LOOP FRAMES
      # ----------- #
      for(row in 1:nrow(df_disp)){ # go through each frames to find sequence starts

        frameN <- df_disp$frameGlobal[row] #global frame number

        # A: ------------------------
        # What was looked at before:
        if(row == 1){ # display just started -> assign before to "undefined"
          before <- "undefined" # avoid spill over from old displays/subjects!
        }else{ # somewhere in the middle of the display --> assign gaze of one frame before
          before <- df_main$gazeArea[frameN-1]}

        # B: ------------------------
        # was the break between models shorter that n = cutoff frames?

        if(row < cutoff){looking_back <- row # adjust cutoff for all frames smaller than cutoff
        }else {
          looking_back <- cutoff}
        #print(looking_back)

        earlier_rows <- df_main$gazeArea[(frameN - looking_back):(frameN-1)] #get latest gazeArea frames
        tooShort <- ifelse("modelArea" %in% earlier_rows, T, F) # if "modelArea" in last x frames --> does not count as new fixation!


        # C: ------------------------
        # Record new gaze on model


        if(df_main$gazeArea[frameN] == "modelArea" # gaze on model, gaze before was NOT on model
           & before != "modelArea"){               # --> new gaze on model!

          if(tooShort == F){ # only record new gaze if cutoff not met

            # store "start frames"
            sequence_display <- c(sequence_display, frameN)

            # also store in main data frame!
            df_main[frameN, seg_start:= 1]
            if(frameN != 2){df_main[frameN-1, seg_end:=1]}
          }
        }
      }

      # D: ------------------------
      # add last row of each display to make looping easier in the end!
      sequence_start[[globalDisp]] <-  c(sequence_display, max(df_disp$frameGlobal)+1)

    } # end display
  } # end combinations

  # --------------------------- #
  # RETURN SEQUENCES
  # -------------------------- #
  # return sequence starts
  print("SEQUENCEING COMPLETE")
  sequence_start
}
