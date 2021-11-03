#' Read in data files for Object Sorting Task in VR.
#'
#' Does not explicitely return data but assigns it to global variable inside the function.
#' @export
#'
#'
searchFrames <- function(df_main, cutoff = 10){

  # ------------------------- #
  # PREPARE
  # ------------------------- #


  df_main[, frameGlobal := c(1:nrow(df_main))] # make sure frame Global exists
  searches <- list() # prepare storing frames
  nsearch_total <- 0

  # prepare progress bar
  displays <- length(unique(df_main[practice == "test"]$display))*length(unique(df_main$sub))*2*length(unique(df_main$timepoint))
  pb <- progress::progress_bar$new(total = displays, clear = T)


  # -- 1: get combinations of subs/timepoint/session/phase/ to avoid nested loops
  subSets <- expand.grid(TP = unique(df_main$timepoint), sess = unique(df_main$session),
                          phase = unique(df_main$practice),       subs = unique(df_main$sub))

  # ------------------------- #
  # LOOP THROUGH FRAMES
  # ------------------------- #

  # -- 2: loop through combinations
  for(n in 1:nrow(subSets)){

    # ------ get correct subset of data frame:
    df_subSet <- df_main[sub == subSets[n,]$subs &
                            timepoint == subSets[n,]$TP &
                            session == subSets[n,]$sess &
                            practice == subSets[n,]$phase]



    # go through each display:
    for(dispI in 1:length(unique(df_subSet$display))){

      if(subSets[n,]$phase == "test"){ pb$tick()}

      df_disp <- df_subSet[display == dispI] # only look at current display... avoid spill over from other trials
      searching <- F

      n_search <- 0

      # mow go through each row in display:
      for(row in 1:nrow(df_disp)){

        # allow cut off: if looking away from search for less frames than cutiff... keep counting as one sequence!
        if((nrow(df_disp) - row) > cutoff){
          looking_ahead <- cutoff
        } else {
          looking_ahead <-(nrow(df_disp) - row)}
          #print(looking_ahead)}


        frameN <- df_disp$frameGlobal[row] #global frame number
        next_rows <- df_main$gazeArea[(frameN+1):(frameN +looking_ahead)] # get the next few entries of gazeArea

        # what has been looked at before?
        if(row == 1){ # display just started -> assign before to "undefined"
          before <- "undefined" # avoid spill over from old displays/subjects!
        }else{ # somewhere in the middle of the display --> assign gaze of one frame before
          before <- df_main$gazeArea[frameN-1]}


        # RECORD START OF SEARCH!
        if(df_main$gazeArea[frameN] == "ressourceArea" # looking at resource
           & before != "ressourceArea" # before that we did not look at resource
           & !searching # also not already currently in a search
           & df_main$grabbed[frameN] == F){ # also not already having something grabbed

          search_start <- frameN
          searching <- T

          n_search <-  n_search + 1 # count search in display
          nsearch_total <- nsearch_total + 1 # count overall searches
        }


        if(frameN == 1  | row == 1){
          grabbed_before <- F
        }else{grabbed_before <- df_main$grabbed[frameN-1]}

        # RECORD END OF SEARCH
        # 1:  looking away
        if(df_main$gazeArea[frameN] != "ressourceArea"  # ot looking at resource anymore
           & !("ressourceArea" %in% next_rows) # not looking back in the next 5 rows
           & searching){ # currently still searching
          #& df_main$grabbed[frameN] == T){ # nothing grabbed

          searching <- F # stop searching
          search_end <- frameN-1 # record end

          # store search
          searches[[nsearch_total]] <- c(search_start, search_end)
          # 2: grabbing an object
        }
        else if(before == "ressourceArea" # just looked at resource
                & df_main$grabbed[frameN] == T # just grabbed something
                & grabbed_before == F
                & searching){

          searching <- F
          search_end <- frameN-1 # record end

          # store search
          searches[[nsearch_total]] <- c(search_start, search_end)
        }
      }
    }
  }

  searches[sapply(searches, is.null)] <- NULL
  searches
} # end function
