#' pre processing for memory task
#' @export
#'
#'

SequencingMemory <- function(df_memory, cutoff = 2){

  # -------------------------------------------------------------------------- #
  # PREPARE
  # -------------------------------------------------------------------------- #
  mem_dat <- data.table(

    # general info
    sub = numeric(),
    timepoint = numeric(),
    practice = character(),
    block = numeric(),
    angle = numeric(),
    distractor = character(),
    placement = numeric(),
    trial = numeric(),
    display = numeric(),
    completionTime = numeric(),

    # encoding
    encodingTime = numeric(),
    nFixationsEncoding = numeric(),
    nObjectsEncoded = numeric(),

    # performance
    correctFeatures = numeric(),
    IDonly = numeric(),
    LOConly = numeric(),
    startStreak = numeric(),
    longestStreak = numeric()
  )

  count <- 0

  # for each subject, timepoint, phase, trial:

  # -- 1: get combinations of subs/timepoint/session/phase/ to avoid nested loops
  subSets <- expand.grid(TP = unique(df_memory$timepoint),
                         phase = unique(df_memory$practice),
                         subs = unique(df_memory$sub),
                         disp = unique(df_memory$display))

  pb <- progress::progress_bar$new(total = nrow(subSets)) # progress bar

  # ---- 2: go through combinations
  for(n in 1:nrow(subSets)){

    pb$tick()

    fixatedObjects <- c()


    # ------ get correct subset of data frame:
    df_subSet <- df_memory[sub == subSets[n,]$subs &
                             timepoint == subSets[n,]$TP &
                             practice == subSets[n,]$phase &
                             display == subSets[n,]$disp]

    # make robust in case specific combination does not exist
    if(nrow(df_subSet) == 0){
      next

    }

    # -------------------------------------------------------------------------- #
    # ENCODING DATA
    # -------------------------------------------------------------------------- #

    # 1. subset data frame to get encoding duration
    df_encoding <- df_subSet[empty == F]

    if(nrow(df_encoding) == 0){
      encodingTime <- completionTime <- Nfixated <- nObjectsEncoded <- NA

      next
    }

    # 2. SUMMARY measures
    encodingTime <- df_encoding$timeExp[nrow(df_encoding)] - df_encoding$timeExp[1]
    completionTime <- df_subSet$timeExp[nrow(df_subSet)] - df_subSet$timeExp[1]

    # 3. frame wise measures
    for(frame in 1:nrow(df_encoding)){

      # which object has been looked at before
      currObj <- df_encoding$gazeModel_Item[frame] # which object is looked at right now

      if(frame == 1){oldObj <- ""
      }else oldObj <- df_encoding$gazeModel_Item[frame-1] # which object was looked at in earlier frame?

      # count consecutive frames!
      if(currObj == oldObj & currObj != ""){ # same object in next frame and not looking at nothing
        count <- count + 1
      }
      # --- N FIXATIONS ON OBJECT --- #
      # new object recorded:

      if(currObj != oldObj){
        # store if it exceeds cutoff threshold
        if(count >= cutoff && currObj != "" && currObj != "none"){

          # store object
          fixatedObjects <- c(fixatedObjects, oldObj)
        }
        count <- 0 # reset count
      }

    } # end frame wise

    # 4: some more summary statistics:
    Nfixated <- length(fixatedObjects)
    nObjectsEncoded <- length(unique(fixatedObjects))

    # -------------------------------------------------------------------------- #
    # MEMORY DATA
    # -------------------------------------------------------------------------- #

    # 1: SUMMARY MEASURES
    # how many correct features overall?
    corr_Feat <- nrow(df_subSet[correctPlacement == T])* 2

    # only location is correct
    locOnly <- nrow(df_subSet[placedIsTarget == F & chosenSlot == correctSlot])

    # only identity is correct
    IDOnly <- nrow(df_subSet[placedIsTarget == T & chosenSlot != correctSlot])

    # how many of encoded objects have been placed?


    # 2: FRAME WISE
    df_place <- df_subSet[correctPlacement != ""]
    streak <- 0
    longestStreak <- 0
    startStreak <- 0
    wrong <- F

    if(nrow(df_place) == 0){

      next
    }

    for(frame in 1:nrow(df_place)){

      # person made correct placement
      if(df_place$correctPlacement[frame] == T){

        streak <- streak + 2

        # no wrong placement so far --> add to startStreak
        if(wrong == F){startStreak <- startStreak + 2}

        # wrong placement
      } else {
        wrong <- T
        streak <- 0
      }

      # determine longest streak
      if(streak > longestStreak){longestStreak <- streak}

    } #end frame wise




    # -------------------------------------------------------------------------- #
    # store data
    # -------------------------------------------------------------------------- #
    trialDat <- list(df_subSet$sub[1], df_subSet$timepoint[1], df_subSet$practice[1], df_subSet$block[1],
                     df_subSet$angle[1], df_subSet$load[1], df_subSet$placement[1],
                     df_subSet$trial[1], df_subSet$display[1],completionTime,
                     encodingTime, Nfixated, nObjectsEncoded,
                     corr_Feat,locOnly,IDOnly,startStreak, longestStreak)

    mem_dat <- rbindlist(list(mem_dat, trialDat))

  } # end subsets
  mem_dat
} # end function

