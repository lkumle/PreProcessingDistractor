# --------------------------------------------------------------------------- #
# UTILITY FUNCTIONS FOR OBJECTSORTING TASK: PREPROCESSING
# --------------------------------------------------------------------------- #

# ERRORS ---------
### errors
placementSequencing <- function(df){

  # ------------------ LOCATION ERRORS ---------- #

  # prepare storing data
  locError_dat <- data.table(
    sub = numeric(),
    practice = character(),
    session = numeric(),
    block = numeric(),
    angle = numeric(),
    placement = numeric(),
    trial = numeric(),
    display = numeric(),
    frameID = numeric(),
    frameGlobal = numeric(),
    grabbedObject = numeric(), # object ID of grabbed object
    grabbedObject2 = numeric(),
    isTarget = logical(), # is grabbed object a target? maybe someone tries to place non target object
    locationError = logical(), # was there a location error
    precisionError = logical(), # was it a precision error?
    correctSlot = character(),  # what is the correct slot for grabbed object?
    chosenSlot = character(),   # which one was wrongfully selected?
    distToCorrect = numeric(),  # how far away is chosen slot from correct slot? dManhattan!
    chosenSlotTarget = numeric(), # which target was suppose to go in chosen slot?
    nextAction = character(),
    placementsSub = numeric(),
    placementsAngle = numeric(),
    overallPlacements = numeric() # how many placements where there? --> compute ratios etc
  )

  # 1: only get placing frames
  df_place <- df[!is.na(correctPlacement)]

  # 2: re-check correct placement --> not entirely reliable in unity


  # 3: now go through each error
  #df_error <- df_place[correctPlacement == F]


  # 3. get measures:
  for(er in 1:nrow(df_place)){

    # get grabbed object: requires going backwards in df a little bit!
    globalFrame <- df_place$frameGlobal[er]
    grabInfo <- df$grabbedTargetName[(globalFrame-20):globalFrame]
    grabbedObject <- unique(grabInfo[grabInfo != -1]) # iterate backwards until grabbedTargetName != -1 --> grabbed object

    # isTarget:
    idx <-  globalFrame - (20 - (match(-1, grabInfo)-1)) # get globalFrame number when object still grabbed!
    isTarget <- df$isTarget[idx]
    grabbedObject2 <- df$grabbedTargetName[idx]

    # ERROR specific things
    if(df_place$correctPlacement[row] == F){
      locationError <- ifelse(df_place$chosenSlot[er] == df_place$correctSlot[er], yes = F, no = T)
      precisionError <- ifelse(df_place$chosenSlot[er] == df_place$correctSlot[er], yes = T, no = F)

      distToCorrect <- dManhattan(df_place$chosenSlot[er], df_place$correctSlot[er])

    } else {
      locationError <- precisionError <- distToCorrect <- NA
    }

    # nextAction:

    # 1: get current display "end-frame"
    subcurr <- df_place$sub[er]
    dispcurr <- df_place$display[er]
    jj <- df[sub == subcurr & display == dispcurr]
    endFrame <- tail(jj$frameGlobal, 1)



    #2: loop until end of display
    for(frame in globalFrame:endFrame){

      # if gazeArea == "modelArea" --> sampling
      if(df$gazeArea[frame] == "modelArea"){
        nextAction <- "sampling"
        break

      # if gazeArea == "ressourcesArea" --> searching
      } else if (df$gazeArea[frame] == "ressourceArea"){
        nextAction <- "searching"
        break
      } else{ nextAction <- "undefined"}

      if(frame == endFrame){
        nextAction <- "trialEnd"
      }
    }







    angcurr <- df_place$angle[er]
    currphase <- df_place$practice[er]

    placementsAngle <- nrow(df_place[sub == subcurr & angle == angcurr &  practice == currphase])
    # store metrics
    error_dat <- list(df_place$sub[er],df_place$practice[er], df_place$session[er], df_place$block[er],
                      df_place$angle[er], df_place$placement[er], df_place$trial[er], df_place$display[er],
                      df_place$frame[er], df_place$frameGlobal[er], grabbedObject,grabbedObject2, isTarget,locationError, precisionError,
                      df_place$chosenSlot[er],df_place$correctSlot[er],distToCorrect, df_place$chosenSlotTarget[er], nextAction,
                      nrow(df_place[sub == subcurr]),placementsAngle, nrow(df_place))

    locError_dat <- rbindlist(list(locError_dat, error_dat))

  }

  locError_dat
}

# grab data --> identity errors
# grab data --> identity errors
grabData <- function(df_main, WM_dat, SEARCH_dat){
  grab_dat <- data.table(
    sub = numeric(),
    practice = character(),
    session = numeric(),
    block = numeric(),
    angle = numeric(),
    placement = numeric(),
    trial = numeric(),
    display = numeric(),
    frameGlobal = numeric(),
    sequenceID = numeric(),
    searchID = numeric(),
    grabbedObject = character(),
    isTarget = logical(),
    obj1 = character(),
    obj2 = character(),
    obj3 = character(),
    obj4 = character(),
    dt_1 =  numeric(),
    dt_2 =  numeric(),
    dt_3 =  numeric(),
    dt_4 =  numeric()
  )

  for(frame in 1:nrow(df_main)){


    # what has been looked at before?
    if(frame == 1){ # display just started -> assign before to "undefined"
      before <- "undefined" # avoid spill over from old displays/subjects!
    }else{ # somewhere in the middle of the display --> assign gaze of one frame before
      before <- df_main$grabbed[frame-1]}


    # RECORD START OF SEARCH!
    if(df_main$grabbed[frame] == T # looking at resource
       & before !=  T # before that we did not look at resource
       & df_main$gazeArea[frame] != "workspaceArea" # also not already currently in a search
    ){ # also not already having something grabbed

      grab <- df_main$grabbedTargetName[frame]


      #find sequence ID + search DD
      idx <- which.min(abs(SEARCH_dat$seachEnd - frame))
      searchID <- SEARCH_dat$searchID[idx]
      seqIDg <- SEARCH_dat$sequenceID[idx]

      # get list of encoded objectsin that sequence:
      seq <- WM_dat[seqID == seqIDg]




      grab_dat <- rbindlist(list(grab_dat,
                                 list(df_main$sub[frame], df_main$practice[frame], df_main$session[frame],df_main$block[frame],
                                      df_main$angle[frame], df_main$placement[frame],df_main$trial[frame],df_main$display[frame],
                                      frame, searchID, seqIDg, grab, df_main$isTarget[frame],
                                      seq$modelItem1, seq$modelItem2, seq$modelItem3, seq$modelItem4, seq$modelItem1_dt, seq$modelItem2_dt,
                                      seq$modelItem3_dt, seq$modelItem4_dt
                                 )))

    }
  }
  grab_dat
}



# -----------------------------------------

#' Get distance of two slots in model/workspace
dManhattan <- function(slot1, slot2){

  # prepare:
  slot1 <- as.numeric(str_remove(slot1, "Slot"))
  slot2 <- as.numeric(str_remove(slot2, "Slot"))

  # model grid proxi
  model_grid <- matrix(1:18, ncol = 6, nrow = 3, byrow = T)

  # get coordinates of two slots
  coordinates <- c(which(model_grid == slot1, arr.ind = TRUE), which(model_grid == slot2, arr.ind = TRUE))

  # compute distance
  dist <- dist(matrix(coordinates, ncol = 2, byrow = T), method = "manhattan", diag = F, upper = F)[1]

  # return sist
  dist
}

# compute distance
distancePlacements <- function(df_test){
  ## --------- compute average distance of all displayes --- #

  #unique display counter
  df_test[sub == 3]$display <- df_test[sub == 3]$display + 40


  slots  <- data.table(
    display = numeric(),
    placed = numeric(),
    slot = character())
  # --------------------------- #
  # GET PLACING FRAME
  # -------------------------- #

  for(disp in 1:length(unique(df_test$display))){

    df_display <- df_test[display == disp]
    for(i in 1:8){
      frameID <- min(df_display[df_display$Nplaced == i]$frameGlobal)
      slot <- df_display[frameGlobal == frameID]$correctSlot

      row <- list(disp, i, slot)
      slots <- rbindlist(list(slots, row))
    }
  }

  slots$numSLot <- as.numeric(str_remove(slots$slot, "Slot"))


  dists_emp <- c()
  sums_emp <- c()


  dists_sim <- c()

  # -------- compute sum distance ------ #
  for(disp in 1:length(unique(slots$display))){

    sequence <- slots[display == disp]$numSLot
    sequence <- sequence[!is.na(sequence)]

    sum <- 0


    # --> get actual ddistances of participant
    for(i in 2:length(sequence)){
      dist <- dManhattan(sequence[i-1], sequence[i])
      dists_emp <- c(dists_emp, dist)
      sum <- sum + dist
    }

    sums_emp <- c(sums_emp,sum)


    # --> random
    for(i in 1:10){
      rand_sequence <- sample(sequence)

      for(i in 2:length(rand_sequence)){
        dist <- dManhattan(rand_sequence[i-1], rand_sequence[i])
        dists_sim <- c(dists_sim, dist)
        #sum <- sum + dist
      }
    }
  }


  prob_emp <- table(dists_emp)/length(dists_emp)
  prob_sim <- table(dists_sim)/length(dists_sim)

  if(length(prob_emp) != 7){
    prob_emp <- c(prob_emp, rep(0, (7 - length(prob_emp))))
  }

  strat <- data_frame(prob =  c(prob_emp, prob_sim), cond = c(rep("emp", 7), rep("sim", 7)), dist = c(1:7, 1:7))
  strat # return
}


ObjSimilarity <- function(obj1, obj2){


  similarity_scores <- data.table(read_excel("similarity_scores.xlsx"))

  objs <- sort(c(as.numeric(obj1), as.numeric(obj2)))

  NOUN_n1 <- similarity_scores[ObSort_name == objs[1]]$NOUN_name
  NOUN_n2 <- similarity_scores[ObSort_name == objs[2]]$NOUN_name



  distance <- similarity_scores[Stim_1 == NOUN_n1 & Stim_2 == NOUN_n2]$Distance

}
