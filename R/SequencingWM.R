#' Read in data files for Object Sorting Task in VR.
#'
#' Does not explicitely return data but assigns it to global variable inside the function.
#' @export



#' get seq_data
SequencingWM <- function(df_main, sequenceFrames,  cutoff = 2){

  pb <- progress::progress_bar$new(total = length(sequence_start)) # progress bar

  # prepare data frame to store sequence data
  seq_data <- data.table(
    seqID = numeric(),
    seqStart = numeric(),
    seqEnd = numeric(),
    sub = numeric(),
    session = numeric(),
    timepoint = numeric(),
    practice = character(),
    block = numeric(),
    angle = numeric(),
    distractor = character(),
    placement = numeric(),
    trial = numeric(),
    display = numeric(),
    ItemsPlaced = numeric(),
    timeSeqStart = numeric(),
    timeSeqEnd   = numeric(),
    headRotation = numeric(),
    headRotation_yaw = numeric(),
    headRotation_angle = numeric(),
    featuresWM = numeric(),
    IDfeatures = numeric(),
    LOCfeatures = numeric(),
    grabStart = logical(),
    grabEnd = logical(),
    grabErrors = numeric(),
    grabErrorObjects = character(),
    locationError = numeric(),
    fineMotorError = numeric(),
    distRefixate = numeric(),
    distTotal = numeric(),
    refixateGrabbedTarget= logical(),
    dwellTimeModel = numeric(),
    countModelItems = numeric(),
    countModelItemsNoCutoff = numeric(),
    features_encoded = numeric(),
    modelItem1 = numeric(),
    modelItem2 = numeric(),
    modelItem3 = numeric(),
    modelItem4 = numeric(),
    modelItem5 = numeric(),
    modelItem6 = numeric(),
    modelItem7 = numeric(),
    modelItem8 = numeric(),
    modelItem1_dt = numeric(),
    modelItem2_dt = numeric(),
    modelItem3_dt = numeric(),
    modelItem4_dt = numeric(),
    modelItem5_dt = numeric(),
    modelItem6_dt = numeric(),
    modelItem7_dt = numeric(),
    modelItem8_dt = numeric())


  features_carried <- c()
  location_features <- c()
  identity_features <- c()

  df_main[, sequenceID := 0]


  # ------------------------------------------------------------------ #
  # GO THROUGH EACH SEQUENCE
  # -------------------------------------------------------------------#
  # go through all displays:
  # avoid overlap between displays!
  seqID <- 0
  for(seqs_display in sequence_start){

    pb$tick()

    # now look at all sequences associated with this display!
    # last frame is end frame so do not use it as a start!
    for(seq in 1:(length(seqs_display)-1)){
      seqID <- seqID + 1

      # ---------------- #
      # ---- prepare --- #
      identity <- location <-  grabErrors <- 0
      dwell_time   <-0
      grabErrorObjects <- c() # store objects
      fineMotorError <- locationerror <- 0
      fixatedObjects <- c()
      count <- 0
      features_encoded <- 0

      headRotation <- 0


      # new display:
      if(seq == 1){
        placedObjects <- c()
      }

      # easier indexing:
      start <- seqs_display[seq]
      end <- seqs_display[seq + 1] -1 # frame before new start


      df_main[start:end, sequenceID:= seqID]



      # ------------------------------------------------------------------ #
      # FRAME WISE COMPUTATIONS
      # -------------------------------------------------------------------#
      for(frame in start:end){



        # ---- headrotation: sum up ----- #
        # get distance between two frames and add up over sequence


        # ------ get all object names of already placed objects --- #
        if(!is.na(df_main$correctPlacement[frame])){
          if(df_main$correctPlacement[frame] == T){
            placedObjects <- c(placedObjects, df_main$chosenSlotTarget[frame])
          }
        }


        # ------ GRABSTART ----- #
        # check if sequence starts with target object grabbed
        if(df_main$grabbed[start] == T & df_main$isTarget[start] == T){
          grabStart <- T
        }
        else{
          grabStart  <-  F
          distRefixate <- distTotal <- -1}

        # has something been grabbed on the frame before
        if(frame == 1){
          grabbed_before <- F
          seen_before <- "undefined"
        }else{
          grabbed_before <- df_main$grabbed[frame-1]
          seen_before <- df_main$gazeArea[frame-1]
        }


        # --------------------------#
        # --- IDENTITY CARRIIED --- #
        # --------------------------#
        # --> object has been picked up --> +1 idenity features
        if(df_main$grabbed[frame] == T  # something is grabbed
           & grabbed_before == F # before that nothing was grabbed
           & df_main$isTarget[frame] == T # grabbed object is target
           & df_main$gazeArea[frame] != "workspaceArea" ){ # was grabbed from resource table! (not after placement error in workspace)

          # count up
          identity <- identity + 1
          # ----- grabError -------- #
        } else if(
          df_main$grabbed[frame] == T &
          grabbed_before == F &
          df_main$isTarget[frame] == F &
          df_main$gazeArea[frame] != "workspaceArea" ){

          grabErrors <- grabErrors +1
          grabErrorObjects <- c(grabErrorObjects, df_main$grabbedTargetName[frame])
        }

        # ----MODEL DWELL TIME ---- #
        if(df_main$gazeArea[frame] == "modelArea"){
          dwell_time <- dwell_time + df_main$timeFrame[frame]}



        # ------------------------------------------- #
        # WHICH OBJECTS ARE REALLY LOOKED AT IN MODEL:
        # ------------------------------------------- #

        currObj <- df_main$gazeModel_Item[frame] # which object is looked at right now

        if(frame == start){oldObj <- ""
        }else oldObj <- df_main$gazeModel_Item[frame-1] # which object was looked at in earlier frame?

        # count consecutive frames!
        if(currObj == oldObj & currObj != ""){ # same object in next frame and not looking at nothing
          count <- count + 1
        }

        # new object recorded:
        if(currObj != oldObj){
          # store if it exceeds cutoff threshold
          if(count >= cutoff){

            # how many features are carried in sequence?
            # object we looked at: is it already placed?
            if(!(oldObj %in% placedObjects) && oldObj != "" && oldObj != "none"
               && !(oldObj %in% fixatedObjects)){ # only count each object once!
              features_encoded <- features_encoded + 2
            }
            # store object
            fixatedObjects <- c(fixatedObjects, oldObj)
          }
          count <- 0 # reset count
        }

      } # end frame loop


      # ------------------------------------------------------------------ #
      # SUMMARY MEASURES --> all frames of one sequence are relevant
      # -------------------------------------------------------------------#

      #---------------------------- #
      # ------ LOCATION FEATURE---- #
      # --------------------------- #
      # --> how many objects have been placed in sequence ? --> + location feature
      location  <- location + (length(unique(df_main$Nplaced[start:end])) - 1)

      # GRAB END
      if(df_main$grabbed[end] == T & df_main$isTarget[end] == T){
        grabEnd <- T
        distRefixate <- df_main$distTargetCell[end]
        distTotal <- df_main$totalDistTargetCell[end]}
      else {
        grabEnd <- F
        distRefixate <- distTotal <- NA}

      # ---- ENCODED ITEMS ----- #

      Items <- unique(df_main$gazeModel_Item[start:end])

      # --- how many objects have been looked at in model
      countModelItemsNoCutoff <- length(Items)

      if("" %in% Items){
        countModelItemsNoCutoff <- countModelItemsNoCutoff - 1
        #Items <- Items[Items != ""]
      }

      if("none" %in% Items){
        countModelItemsNoCutoff <- countModelItemsNoCutoff - 1
        #Items <- Items[Items!="none"]
      }

      Items <- unique(fixatedObjects)
      countModelItems <- length(Items)


      if("none" %in% Items){
        countModelItems <- countModelItems - 1
        Items <- Items[Items!="none"]
      }


      # fill up with NAs to index later!
      store_Items <- c(Items, rep(NA, (8-countModelItems))) # c(Items, rep(NA, (8)))#
      dt_items <-  c(rep(NA,8)) #store dwell time items

      idx <- 1
      # how long has each object been looked at during sequence?
      for(item in Items){
        # get sum of frames in sequence where object was fixated
        dt_items[idx] <- sum(df_main[start:end,][gazeModel_Item == item]$timeFrame)
        idx <- idx + 1
      }

      # was grabed object refixated?
      if(grabStart & is.element(df_main$grabbedTargetName[start], Items) == T){
        refixateGrabbedTarget <- T

        # substract identity feature of object looked at.. we already have it grabbed!
        features_encoded <- features_encoded - 1

      } else if (grabStart & is.element(df_main$grabbedTargetName[start], Items) == F){
        refixateGrabbedTarget <- F
      } else {refixateGrabbedTarget <- NA}




      # ---- ERRORS ----- #
      grabErrorObjects <- toString(grabErrorObjects) # store wrongly grabbed objects

      # PLACEMENT RELATED:
      placements <- df_main[start:end][!is.na(correctPlacement)]

      if(nrow(placements > 0)){
        placements$validPlacement <- ifelse(placements$chosenSlot == placements$correctSlot, T, F)

        fineMotorError <-  sum(placements[correctPlacement == F]$validPlacement == T)
        locationError <- sum(placements[correctPlacement == F]$validPlacement == F)

      }else {fineMotorError <- locationError <- 0}


      # headrotations
      headRotation <- sum(df_main$headRotation[start:end])
      headRotation_yaw <- sum(abs(df_main$headRotation_yaw[start:end]))
      headRotation_angle <- 0


      # ------------------------------------ #
      # ----- store ------------------------ #
      location_features <- c(location_features, location)
      identity_features <- c(identity_features, identity)
      features_carried <- location + identity




      # ---- store in data frame --- #
      single_seq <- list(seqID, start, end, df_main$sub[start], df_main$session[start],df_main$timepoint[start], df_main$practice[start], df_main$block[start],
                         df_main$angle[start], df_main$load[start], df_main$placement[start],
                         df_main$trial[start], df_main$display[start],df_main$Nplaced[start],
                         df_main$timeExp[start], df_main$timeExp[end],
                         headRotation, headRotation_yaw, headRotation_angle, features_carried, identity, location, grabStart, grabEnd, grabErrors, grabErrorObjects, locationError, fineMotorError,
                         distRefixate, distTotal, refixateGrabbedTarget, dwell_time,
                         countModelItems, countModelItemsNoCutoff,features_encoded,
                         store_Items[1],store_Items[2],store_Items[3],store_Items[4],store_Items[5],store_Items[6],store_Items[7],store_Items[8],
                         dt_items[1], dt_items[2], dt_items[3], dt_items[4], dt_items[5], dt_items[6],dt_items[7], dt_items[8])

      seq_data <- rbindlist(list(seq_data, single_seq)) # add to data


      ## ---- also store things in df_main ----- ##
    }
  }

  seq_data$dispCounter <- seq_data$display
  seq_data[seq_data$session ==2]$dispCounter <- seq_data[seq_data$session ==2]$dispCounter + 56
  seq_data
}
