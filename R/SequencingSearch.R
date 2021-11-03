#' Read in data files for Object Sorting Task in VR.
#'
#'
#' Does not explicitely return data but assigns it to global variable inside the function.
#' @export

SequencingSearch <- function(df_main, searchFrames, WM_dat, cutoff = 2){

  searchData <- data.table(
    searchID = numeric(),
    seachStart = numeric(),
    seachEnd = numeric(),
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
    seqID  = numeric(),
    countSearchSequence = numeric(),
    featuresWM = numeric(),
    IDfeatures = numeric(),
    LOCfeatures = numeric(),
    searchTime = numeric(),
    ItemsFixated = numeric(),
    ItemsfixatedCutoff = numeric(),
    uniqueFixatedCutoff = numeric(),
    timePerObject = numeric(),
    timePerFixation = numeric(),
    nTargets = numeric(),
    nTargetsUnique = numeric(),
    timePerTarget = numeric(),
    timePerTargetFixation = numeric(),
    nDistractors = numeric(),
    nDistractorsUnique = numeric(),
    timePerDistractor = numeric(),
    timePerDistractorFixation = numeric(),
    findObject = logical(),
    ObjectName = character(),
    isTarget = logical(),
    encodingTimeObject = numeric(),
    nObjectsEncodedSequence = numeric(),
    nFeaturesEncodedSequence = numeric(),
    dwellTimeModel = numeric(),
    lastEncoded = character(),
    longestEncoded = character(),
    nextAction = character(),
    grabStart = logical(),
    dwellTime_obj1 = numeric(),
    dwellTime_obj2 = numeric(),
    obj1 = character(),
    obj2 = character()
  )
  pb <- progress::progress_bar$new(total = length(searchFrames)) # progress bar

  searchID <- 0
  seq <- 0 # start with sequence 0
  countSearch <- 1


  # go through each search
  for(search in searchFrames){


    searchID <- searchID +1
    start <- search[1]
    end <- search[2]

    df_search <- df_main[start:end]

    count <- 0 # prepare for later
    fixatedObjects <- c() # store objects that exeed threshhold

    # which sequence belongs to last search?
    old_seq <- seq

    # which sequence belongs to current search?
    seq <- df_main$sequenceID[start]

    # still same sequence?
    if(old_seq == seq){
      countSearch <- countSearch + 1
    } else {countSearch <- 1}


    # get data belonging to current search
    sequence <- WM_dat[seqID == seq]

    pb$tick()


    # go through each frame in seach
    for(frame in search[1]:search[2]){


      # ---------------------------------- #
      # PICKED UP OBJECT:
      # ---------------------------------- #
      grabEnd <- df_main$grabbed[end + 1] # did sub find something

      # if yes: which one was grabbed
      grabbedObject <- df_main$grabbedTargetName[end + 1]
      isTarget <- df_main$isTarget[end + 1] # was it a target?

      # if yes: was it looked at in model right before?


      # ---------------------------------- #
      # PRECEDING EXTERNAL SAMPLING
      # ---------------------------------- #

      # which objects where looked at in model before search?
      encoded_objects <- c(as.character(sequence[ ,modelItem1:modelItem8]))
      encoded_objects <- encoded_objects[!is.na(encoded_objects)]

      nencoded <- length(encoded_objects)

      # how long have objects been looked at?
      encodingTimes <- c(as.numeric(sequence[ ,modelItem1_dt:modelItem8_dt]))
      encodingTimes <- encodingTimes[!is.na(encodingTimes)]

      if(nencoded > 0){
        # what was last object to be looked at in model?
        lastEncoded <- tail(encoded_objects, 1)
        longestEncoded <- encoded_objects[which.max(encodingTimes)]

      } else{
        lastEncoded <- NA
        longestEncoded <- NA}


      if(grabbedObject %in% encoded_objects){
        idx <- which(encoded_objects == grabbedObject)
        encodingTimeObject <- encodingTimes[idx]

      } else{
        encodingTimeObject <- NA
      }


      # how many targets fixated during current search? (from what sub just looked at)


      # how many distractors fixated during currect search? + time spend on distractors

      # fixated earlier in sequence

      # how many reinits fixated?


      # ---------------------------------- #
      # WHICH OBJECTS ARE REALLY LOOKED AT:
      # ---------------------------------- #
      # are they distractors or targets?
      # how long are they looked at?

      currObj <- df_main$gazeResource_Item[frame] # which object is looked at right now

      if(frame == start){oldObj <- ""
      }else oldObj <- df_main$gazeResource_Item[frame-1] # which object was looked at in earlier frame?

      # count consecutive frames!
      if(currObj== oldObj & currObj != ""){ # same object in next frame and not looking at nothing
        count <- count + 1
      }

      # new object recorded:
      if(currObj != oldObj){

        # store if it exeeds cutoff threshold
        if(count >= cutoff){
          fixatedObjects <- c(fixatedObjects, oldObj)}

        count <- 0 # reset count
      }




    }

    # ------------------- summary statistics ------------------------ #

    # what is next action?
    action <- nextAction(end, df_main, grabEnd, searchID)

    # how long did search take
    searchtime <- df_main$timeExp[end] - df_main$timeExp[start]

    # how many object looked at during search?
    ItemsFixatedSearch <- unique(df_search$gazeResource_Item)
    NItemsFixatedSearchTotal  <- length(ItemsFixatedSearch)

    if("" %in% ItemsFixatedSearch){
      NItemsFixatedSearchTotal <- NItemsFixatedSearchTotal - 1
    }

    NItemsfixatedCutoff <- length(fixatedObjects)
    uniquefixatedCutoff <- length(unique(fixatedObjects))

    # encoding time for first two objects:
    dwell_times <- c(as.character(sequence[ ,modelItem1_dt:modelItem2_dt]))
    dwellTime_obj1 <- dwell_times[1]
    dwellTime_obj2 <- dwell_times[2]

    obj <- c(as.character(sequence[ ,modelItem1:modelItem2]))
    obj1 <- obj[1]
    obj2 <- obj[2]

    # -------------
    # time per object/target/distarctor statistics
    timePerObject <- searchtime/NItemsfixatedCutoff
    timePerFixation <- searchtime/NItemsFixatedSearchTotal

    # DISTRACTORS
    Distractors <- grep("D", fixatedObjects , value = T)

    nDistractors <- length(Distractors)
    nDistractorsUnique <- length(unique(Distractors))

    timePerDistractor <- sum(df_search[gazeResource_Item %in% Distractors,]$timeFrame)/nDistractorsUnique
    timePerDistractorFixation <- sum(df_search[gazeResource_Item %in% Distractors,]$timeFrame)/nDistractors

    # TARGETS
    Targets <- fixatedObjects[!grepl("D", fixatedObjects)]

    nTargets <- length(Targets)
    nTargetsUnique <- length(unique(Targets))

    timePerTarget <- sum(df_search[gazeResource_Item %in% Targets,]$timeFrame)/nTargetsUnique
    timePerTargetFixation <- sum(df_search[gazeResource_Item %in% Targets,]$timeFrame)/nTargets





    # -------------------------------------------------------------------------------------------------------------------------------------------
    # store data
    single_search <- list(searchID, search[1], search[2], df_main$sub[start], df_main$session[start],df_main$timepoint[start],df_main$practice[start], df_main$block[start],
                          df_main$angle[start], df_main$load[start], df_main$placement[start], df_main$trial[start], df_main$display[start],
                          df_main$Nplaced[start+5], df_main$timeExp[start], df_main$timeExp[end], # basic information
                          df_main$sequenceID[start], countSearch, sequence$featuresWM, sequence$IDfeatures, sequence$LOCfeatures,
                          searchtime, NItemsFixatedSearchTotal, NItemsfixatedCutoff, uniquefixatedCutoff,timePerObject, timePerFixation,
                          nTargets, nTargetsUnique, timePerTarget, timePerTargetFixation,
                          nDistractors, nDistractorsUnique, timePerDistractor, timePerDistractorFixation,
                          grabEnd, grabbedObject, isTarget,encodingTimeObject,
                          nencoded, sequence$features_encoded, sequence$dwell_time, lastEncoded, longestEncoded, action, sequence$grabStart,
                          dwellTime_obj1, dwellTime_obj2, obj1, obj2)

    searchData <- rbindlist(list(searchData, single_search))
  }


  searchData
}
