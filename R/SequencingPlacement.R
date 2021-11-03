#' Read in data files for Object Sorting Task in VR.
#'
#' Does not explicitely return data but assigns it to global variable inside the function.
#' @export



SequencingPlacement <- function(df){

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
