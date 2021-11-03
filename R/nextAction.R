#' Read in data files for Object Sorting Task in VR.
#'
#'
#' Does not explicitely return data but assigns it to global variable inside the function.


nextAction <- function(end, df_main, grabEnd, searchID, sub, phase){

  # how far do we need to look ahead:
  if(searchID != length(searchFrame)){
    next_sequence <- searchFrame[[searchID+1]]
    new_start <-next_sequence[1]
  } else{ new_start <- end}

  df_next <- df_main[(end+1):new_start]


  # next search is in new display --> do not look further than display end!!
  if(length(unique(df_next$display)) != 1){

    #ndisplaysSub <- length(unique(df_main[sub == sub & practice == phase]$display))

    #if(df_main$display[end] == ndisplaysSub ){}
    display_now <- df_main$display[end]

    new_start <- df_next$frameGlobal[min(which(df_next$display != (display_now)))]
  }

  # search ends with grabbing object:
  if(grabEnd){

    # look through next frames:
    df_next <- df_main[(end+1):new_start]
    NplacedNow <- df_main$Nplaced[end]


    # is something placed before next sequence?
    if((NplacedNow+1) %in% df_next$Nplaced){
      placementFrame <- min(which(df_next$Nplaced == (NplacedNow+1)))
      placed <- T
    } else { placed <- F}

    # if something was placed: did we look at model before that?
    if(placed == T){
      modelGaze <- ifelse("modelArea" %in% df_next$gazeArea[1:placementFrame], T, F)

      # record action
      action <- ifelse(modelGaze == T, "sampling", "placement")
    } else{
      modelGaze <- ifelse("modelArea" %in% df_next$gazeArea[1:nrow(df_next)], T, F)

      action <- ifelse(modelGaze == F, "searching", "sampling")
    }


  } else {

    modelGaze <- ifelse("modelArea" %in% df_next$gazeArea[1:nrow(df_next)], T, F)

    action <- ifelse(modelGaze == F, "searching", "sampling")
  }
  action
}
