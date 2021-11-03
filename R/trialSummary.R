#df_main
#WM_dat
#SEARCH_dat
trialSummary <- function(df_main, WM_dat, SEARCH_dat, n_displays = 112){



    trialSum <- data.table(
      sub = numeric(),
      session = numeric(),
      timepoint = numeric(),
      practice = character(),
      block = numeric(),
      angle = numeric(),
      placement = numeric(),
      distractor = character(),
      #modelID = character(),
      trial = numeric(),
      display = numeric(),
      displayCounter = numeric(),
      completionTime = numeric(),
      NSequences = numeric(),
      meanFeatures = numeric(),
      meanFeaturesNo0 = numeric(),
      encodingSum = numeric(),
      encodingMean = numeric(),
      searchTimeSum = numeric(),
      searchTimeMean = numeric(),
      headRotationSum = numeric()
    )

    #df_main$dispCounter <- df_main$display
    #WM_dat$dispCounter <- WM_dat$display
    SEARCH_dat$dispCounter <- SEARCH_dat$display
    #df_main[df_main$session ==2]$dispCounter <- df_main[df_main$session ==2]$dispCounter + 56
    #WM_dat[WM_dat$session ==2]$dispCounter <- WM_dat[WM_dat$session ==2]$dispCounter + 56
    SEARCH_dat[SEARCH_dat$session ==2]$dispCounter <- SEARCH_dat[SEARCH_dat$session ==2]$dispCounter + 56

    # -- 1: get combinations of subs/timepoint/session/phase/ to avoid nested loops
    subSets <- expand.grid(TP = unique(df_main$timepoint),
                           phase = unique(df_main$practice),       subs = unique(df_main$sub))

    displays <- n_displays*length(unique(df_main$sub))*length(unique(df_main$timepoint))
    pb <- progress::progress_bar$new(total = displays, clear = T) # progress bar


    for(n in 1:nrow(subSets)){

      # ------ get correct subset of data frame:
      df_subSet <- df_main[sub == subSets[n,]$subs &  timepoint == subSets[n,]$TP &
                             practice == subSets[n,]$phase]

      WM_subSet <- WM_dat[sub == subSets[n,]$subs &  timepoint == subSets[n,]$TP &
                            practice == subSets[n,]$phase]

      SEARCH_subSet <- SEARCH_dat[sub == subSets[n,]$subs & timepoint == subSets[n,]$TP &
                                  practice == subSets[n,]$phase]


      for(disp in 1:n_displays){

        if(subSets[n,]$phase == "test"){ pb$tick()}

        df_disp <- df_subSet[dispCounter == disp]

        # RAW DATA ----------
        completionTime <- df_disp$timeExp[nrow(df_disp)] - df_disp$timeExp[1]

        # WM DAT -----------
        NSequences <- nrow(WM_subSet[dispCounter == disp ])
        meanFeatures <- mean(WM_subSet[dispCounter == disp]$featuresWM)

        meanFeaturesNo0 <- mean(WM_subSet[dispCounter == disp & featuresWM != 0]$featuresWM)

        encodingSum <- sum(WM_subSet[WM_subSet$dispCounter == disp]$dwellTimeModel)

        encodingMean <- mean(WM_subSet[WM_subSet$dispCounter == disp]$dwellTimeModel)

        headRotationSum <- sum(WM_subSet[WM_subSet$dispCounter == disp]$headRotation)

        # SEARCH dat ---------

        searchTimeSum <- sum(SEARCH_subSet[SEARCH_subSet$dispCounter == disp]$searchTime)

        searchTimeMean <- mean(SEARCH_subSet[SEARCH_subSet$dispCounter == disp]$searchTime)


        single_disp <- list(df_disp$sub[1], df_disp$session[1],df_disp$timepoint[1],df_disp$practice[1], df_disp$block[1],
                            df_disp$angle[1], df_disp$placement[1], df_disp$load[1], #df_disp$modelID[1],
                            df_disp$trial[1], df_disp$display[1], disp,
                            completionTime, NSequences, meanFeatures, meanFeaturesNo0, encodingSum, encodingMean, searchTimeSum, searchTimeMean, headRotationSum)

        trialSum <- rbindlist(list(trialSum, single_disp)) # add to data

      } # end for disp in 1:96
    } # end subset
  trialSum
}

