#' Read in data files for Object Sorting Task in VR.
#'
#' @param path directory in which data is stored.
#' @param main indicates if main files (distractor task) should be loaded
#' @param memory indicates if memory files should be loaded
#'
#' Does not explicitely return data but assigns it to global variable inside the function.
#' @export

readData <- function(path, main = T, memory = T){

  # handle working directors
  OLD <- getwd()
  setwd(path)

  # -----------------------------------
  # SUMMARY FILES: ---------------------
  if(main == T){

    print("----------------- READING IN  MAIN FILES--------------- ")

    # get file names
    files_summary <- list.files(path, pattern = "^sorting_MAIN")
    writeLines(files_summary) # check which files we read in

    # read in and combine
    data <- data.table::rbindlist(sapply(files_summary, data.table::fread, simplify = FALSE),
                       use.names = TRUE)
  }



  # -----------------------------------
  # RECALL FILES: ---------------------
  if(memory == T){

    #get file names
    print("----------------- READING IN  MEMORY FILES--------------- ")
    files_recall <- list.files(path, pattern = "^sorting_MEMORY")
    writeLines(files_recall) # check which files we read in

    # read in and combine
    data<- data.table::rbindlist(sapply(files_recall, data.table::fread, simplify = FALSE),
                            use.names = TRUE)

  }
  setwd(OLD)

  data
}
