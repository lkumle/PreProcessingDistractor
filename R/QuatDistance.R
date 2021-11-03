#' Read in data files for Object Sorting Task in VR.
#'
#' @param frameGlobal frame no for which distance should be computed (in relation to frame before)
#' @export
#'
QuatDistance <- function(frameGlobal, df_main){

    #df_main <- as.data.table(df_main)

    if(is.na(df_main$head_yaw2[frameGlobal])){

      distance <- 0
    } else {

      # get head yaw, pitch, roll & quat_w from data frame
      q1 <- as.matrix(df_main[,head_yaw2:head_quat_w2][frameGlobal]) # frame before
      q2 <- as.matrix(df_main[,head_yaw:head_quat_w][frameGlobal]) # current frame

      # get distance:
      # --> Huynh, D. Q. (2009). Metrics for 3D rotations: Comparison and analysis.
      #                          Journal of Mathematical Imaging and Vision, 35(2), 155-164.)
      distance <- min(base::norm((q1 + q2)), base::norm(q1-q2))

      # return distance
    }

  distance

} # end function


angle <- function(frameGlobal, df_main){

  if(frameGlobal == nrow(df_main)){
    theta <- 0
  }else{
    x <- c(df_main$head_x[frameGlobal], df_main$head_y[frameGlobal])
    y <- c(df_main$head_x2[frameGlobal], df_main$head_y2[frameGlobal])


    dot.prod <- x%*%y
    norm.x <- norm(x,type="2")
    norm.y <- norm(y,type="2")
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
  }



  return(theta)
}



