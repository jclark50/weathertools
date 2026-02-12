#' Wind run
#'
#' @param windSpd Numeric vector of wind speed values.
#' @param archiveInterval Numeric or integer. Time interval represented by each wind speed value
#'   (document units: seconds/minutes/hours, whichever you use).
#' @param roundBy Integer; decimal places to round output.
#' @return Numeric vector of wind run (document units).
#' @export
windRun = function(windSpd, archiveInterval=1, roundBy=5){
  return(round(windSpd/(60/archiveInterval),roundBy))
}


################################################################################################!
