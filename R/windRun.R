#' windRun
#' @export
windRun = function(windSpd, archiveInterval=1, roundBy=5){
  return(round(windSpd/(60/archiveInterval),roundBy))
}


################################################################################################!
