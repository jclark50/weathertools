#' Inches of mercury to millibars
#'
#' @param presin Numeric vector of pressure in inches of mercury (inHg).
#' @return Numeric vector of pressure in millibars (hPa).
#' @export
intomb = function(presin){
  return(presin/0.029530)
}

################################################################################################!
