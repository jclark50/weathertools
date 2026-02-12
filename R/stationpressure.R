#' Estimate station pressure from sea-level pressure and elevation
#'
#' @param pressureMB Numeric vector of pressure (typically sea-level) in millibars/hPa.
#' @param airTemp Numeric vector of air temperature.
#' @param elevation Numeric vector of station elevation.
#' @param airTempUnits Character temperature units for `airTemp` (e.g. `"degC"` or `"degF"`).
#' @param elevUnits Character elevation units for `elevation` (e.g. `"m"` or `"feet"`).
#' @return Numeric vector of estimated station pressure (typically in millibars/hPa).
#' @export
stationpressure <- function(pressureMB, airTemp, elevation, airTempUnits="C", elevUnits="m"){
  # warning(("This function is deprecated. Please switch to function: jj::calcPres"))

  #slp = ((df[,presCOL]/33.8639)/(((288-0.0065*(df[,elevCOL]*0.3048))/288)^5.2561)/(0.0295300))
  if (elevUnits == "ft"){
    if (length(elevation)==1){
      elevation = rep(elevation*0.3048, length(pressureMB))
    } else {
      elevation = elevation*0.3048
    }
  }
  if (airTempUnits=="F"){
    airTemp = ftoc(airTemp, ignoreattr = TRUE, quiet = TRUE)
  }
  slp = pressureMB * (( 1 - ((0.0065*elevation)/(airTemp + 0.0065*elevation + 273.15)))^-5.257)
  return(slp)
}

################################################################################################!
