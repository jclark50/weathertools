#' Solar zenith information
#'
#' @param sun Date-time(s) at which to compute solar geometry.
#' @param lon Longitude in decimal degrees.
#' @param lat Latitude in decimal degrees.
#' @return A list or data.frame with zenith-related quantities (document what you return).
#' @export
zenithinfo = function(sun, lon, lat){
  rad <- pi/180
  hourAngle <- sun$solarTime + lon - 180
  cosZenith <- (sin(rad * lat) * sun$sinSolarDec + cos(rad *
                                                         lat) * sun$cosSolarDec * cos(rad * hourAngle))
  cosZenith[cosZenith > 1] <- 1
  cosZenith[cosZenith < -1] <- -1
  acos(cosZenith)/rad
}

################################################################################################!
