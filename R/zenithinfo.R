#' zenithinfo
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
