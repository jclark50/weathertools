#' Wind chill (US NWS formula)
#'
#' Computes wind chill using the standard U.S. National Weather Service formula.
#'
#' @param airTemp Numeric. Air temperature in degrees Fahrenheit.
#' @param sfcWind Numeric. Wind speed in mph.
#' @param roundBy Integer. Number of decimal places to round to (default 1).
#'
#' @return Numeric vector of wind chill values.
#'
#' @examples
#' calcWindchill(airTemp = 30, sfcWind = 10)
#' calcWindchill(airTemp = c(30, 25), sfcWind = c(10, 15), roundBy = 0)
#'
#' @export
calcWindchill <- function(airTemp, sfcWind, roundBy = 1) {
  wchill <- 35.74 + (0.6215 * airTemp) - (35.75 * sfcWind^0.16) +
    (0.4275 * airTemp * sfcWind^0.16)
  round(wchill, roundBy)
}
