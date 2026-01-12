#' Sea-level pressure from station pressure
#'
#' Converts station pressure (in millibars / hPa) to sea-level pressure using a
#' standard atmosphere approximation and the station elevation.
#'
#' @param pressureMB Numeric vector. Station pressure in millibars (hPa).
#' @param airTemp Numeric vector. Air temperature at the station. Units set by
#'   `inputunits`.
#' @param elevation Numeric vector or length-1 scalar. Station elevation. Units
#'   set by `elevUnits`.
#' @param inputunits Character scalar. Temperature units: `"degC"` (default) or
#'   `"degF"`.
#' @param elevUnits Character scalar. Elevation units: `"m"` (default) or `"ft"`.
#' @param ignoreattr Logical.
#' @param quiet Logical.
#'
#' @return Numeric vector of sea-level pressure (mb / hPa), same length as
#'   `pressureMB`.
#'
#' @details
#' If `elevation` is a scalar, it is recycled to match `pressureMB`.
#'
#' @examples
#' # Example: convert 1000 mb at 20C and 100 m elevation
#' calcPres(pressureMB = 1000, airTemp = 20, elevation = 100)
#'
#' # Vectorized
#' calcPres(pressureMB = c(995, 1002), airTemp = c(15, 18), elevation = 120)
#'
#' @export
calcPres <- function(pressureMB, airTemp, elevation, inputunits = "degC",
                     elevUnits = "m", ignoreattr = TRUE, quiet = TRUE) {

  if (tolower(elevUnits) == "ft") {
    if (length(elevation) == 1) {
      elevation <- rep(elevation * 0.3048, length(pressureMB))
    } else {
      elevation <- elevation * 0.3048
    }
  }

  if (toupper(inputunits) == "degF") {
    # Convert Fahrenheit to Celsius locally (no dependency on ftoc()).
    # ignoreattr/quiet retained for backward-compatibility with prior signature.
    airTemp <- (airTemp - 32) * (5 / 9)
  }

  slp <- pressureMB * ((1 - ((0.0065 * elevation) / (airTemp +
                                                       0.0065 * elevation + 273.15))) ^ -5.257)

  slp
}
