#' Convert u/v wind components to direction and speed
#'
#' Converts u (zonal, positive eastward) and v (meridional, positive northward)
#' wind components into meteorological wind direction (degrees from which the
#' wind is blowing) and wind speed.
#'
#' @param u Numeric vector. Zonal wind component.
#' @param v Numeric vector. Meridional wind component.
#'
#' @return A numeric matrix with two columns: `wd` (wind direction degrees,
#'   [0, 360)) and `ws` (wind speed, same units as `u`/`v`).
#'
#' @examples
#' uv2wdws(u = c(1, 0, -1), v = c(0, 1, 0))
#'
#' @export
uv2wdws <- function(u, v) {
  radians_to_degrees <- 180 / pi
  mathdegs <- atan2(v, u) * radians_to_degrees
  wd <- (270 - ifelse(mathdegs > 0, mathdegs, mathdegs + 360)) %% 360
  ws <- sqrt(u^2 + v^2)
  cbind(wd = wd, ws = ws)
}
