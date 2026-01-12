#' Wind direction degrees to 16-point compass labels
#'
#' Converts wind direction degrees into standard 16-point compass labels:
#' N, NNE, NE, ENE, E, ESE, SE, SSE, S, SSW, SW, WSW, W, WNW, NW, NNW.
#'
#' @param windDeg Numeric vector of wind directions in degrees.
#'
#' @return Character vector of compass labels (same length as `windDeg`).
#'
#' @examples
#' winddeg(c(0, 20, 45, 90, 200, 359.9))
#'
#' @export
winddeg <- function(windDeg) {
  rose_breaks <- c(0, 360 / 32, (1 / 32 + (1:15 / 16)) * 360, 360)
  rose_labs <- c(
    "N", "NNE", "NE", "ENE", "E", "ESE", "SE",
    "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N"
  )

  cut(
    windDeg,
    breaks = rose_breaks,
    labels = rose_labs,
    right = FALSE,
    include.lowest = TRUE
  ) |>
    as.character()
}
