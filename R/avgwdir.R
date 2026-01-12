#' Moving-window average wind direction
#'
#' Computes a rolling-mean wind direction using vector (u/v-style) averaging:
#' directions are converted to north/south and east/west components, summed in a
#' rolling window with weights given by wind speed, and converted back to a
#' direction in degrees.
#'
#' @param winddirectionsDeg Numeric vector. Wind direction in degrees (0–360).
#' @param windspeeds Numeric vector. Wind speed (same length as
#'   `winddirectionsDeg`).
#' @param movingWindow Integer. Window size (number of samples) for the rolling sum.
#' @param na.pad Logical. If `TRUE`, pads the leading window with `NA` values to
#'   preserve input length; if `FALSE`, returns only the fully-defined values.
#'
#' @return Numeric vector of averaged wind direction degrees in [0, 360).
#'
#' @details
#' This function performs a rolling sum on weighted wind components and converts
#' back to a direction via `atan2()`. When `na.pad = TRUE`, the result length
#' equals the input length.
#'
#' @examples
#' wd  <- c(350, 10, 15, 20, 25)
#' wsp <- c(5,   5,  5,  5,  5)
#' avgwdir(wd, wsp, movingWindow = 3)
#'
#' @importFrom zoo rollsum
#' @export
avgwdir <- function(winddirectionsDeg, windspeeds, movingWindow = 10, na.pad = TRUE) {

  Cns <- cos((winddirectionsDeg * (pi / 180))) * windspeeds
  Cew <- sin((winddirectionsDeg * (pi / 180))) * windspeeds

  # BUGFIX vs original: pass na.pad argument through, don't hardcode TRUE
  Cns <- zoo::rollsum(Cns, k = movingWindow, na.pad = isTRUE(na.pad), align = "right")
  Cew <- zoo::rollsum(Cew, k = movingWindow, na.pad = isTRUE(na.pad), align = "right")

  avgdirs1 <- atan2(Cew, Cns) * (180 / pi)

  if (isTRUE(na.pad)) {
    avgdirs <- avgdirs1
    # Normalize to [0, 360)
    avgdirs[!is.na(avgdirs) & avgdirs < 0] <- avgdirs[!is.na(avgdirs) & avgdirs < 0] + 360
    avgdirs
  } else {
    avgdirs <- stats::na.omit(avgdirs1)
    avgdirs[avgdirs < 0] <- avgdirs[avgdirs < 0] + 360
    as.numeric(avgdirs)
  }
}
