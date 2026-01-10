#' Convert and Round a Vector Between Units
#'
#' @description
#' `convert_units_round` converts numeric vectors from one unit to another using the
#' \pkg{units} package, then rounds the result to a specified number of decimal places.
#' This can handle temperature (e.g., "degC" → "degF"), speed ("m/s" → "mi/h"),
#' and any other convertible units supported by \pkg{units}.
#'
#' @param x Numeric or units object. The values to convert.
#' @param from Character. Original unit of \code{x} (e.g., "degC", "m/s").
#' @param to Character. Desired target unit (e.g., "degF", "mi/h").
#' @param digits Integer. Number of decimal places to round the converted values (default 1).
#'
#' @return A numeric vector of the converted and rounded values.
#'
#' @details
#' Internally, this function wraps \code{set_units(x, from)} and \code{set_units(..., to)},
#' then drops the units and applies rounding. It requires that the \pkg{units} package be
#' installed and that the requested units are compatible for conversion.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert WBGT from °C to °F
#' wbgt_c <- c(20, 25, 30)
#' unitConvertRound(wbgt_c, from = "degC", to = "degF", digits = 1)
#'
#' # Convert wind speed from m/s to mi/h
#' speed_ms <- c(5, 10, 15)
#' unitConvertRound(speed_ms, from = "m/s", to = "mi/h", digits = 2)
#' }
unitConvertRound <- function(x, from, to, digits = 2, strip = FALSE) {
  if (!requireNamespace("units", quietly = TRUE)) {
    stop("Package 'units' is required for unit conversions. Please install it.")
  }
  # If x has units, ensure they match `from`
  if (inherits(x, "units")) {
    current <- as.character(units(x))
    if (!identical(current, from)) {
      stop(sprintf("Input has units '%s' but 'from' is '%s'", current, from))
    }
  }
  # Perform conversion: assign units if needed, then convert
  converted <- x %>%
    units::set_units(from, mode = "standard") %>%
    units::set_units(to,    mode = "standard")
  # Round
  rounded <- round(converted, digits)
  # Strip units if requested
  if (strip) {
    return(as.numeric(rounded))
  }
  rounded
}