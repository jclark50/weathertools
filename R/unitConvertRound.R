################################################################################################!

#' Convert and round a vector between units (lightweight conversions)
#'
#' Converts numeric vectors from one unit to another using weathertools' internal
#' conversion table (see \code{.convert_units()}) and rounds the result to a
#' specified number of decimal places. This function does not use the \pkg{units}
#' package and only supports unit pairs present in the internal conversion table.
#'
#' @param x Numeric vector (optionally carrying \code{attr(x, "unit")}).
#' @param from Character. Original unit of \code{x} (e.g., \code{"degC"}, \code{"m/s"}).
#' @param to Character. Desired target unit (e.g., \code{"degF"}, \code{"mph"}).
#' @param digits Integer. Number of decimal places to round the converted values (default \code{2}).
#' @param strip Logical. If \code{TRUE}, remove the unit attribute from the result (default \code{FALSE}).
#'
#' @return A numeric vector of converted values, rounded to \code{digits}. By default,
#'   \code{attr(out, "unit")} is set to the canonical target unit \code{to}.
#'
#' @details
#' This function calls \code{.convert_units(x, from, to)} and then rounds the result.
#' If a requested conversion is not present in the internal table, an error is thrown.
#'
#' If \code{x} already has \code{attr(x, "unit")} and \code{from} is \code{NULL},
#' the attribute is used as the source unit. If both are provided and disagree,
#' an error is thrown unless \code{strip = TRUE} is used to explicitly drop unit
#' tagging on output (conversion still requires a valid \code{from}).
#'
#' @examples
#' # Convert temperature from degC to degF
#' wbgt_c <- c(20, 25, 30)
#' unitConvertRound(wbgt_c, from = "degC", to = "degF", digits = 1)
#'
#' # Convert wind speed from m/s to mph
#' speed_ms <- c(5, 10, 15)
#' unitConvertRound(speed_ms, from = "m/s", to = "mph", digits = 2)
#'
#' # Attribute-driven source unit (from inferred)
#' x <- c(0, 10)
#' attr(x, "unit") <- "degC"
#' unitConvertRound(x, from = NULL, to = "degF", digits = 1)
#'
#' @export
unitConvertRound <- function(x, from, to, digits = 2, strip = FALSE) {

  # infer / validate 'from' against attr(x,"unit") if present
  u_attr <- .norm_u(attr(x, "unit", exact = TRUE))
  if (is.null(from) || (length(from) == 1L && is.na(from))) {
    from_use <- u_attr
    if (is.na(from_use)) {
      stop("unitConvertRound(): 'from' is NULL and x has no 'unit' attribute.", call. = FALSE)
    }
  } else {
    from_use <- .norm_u(from)
    if (!is.na(u_attr) && !identical(u_attr, from_use)) {
      stop(sprintf("unitConvertRound(): attr(x,'unit')='%s' but from='%s'.", u_attr, from_use), call. = FALSE)
    }
  }

  to_use <- .norm_u(to)
  if (is.na(to_use)) stop("unitConvertRound(): 'to' is missing/unknown.", call. = FALSE)

  out <- .convert_units(as.numeric(x), from = from_use, to = to_use)
  out <- round(out, digits)

  if (!strip) attr(out, "unit") <- to_use
  out
}
