#' Convert meters per second to miles per hour (lightweight unit attributes)
#'
#' Converts wind speed from \code{"m/s"} to \code{"mph"} using lightweight unit
#' handling via \code{attr(x, "unit")}. This function does not use the \pkg{units}
#' package.
#'
#' @section Unit handling:
#' \itemize{
#'   \item If \code{ignoreattr = FALSE} (default), \code{windspeed} must have
#'         \code{attr(windspeed, "unit")} that resolves to \code{"m/s"}; otherwise an error is thrown.
#'   \item If \code{ignoreattr = TRUE}, the attribute is ignored and the input is assumed to be \code{"m/s"}.
#'   \item Output is a numeric vector in \code{"mph"} with \code{attr(out, "unit") = "mph"}.
#' }
#'
#' @param windspeed Numeric vector of wind speed in meters per second. May carry \code{attr(., "unit")}.
#' @param ignoreattr Logical; if \code{FALSE}, require and validate \code{attr(windspeed, "unit")} is \code{"m/s"}.
#' @param quiet Logical; if \code{FALSE}, emit a short message about the conversion.
#' @param return_original_as_attr Logical; if \code{TRUE}, attach \code{attr(out, "original")}
#'   containing the original numeric input (assumed/validated \code{"m/s"}).
#' @param ... Reserved for backward compatibility (ignored).
#'
#' @return Numeric vector of wind speed in miles per hour. The result carries
#'   \code{attr(out, "unit") = "mph"}. If \code{return_original_as_attr = TRUE},
#'   \code{attr(out, "original")} holds the original numeric meters-per-second values.
#'
#' @examples
#' x <- c(0, 5, 10)
#' attr(x, "unit") <- "m/s"
#' y <- mstomph(x)
#' y
#' attr(y, "unit")  # "mph"
#'
#' # Ignore missing attribute (assume m/s)
#' mstomph(c(5, 7), ignoreattr = TRUE)
#'
#' @export
mstomh <- function(windspeed, ignoreattr = FALSE, quiet = TRUE,
                   return_original_as_attr = TRUE, ...) {

  # --- capture original numeric ---
  x_num <- as.numeric(windspeed)

  # --- validate units attribute unless ignoreattr ---
  if (!ignoreattr) {
    u <- .norm_u(attr(windspeed, "unit", exact = TRUE))
    if (is.na(u)) {
      stop("mstomh(): input has no 'unit' attribute. Set attr(x,'unit') <- 'm/s' or call with ignoreattr=TRUE.", call. = FALSE)
    }
    if (!identical(u, "m/s")) {
      stop(sprintf("mstomh(): expected attr(windspeed,'unit') == 'm/s' but got '%s'.", u), call. = FALSE)
    }
  }

  # --- convert (m/s -> mph) ---
  out <- .convert_units(x_num, from = "m/s", to = "mph")

  if (!quiet) message("Converted m/s to mph")

  attr(out, "unit") <- "mph"
  if (return_original_as_attr) attr(out, "original") <- x_num
  out
}
