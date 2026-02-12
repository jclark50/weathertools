#' Convert miles per hour to meters per second (lightweight unit attributes)
#'
#' Converts wind speed from \code{"mph"} to \code{"m/s"} using lightweight unit
#' handling via \code{attr(x, "unit")}. This function does not use the \pkg{units}
#' package.
#'
#' @section Unit handling:
#' \itemize{
#'   \item If \code{ignoreattr = FALSE} (default), \code{thedata} must have
#'         \code{attr(thedata, "unit")} that resolves to \code{"mph"}; otherwise an error is thrown.
#'   \item If \code{ignoreattr = TRUE}, the attribute is ignored and the input is assumed to be \code{"mph"}.
#'   \item Output is a numeric vector in \code{"m/s"} with \code{attr(out, "unit") = "m/s"}.
#' }
#'
#' @param thedata Numeric vector of wind speed in miles per hour. May carry \code{attr(., "unit")}.
#' @param ignoreattr Logical; if \code{FALSE}, require and validate \code{attr(thedata, "unit")} is \code{"mph"}.
#' @param quiet Logical; if \code{FALSE}, emit a short message about the conversion.
#' @param return_original_as_attr Logical; if \code{TRUE}, attach \code{attr(out, "original")}
#'   containing the original numeric input (assumed/validated \code{"mph"}).
#' @param ... Reserved for backward compatibility (ignored).
#'
#' @return Numeric vector of wind speed in meters per second. The result carries
#'   \code{attr(out, "unit") = "m/s"}. If \code{return_original_as_attr = TRUE},
#'   \code{attr(out, "original")} holds the original numeric miles-per-hour values.
#'
#' @examples
#' x <- c(0, 10, 25)
#' attr(x, "unit") <- "mph"
#' y <- mphtoms(x)
#' y
#' attr(y, "unit")  # "m/s"
#' attr(y, "original")
#'
#' # Ignore missing attribute (assume mph)
#' mphtoms(c(5, 15), ignoreattr = TRUE)
#'
#' @export
mphtoms <- function(thedata, ignoreattr = FALSE, quiet = TRUE,
                    return_original_as_attr = TRUE, ...) {

  x_num <- as.numeric(thedata)

  if (!ignoreattr) {
    u <- .norm_u(attr(thedata, "unit", exact = TRUE))
    if (is.na(u)) {
      stop("mphtoms(): input has no 'unit' attribute. Set attr(x,'unit') <- 'mph' or call with ignoreattr=TRUE.", call. = FALSE)
    }
    if (!identical(u, "mph")) {
      msg <- sprintf("mphtoms(): expected attr(thedata,'unit') == 'mph' but got '%s'.", u)
      cat("From unit is ", .crayon_red("mph"), " but unit attribute is ", .crayon_red(u), "\n", sep = "")
      stop(msg, call. = FALSE)
    }
  }

  out <- .convert_units(x_num, from = "mph", to = "m/s")

  if (!quiet) message("Converted mph to m/s")

  attr(out, "unit") <- "m/s"
  if (return_original_as_attr) attr(out, "original") <- x_num
  out
}
