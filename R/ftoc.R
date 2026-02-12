################################################################################################!

#' Convert Fahrenheit to Celsius (lightweight unit attributes)
#'
#' Converts temperatures expressed in Fahrenheit to Celsius using lightweight unit
#' handling via \code{attr(x, "unit")}. This function does not use the \pkg{units}
#' package.
#'
#' @section Unit handling:
#' \itemize{
#'   \item If \code{ignoreattr = FALSE} (default), \code{f} must have
#'         \code{attr(f, "unit")} that resolves to \code{"degF"}; otherwise an error is thrown.
#'   \item If \code{ignoreattr = TRUE}, the attribute is ignored and the input is assumed to be Fahrenheit.
#'   \item Output is a numeric vector in Celsius with \code{attr(out, "unit") = "degC"}.
#' }
#'
#' @param f Numeric vector of temperatures in Fahrenheit. May carry \code{attr(., "unit")}.
#' @param ignoreattr Logical; if \code{FALSE}, require and validate \code{attr(f, "unit")} is Fahrenheit.
#' @param quiet Logical; if \code{FALSE}, emit a short message about the conversion.
#' @param return_original_as_attr Logical; if \code{TRUE}, attach \code{attr(out, "original")}
#'   containing the original numeric input (assumed/validated Fahrenheit).
#' @param ... Reserved for backward compatibility (ignored).
#'
#' @return Numeric vector of temperatures in Celsius. The result carries
#'   \code{attr(out, "unit") = "degC"}. If \code{return_original_as_attr = TRUE},
#'   \code{attr(out, "original")} holds the original numeric Fahrenheit values.
#'
#' @examples
#' x <- c(32, 68, 77)
#' attr(x, "unit") <- "degF"
#' y <- ftoc(x)
#' y
#' attr(y, "unit")  # "degC"
#'
#' # Ignore missing attribute (assume degF)
#' ftoc(c(32, 212), ignoreattr = TRUE)
#'
#' @export
ftoc <- function(f, ignoreattr = FALSE, quiet = TRUE,
                 return_original_as_attr = TRUE, ...) {

  # --- capture original numeric ---
  f_num <- as.numeric(f)

  # --- validate units attribute unless ignoreattr ---
  if (!ignoreattr) {
    u <- .norm_temp_unit(attr(f, "unit", exact = TRUE))
    if (is.na(u)) {
      stop("ftoc(): input has no 'unit' attribute. Set attr(x,'unit') <- 'degF' or call with ignoreattr=TRUE.", call. = FALSE)
    }
    if (!identical(u, "degF")) {
      stop(sprintf("ftoc(): expected attr(f,'unit') == 'degF' but got '%s'.", u), call. = FALSE)
    }
  }

  # --- convert (F -> C) ---
  out <- .temp_convert(f_num, from = "degF", to = "degC")

  if (!quiet) message("Converted degF to degC")

  attr(out, "unit") <- "degC"
  if (return_original_as_attr) attr(out, "original") <- f_num
  out
}
