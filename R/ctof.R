#' Convert Celsius to Fahrenheit (lightweight unit attributes)
#'
#' Converts temperatures expressed in Celsius to Fahrenheit using lightweight unit
#' handling via \code{attr(x, "unit")}. This function does not use the \pkg{units}
#' package.
#'
#' @section Unit handling:
#' \itemize{
#'   \item If \code{ignoreattr = FALSE} (default), \code{c} must have
#'         \code{attr(c, "unit")} that resolves to \code{"degC"}; otherwise an error is thrown.
#'   \item If \code{ignoreattr = TRUE}, the attribute is ignored and the input is
#'         assumed to be Celsius.
#'   \item Output is a numeric vector in Fahrenheit with \code{attr(out, "unit") = "degF"}.
#' }
#'
#' @param c Numeric vector of temperatures in Celsius. May carry \code{attr(., "unit")}.
#' @param ignoreattr Logical; if \code{FALSE}, require and validate \code{attr(c, "unit")} is Celsius.
#' @param quiet Logical; if \code{FALSE}, emit a short message about the conversion.
#' @param return_original_as_attr Logical; if \code{TRUE}, attach \code{attr(out, "original")}
#'   containing the original numeric input (assumed/validated Celsius).
#' @param ... Reserved for backward compatibility (ignored).
#'
#' @return Numeric vector of temperatures in Fahrenheit. The result carries
#'   \code{attr(out, "unit") = "degF"}. If \code{return_original_as_attr = TRUE},
#'   \code{attr(out, "original")} holds the original numeric Celsius values.
#'
#' @examples
#' x <- c(0, 25, 30)
#' attr(x, "unit") <- "degC"
#' y <- ctof(x)
#' y
#' attr(y, "unit")  # "degF"
#' attr(y, "original")
#'
#' # Ignore missing attribute (assume Celsius)
#' ctof(c(0, 25), ignoreattr = TRUE)
#'
#' @export
ctof <- function(c, ignoreattr = FALSE, quiet = TRUE,
                 return_original_as_attr = TRUE, ...) {

  # --- capture original numeric ---
  c_num <- as.numeric(c)

  # --- validate units attribute unless ignoreattr ---
  if (!ignoreattr) {
    u <- .norm_temp_unit(attr(c, "unit", exact = TRUE))
    if (is.na(u)) {
      stop("ctof(): input has no 'unit' attribute. Set attr(x,'unit') <- 'degC' or call with ignoreattr=TRUE.", call. = FALSE)
    }
    if (!identical(u, "degC")) {
      msg <- sprintf("ctof(): expected attr(c,'unit') == 'degC' but got '%s'.", u)
      stop(msg, call. = FALSE)
    }
  }

  # --- convert (C -> F) ---
  f_num <- .temp_convert(c_num, from = "degC", to = "degF")

  # --- message ---
  if (!quiet) message("Converted degC to degF")

  # --- attach lightweight units + original ---
  attr(f_num, "unit") <- "degF"
  if (return_original_as_attr) attr(f_num, "original") <- c_num

  f_num
}
