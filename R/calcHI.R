#' Compute Heat Index (fast parallel C++ core)
#'
#' Calculates Heat Index (HI) from air temperature and relative humidity. Handles
#' input unit declaration via argument and/or \code{attr(x, "unit")}. The C++ core
#' computes HI in Fahrenheit; output can be returned in \code{"degF"} or \code{"degC"}.
#'
#' @section Unit handling:
#' \itemize{
#'   \item If \code{ignoreattr = FALSE} (default), the function requires
#'         \code{attr(airTemp, "unit")} and enforces agreement with \code{inputunits}
#'         when provided; otherwise it errors on mismatch/missing attribute.
#'   \item If \code{ignoreattr = TRUE}, the function accepts either the attribute or
#'         \code{inputunits} (but needs at least one).
#'   \item Internally, temperature is normalized to \code{"degF"} before calling the C++ core.
#' }
#'
#' @param airTemp Numeric vector of air temperature. May carry \code{attr(., "unit")}.
#' @param relativeHumidity Numeric vector of RH in percent (0-100).
#' @param inputunits Character or \code{NULL}; one of \code{"degC"}, \code{"degF"}, \code{"K"}.
#'   Only needed if \code{ignoreattr = TRUE} and no attribute is present.
#' @param outputunits Character; \code{"degF"} (default) or \code{"degC"} for the returned HI.
#' @param roundby Integer; decimal places to round the output (default \code{1}).
#' @param returnWithUnits Logical; if \code{TRUE}, sets \code{attr(result, "unit")} to \code{outputunits}.
#' @param ignoreattr Logical; see Unit handling.
#' @param debug Logical; if \code{TRUE}, emit a brief trace of unit decisions.
#'
#' @return Numeric vector of Heat Index in \code{outputunits}. If
#'   \code{returnWithUnits = TRUE}, the result has \code{attr(x, "unit") = outputunits}.
#'
#' @examples
#' # Attribute-driven (degC -> converted internally to degF for core)
#' ta <- c(30, 35, 40); attr(ta, "unit") <- "degC"
#' rh <- c(50, 60, 65)
#' hiF <- calcHI(ta, rh, outputunits = "degF")
#' attr(hiF, "unit")  # "degF"
#'
#' # Explicit input units, Celsius output
#' hiC <- calcHI(ta, rh, inputunits = "degC", outputunits = "degC")
#'
#' # Ignore attribute and trust supplied units
#' attr(ta, "unit") <- "degF"  # wrong on purpose
#' hi_ok <- calcHI(ta, rh, inputunits = "degC", outputunits = "degF", ignoreattr = TRUE)
#'
#' # data.table pattern
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   DT <- data.table::data.table(ta = c(30, 35), rh = c(50, 60))
#'   attr(DT$ta, "unit") <- "degC"
#'   DT[, hi := calcHI(ta, rh, outputunits = "degC")]
#'   attr(DT$hi, "unit")  # "degC"
#' }
#'
#' @seealso \code{\link{unit}} for lightweight unit tagging.
#' @export
calcHI <- function(airTemp, relativeHumidity,
                    inputunits      = NULL,      # "degC" | "degF" | "K" (only needed if ignoreattr=TRUE and no attr)
                    outputunits     = "degF",    # "degF" | "degC"
                    roundby         = 1,
                    returnWithUnits = TRUE,
                    ignoreattr      = FALSE,
                    debug           = FALSE) {

  # ---- helpers ----
  norm_u <- function(u) {
    if (is.null(u)) return(NA_character_)
    u <- trimws(as.character(u))
    if (!nzchar(u)) return(NA_character_)
    u <- gsub("\\\\","",u); u <- gsub("deg","",u,fixed=TRUE); u <- toupper(u)
    if (u %in% c("C","DEGC","CELSIUS")) return("degC")
    if (u %in% c("F","DEGF","FAHRENHEIT")) return("degF")
    if (u %in% c("K","KELVIN")) return("K")
    u
  }
  convT <- function(x, from, to) {
    from <- norm_u(from); to <- norm_u(to)
    if (is.na(from) || is.na(to)) stop("Unknown temperature units (from=", from, ", to=", to, ")")
    if (identical(from,to)) return(x)
    xC <- if (from=="degF") (x-32)*5/9 else if (from=="K") x-273.15 else x
    if (to=="degF") return(xC*9/5+32)
    if (to=="K")    return(xC+273.15)
    xC
  }
  attr_u <- function(x) {
    u <- attr(x, "unit", exact = TRUE)
    if (is.null(u)) return(NA_character_)
    norm_u(u)
  }

  # ---- decide source unit per strict rules ----
  u_attr <- attr_u(airTemp)
  u_arg  <- norm_u(inputunits)

  if (!ignoreattr) {
    # Require attribute
    if (is.na(u_attr))
      stop("airTemp has no 'unit' attribute and ignoreattr = FALSE. Set attr(airTemp,'unit') or call with ignoreattr=TRUE and inputunits.")
    # If user also provided inputunits, enforce agreement
    if (!is.na(u_arg) && !identical(u_arg, u_attr))
      stop(sprintf("Conflicting units: attr(airTemp)='%s' but inputunits='%s'.", u_attr, u_arg))
    u_src <- u_attr
  } else {
    # ignoreattr = TRUE: accept attr OR inputunits, but need one
    u_src <- if (!is.na(u_arg)) u_arg else u_attr
    if (is.na(u_src))
      stop("No units provided. With ignoreattr=TRUE you must supply inputunits or set attr(airTemp,'unit').")
  }

  # ---- normalize numerics to degF for HI core ----
  at <- as.numeric(airTemp)
  # Convert any source to degF (HI core returns degF)
  if (u_src != "degF") {
    at <- if (u_src == "K") convT(at, "K", "degF") else convT(at, u_src, "degF")
  }

  if (debug) {
    message(sprintf("calcHI2: u_src=%s, at(F)[1]=%s, RH[1]=%s",
                    u_src, if (length(at)) at[1] else NA,
                    if (length(relativeHumidity)) relativeHumidity[1] else NA))
  }

  # C++ core returns degF
  hi_f <- calcHI_parallel(at, as.numeric(relativeHumidity))

  # Convert to requested output
  u_out <- norm_u(outputunits)
  if (is.na(u_out)) stop("outputunits must be 'degF' or 'degC'.")
  hi <- if (u_out == "degC") (hi_f - 32) * 5/9 else hi_f

  hi <- round(hi, roundby)
  if (returnWithUnits) attr(hi, "unit") <- u_out
  hi
}

# ################################################################################################!
# calcHI <- function(airTemp, relativeHumidity,
                   # inputunits  = "degF",
                   # outputunits = "degF",
                   # roundby     = 1,
                   # returnWithUnits = FALSE) {
  # require(units)
  
  # # 1) If input is in degC, convert to degF for HI calculation
  # if (inputunits == "degC") {
    # airTemp <- set_units(airTemp, "degC")  # make sure it's typed
    # airTemp <- set_units(airTemp, "degF")  # convert to degF
  # }
  
  # # 2) Convert to plain numeric (in degF)
  # at <- as.numeric(airTemp)
  
  # # 3) Calculate HI using the C++ function (returns degF)
  # hi_f <- calcHI_parallel(at, relativeHumidity)
  
  # # 4) Convert result to output units
  # if (outputunits == "degC") {
    # hi <- set_units(hi_f, "degF")
    # hi <- set_units(hi, "degC")
  # } else {
    # hi <- set_units(hi_f, "degF")
  # }
  
  # # 5) Round result
  # hi <- round(hi, roundby)
  
  # if (returnWithUnits == FALSE){
    # hi = as.numeric(hi)
  # }
  
  # return(hi)
# }
