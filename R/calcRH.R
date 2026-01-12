#' Compute Relative Humidity from Temperature and Dew Point (or VPD)
#'
#' Computes RH (%) given air temperature and dew point, or alternatively
#' air temperature and vapor pressure deficit (VPD, kPa). Handles temperature
#' unit declaration via argument and/or \code{attr(x, "unit")}.
#'
#' @section Unit handling:
#' \itemize{
#'   \item Recognized temperature tokens: \code{"degC"}, \code{"degF"}, \code{"K"}.
#'   \item \code{inputunits} applies to \code{airTemp} (and \code{dewPoint} if provided).
#'   \item If \code{ignoreattr = FALSE} (default), the function requires unit attributes
#'         on provided temperature vectors and enforces agreement with \code{inputunits}
#'         when specified.
#'   \item If \code{inputunits == "K"}, values are normalized to \code{"degC"} for the core.
#' }
#'
#' @param airTemp Numeric vector of air temperature. May carry \code{attr(., "unit")}.
#' @param dewPoint Numeric vector of dew point temperature (optional if \code{vpd} is supplied).
#'                 May carry \code{attr(., "unit")}.
#' @param inputunits Character; temperature unit token for \code{airTemp}/\code{dewPoint}.
#'   One of \code{"degC"}, \code{"degF"}, \code{"K"} (also accepts \code{"C"}, \code{"F"}).
#'   Defaults to \code{"degC"}.
#' @param vpd Numeric vector of vapor pressure deficit in kPa. If provided, \code{dewPoint} is ignored.
#' @param ignoreattr Logical; if \code{TRUE}, relax attribute checks and trust \code{inputunits}.
#' @param debug Logical; if \code{TRUE}, emit a brief trace of unit decisions.
#'
#' @return Numeric vector of relative humidity in percent (0-100). No unit attribute is attached.
#'
#' @details
#' Internally, temperatures are normalized to \code{"degC"} or \code{"degF"} depending on
#' \code{inputunits}, then passed to a C++ routine. When \code{vpd} is supplied, \code{dewPoint}
#' is ignored and RH is computed from \code{airTemp} and \code{vpd}.
#'
#' @examples
#' # Using dew point
#' ta <- c(30, 31); dp <- c(20, 21)
#' attr(ta, "unit") <- "degC"; attr(dp, "unit") <- "degC"
#' rh <- calcRH(ta, dp, inputunits = "degC")
#' rh  # percent
#'
#' # Using VPD (kPa), ignoring attributes and declaring units
#' taF <- c(86, 88); vpd <- c(1.5, 2.0)
#' rh2 <- calcRH(taF, vpd = vpd, inputunits = "degF", ignoreattr = TRUE)
#'
#' # data.table pattern
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   DT <- data.table::data.table(ta = ta, dp = dp)
#'   attr(DT$ta, "unit") <- "degC"; attr(DT$dp, "unit") <- "degC"
#'   DT[, rh := calcRH(ta, dewPoint = dp, inputunits = "degC")]
#' }
#'
#' @seealso \code{\link{calcTD}}, \code{\link{unit}}.
#' @export
calcRH <- function(airTemp, dewPoint = NULL,
                    inputunits = "degC",      # accepts: "degC","C","degF","F","K"
                    vpd = NULL,               # kPa; if provided, dewPoint is ignored
                    ignoreattr = FALSE,
                    debug = FALSE) {

  # ---- helpers ----
  norm_u <- function(u) {
    if (is.null(u)) return(NA_character_)
    u <- trimws(as.character(u)); if (!nzchar(u)) return(NA_character_)
    u <- gsub("\\\\","",u); u <- gsub("deg","",u, fixed=TRUE); u <- toupper(u)
    if (u %in% c("C","DEGC","CELSIUS")) return("degC")
    if (u %in% c("F","DEGF","FAHRENHEIT")) return("degF")
    if (u %in% c("K","KELVIN")) return("K")
    u
  }
  convT <- function(x, from, to) {
    from <- norm_u(from); to <- norm_u(to)
    if (is.na(from) || is.na(to)) stop("Unknown temperature units (from=", from, ", to=", to, ")")
    if (identical(from, to)) return(x)
    xC <- if (from=="degF") (x-32)*5/9 else if (from=="K") x-273.15 else x
    if (to=="degF") return(xC*9/5+32)
    if (to=="K")    return(xC+273.15)
    xC
  }
  attr_u <- function(x) { u <- attr(x, "unit", exact=TRUE); if (is.null(u)) NA_character_ else norm_u(u) }

  # normalize requested working unit; for K we'll work in degC
  u_req  <- norm_u(inputunits); if (is.na(u_req)) stop("inputunits must be one of: 'degC','C','degF','F','K'")
  u_work <- if (u_req == "K") "degC" else u_req

  # enforce strictness
  uA <- attr_u(airTemp)
  uD <- if (!is.null(dewPoint)) attr_u(dewPoint) else NA_character_

  if (!ignoreattr) {
    if (is.na(uA)) stop("airTemp has no 'unit' attribute and ignoreattr = FALSE.")
    if (!is.null(dewPoint) && is.na(uD)) stop("dewPoint has no 'unit' attribute and ignoreattr = FALSE.")
    # if user also passed inputunits explicitly and conflicts with attr, error
    if (!is.na(norm_u(inputunits)) && !is.na(uA) && u_req != uA)
      stop(sprintf("Conflicting units: attr(airTemp)='%s' but inputunits='%s'.", uA, u_req))
    if (!is.null(dewPoint) && !is.na(norm_u(inputunits)) && !is.na(uD) && u_req != uD)
      stop(sprintf("Conflicting units: attr(dewPoint)='%s' but inputunits='%s'.", uD, u_req))
  }

  # numeric copies
  at <- as.numeric(airTemp)
  dp <- if (!is.null(dewPoint)) as.numeric(dewPoint) else NULL

  # convert from attributes (or from K request) to working unit
  if (!is.na(uA) && uA != u_work) at <- convT(at, uA, u_work)
  if (!is.null(dp) && !is.na(uD) && uD != u_work) dp <- convT(dp, uD, u_work)
  if (u_req == "K") { at <- convT(at, "K", "degC"); if (!is.null(dp)) dp <- convT(dp, "K", "degC") }

  if (debug) message(sprintf("calcRH2: u_req=%s, u_work=%s; at[1]=%s; dp[1]=%s",
                             u_req, u_work, if (length(at)) at[1] else NA,
                             if (!is.null(dp) && length(dp)) dp[1] else NA))

  # call C++ (expects 'degC' or 'degF')
  if (is.null(vpd)) {
    if (is.null(dp)) stop("Provide dewPoint or vpd.")
    calcRH_cpp(at, dp, u_work)
  } else {
    calcRH_vpd_cpp(at, vpd, u_work)
  }
}

# ################################################################################################!
# calcRH <- function(airTemp, dewPoint, inputunits = "degC", vpd = NULL, ignoreattr = FALSE, quiet = FALSE) {
    # # Debug: Print input units
    # # cat("Input units:", inputunits, "\n")
    # # inputunits = gsub('\\\u00b0',"",toupper(inputunits))
    
    # # Process input units
    # # inputunits <- switch(toupper(inputunits), 
                         # # C = '\\\u00b0C',
                         # # # C = "degC",    # Match units() output for Celsius
                         # # F = "degF",  # Match units() output for Fahrenheit
                         # # stop(paste("Unsupported input units:", inputunits, ". Use 'C' or 'F'.")))
    
    # # Debug: Print processed input units
    # # cat("Processed Input Units:", inputunits, "\n")
    
    # # inputunits = gsub("\\\\","",inputunits)
    
    
    # # Check attribute compatibility unless ignoreattr is TRUE
    # if (!ignoreattr) {
      # if (units(airTemp)$numerator != inputunits) {
        # stop("Input air temperature units do not match specified inputunits.")
      # }
      # if (units(dewPoint)$numerator != inputunits) {
        # stop("Input dewpoint units do not match specified inputunits.")
      # }
    # }
    
    # # Convert units to numeric
    # airTemp_numeric <- as.numeric(airTemp)
    # dewPoint_numeric <- as.numeric(dewPoint)
    
    # if (nchar(inputunits) == 2) {
      # inputunits <- substr(inputunits, 2, 3)
      # # clean_string <- str_remove_all(inputunits, "\\p{S}")  # Matches Unicode symbols
    # }

    # # Calculate relative humidity
    # if (is.null(vpd)) {
      # relativeHumidity <- calcRH_cpp(airTemp_numeric, dewPoint_numeric, inputunits)
    # } else {
      # relativeHumidity <- calcRH_vpd_cpp(airTemp_numeric, vpd, inputunits)
    # }
    
    # return(relativeHumidity)
  # }
