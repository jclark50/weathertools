#' Compute Dew Point Temperature (fast C++ core)
#'
#' Calculates dew point from air temperature and relative humidity using a C++
#' implementation. Inputs can be declared via \code{inputunits} and/or inferred
#' from \code{attr(x, "unit")} on \code{airTemp}. Output is returned in the
#' requested unit and, by default, tagged with a lightweight \code{"unit"} attribute.
#'
#' @section Unit handling:
#' \itemize{
#'   \item Recognized temperature tokens: \code{"degC"}, \code{"degF"}, \code{"K"}.
#'   \item If \code{ignoreattr = FALSE} (default), the function reads
#'         \code{attr(airTemp, "unit")} and enforces agreement with \code{inputunits}
#'         when provided; otherwise it errors on conflict or missing attribute.
#'   \item If \code{ignoreattr = TRUE}, the function trusts \code{inputunits}
#'         (or the attribute if \code{inputunits} is missing).
#'   \item Internally, the C++ core expects either \code{"degC"} or \code{"degF"}.
#'         If \code{inputunits == "K"}, values are normalized to \code{"degC"}.
#' }
#'
#' @param airTemp Numeric vector of air temperature. May carry \code{attr(., "unit")}.
#' @param relativeHumidity Numeric vector of RH in percent (0–100).
#' @param inputunits Character; temperature units of \code{airTemp}. One of
#'   \code{"degC"}, \code{"degF"}, \code{"K"}. Defaults to \code{"degC"}.
#' @param outputunits Character; desired dew point units. One of
#'   \code{"degC"}, \code{"degF"}. Defaults to \code{"degC"}.
#' @param roundby Integer; number of decimal places to round the result (performed in C++).
#' @param returnWithUnits Logical; if \code{TRUE}, sets \code{attr(result, "unit")} to \code{outputunits}.
#' @param ignoreattr Logical; if \code{TRUE}, skip attribute checks and rely on \code{inputunits}.
#' @param debug Logical; if \code{TRUE}, emit a brief trace of unit decisions.
#'
#' @return Numeric vector of dew point in \code{outputunits}. If
#'   \code{returnWithUnits = TRUE}, the result has \code{attr(x, "unit") = outputunits}.
#'
#' @examples
#' # Simple vector (attribute-driven)
#' ta <- c(30, 31, 29); attr(ta, "unit") <- "degC"
#' rh <- c(50, 55, 60)
#' dpC <- calcTD(ta, rh, outputunits = "degC")
#' attr(dpC, "unit")  # "degC"
#'
#' # Override: declare input as Fahrenheit, request Fahrenheit output
#' taF <- c(86, 88, 84); attr(taF, "unit") <- "degF"
#' dpF <- calcTD(taF, rh, inputunits = "degF", outputunits = "degF")
#'
#' # Ignore attribute and trust inputunits
#' attr(taF, "unit") <- "degC"  # wrong on purpose
#' dp_ok <- calcTD(taF, rh, inputunits = "degF", outputunits = "degC", ignoreattr = TRUE)
#'
#' # data.table pattern
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   DT <- data.table::data.table(ta = ta, rh = rh)
#'   attr(DT$ta, "unit") <- "degC"
#'   DT[, dp := calcTD(ta, rh, outputunits = "degC")]
#'   attr(DT$dp, "unit")
#' }
#'
#' @seealso \code{\link{unit}} for lightweight unit tagging.
#' @export
calcTD <- function(airTemp, relativeHumidity,
                          inputunits      = "degC",   # "degC" | "degF" | "K"
                          outputunits     = "degC",   # "degC" | "degF"
                          roundby         = 2,
                          returnWithUnits = TRUE,
                          ignoreattr      = FALSE,
                          debug           = FALSE) {

  # helpers (same as above)
  norm_u <- function(u) {
    if (is.null(u)) return(NA_character_)
    u <- trimws(as.character(u)); if (!nzchar(u)) return(NA_character_)
    u <- gsub("\\\\","",u); u <- gsub("°","",u, fixed=TRUE); u <- toupper(u)
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
  attr_u <- function(x) { u <- attr(x, "unit", exact=TRUE); if (is.null(u)) NA_character_ else norm_u(u) }

  # target tokens
  u_in  <- norm_u(inputunits);  if (is.na(u_in))  stop("inputunits must be 'degC','degF','K'.")
  u_out <- norm_u(outputunits); if (is.na(u_out)) stop("outputunits must be 'degC' or 'degF'.")

  uA <- attr_u(airTemp)

  if (!ignoreattr) {
    if (is.na(uA)) stop("airTemp has no 'unit' attribute and ignoreattr = FALSE.")
    if (!is.na(u_in) && u_in != uA)
      stop(sprintf("Conflicting units: attr(airTemp)='%s' but inputunits='%s'.", uA, u_in))
  }

  # numeric temp, convert from attr (or K request) to requested input unit for C++
  at <- as.numeric(airTemp)
  if (!is.na(uA) && uA != u_in) at <- convT(at, uA, if (u_in == "K") "degC" else u_in)
  if (u_in == "K") { at <- convT(at, "K", "degC"); u_in <- "degC" }  # C++ expects degC/degF only

  if (debug) message(sprintf("calcTD2: u_in=%s, u_out=%s; at[1]=%s; RH[1]=%s",
                             u_in, u_out, if (length(at)) at[1] else NA,
                             if (length(relativeHumidity)) relativeHumidity[1] else NA))

  # C++ dewpoint returns in the units you request in outputunits (degC/degF)
  dp <- calcDewpoint_cpp(at, as.numeric(relativeHumidity), u_in, u_out, roundby)

  if (returnWithUnits) attr(dp, "unit") <- u_out
  dp
}

# ################################################################################################!
# Calculate dew point temperature from air temperature (Celsius) and relative humidity (percent)
# calcTD <- function(airTemp, relativeHumidity, inputunits = "degC", outputunits = "degC", 
                           # roundby = 2, ignoreattr = FALSE, quiet = FALSE) {
    
    # # Debug: Print input and output units
    # if (!quiet) {
      # cat("Input units:", inputunits, "\n")
      # cat("Output units:", outputunits, "\n")
    # }
    
    # # Clean input and output units
    # # inputunits <- gsub('\\\u00b0', "", toupper(inputunits))  # Remove degree symbol
    # # outputunits <- gsub('\\\u00b0', "", toupper(outputunits))
    
    # # Process input units
    # # Process input units
    # # inputunits <- switch(toupper(inputunits), 
                         # # C = 'degC',
                         # # # C = "degC",    # Match units() output for Celsius
                         # # 'degF' = "degF",  # Match units() output for Fahrenheit
                         # # stop(paste("Unsupported input units:", inputunits, ". Use 'C' or 'F'.")))
    
    # # # Process input units
    # # outputunits <- switch(toupper(outputunits), 
                         # # C = '\\\u00b0C',
                         # # # C = "degC",    # Match units() output for Celsius
                         # # F = "degF",  # Match units() output for Fahrenheit
                         # # stop(paste("Unsupported output units:", outputunits, ". Use 'C' or 'F'.")))
    
    # # Debug: Print processed units
    # if (!quiet) {
      # cat("Processed Input Units:", inputunits, "\n")
      # cat("Processed Output Units:", outputunits, "\n")
    # }
    
    # # Remove backslashes for clean comparison and ensure consistency
    # inputunits <- gsub("\\\\", "", inputunits)
    # outputunits <- gsub("\\\\", "", outputunits)
    
    # # Check attribute compatibility unless ignoreattr is TRUE
    # if (!ignoreattr) {
      # if (units(airTemp)$numerator != inputunits) {
        # stop("Input air temperature units do not match specified inputunits.")
      # }
    # }
    
    # # Convert air temperature to numeric
    # airTemp_numeric <- as.numeric(airTemp)
    
    # # Debug: Print converted air temperature
    # if (!quiet) cat("Air temperature (numeric):", head(airTemp_numeric), "\n")
    
    # if (nchar(inputunits) == 2) {
      # inputunits <- substr(inputunits, 2, 3)
      # # clean_string <- str_remove_all(inputunits, "\\p{S}")  # Matches Unicode symbols
    # }
    
    # if (nchar(outputunits) == 2) {
      # outputunits <- substr(outputunits, 2, 3)
      # # clean_string <- str_remove_all(inputunits, "\\p{S}")  # Matches Unicode symbols
    # }
    
    # # if (inputunits == 'degF') {
      # # inputunits <- 'F'
    # # }
    # # if (outputunits == 'degF') {
      # # outputunits <- 'F'
    # # }
    
    
    # # Calculate dew point using the C++ function
    # dewpoint <- calcTD_cpp(airTemp_numeric, relativeHumidity, 
                                 # inputunits, outputunits, roundby)
    
    # # Assign appropriate units to the result
    # if (outputunits == "degF") {
      # units(dewpoint) <- "degF"
    # } else {
      # units(dewpoint) <- 'degC'
    # }
    
    # return(dewpoint)
  # }
 