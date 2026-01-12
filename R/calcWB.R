#' Calculate Wet-Bulb Temperature (strict, attribute-aware)
#'
#' @description
#' Computes wet-bulb temperature (WB) from air temperature and relative humidity,
#' using a compiled C++ routine (`calcWB_cpp`). This version follows your
#' lightweight unit model:
#' - Inputs are plain numerics with an optional `attr(x, "unit")`.
#' - By default (`ignoreattr = FALSE`) a **unit attribute is required** on `airTemp`
#'   (and must match `inputunits` if you also supply it). Set `ignoreattr = TRUE`
#'   to skip attribute checks and rely solely on `inputunits`.
#' - Accepts temperature units `"degC"`, `"degF"`, or `"K"`. When `"K"` is used,
#'   values are converted to `"degC"` internally for the C++ call.
#'
#' `relativeHumidity` may be given as **percent** (0-100) or **fraction** (0-1).
#' Fractions (<= 1.5) are auto-scaled to percent.
#'
#' @param airTemp Numeric. Air temperature. Provide `attr(airTemp, "unit")`
#'   as `"degC"`, `"degF"`, or `"K"` unless `ignoreattr = TRUE`.
#' @param relativeHumidity Numeric. Relative humidity; percent (0-100) or
#'   fraction (0-1). Fractions are auto-converted to percent.
#' @param inputunits Character. Temperature unit of `airTemp`
#'   (`"degF"`, `"degC"`, or `"K"`). Required when `ignoreattr = TRUE`;
#'   if provided together with an attribute, they must agree.
#'   Default: `"degF"`.
#' @param outputunits Character. Desired WB output unit (`"degF"` or `"degC"`).
#'   Default: `"degF"`.
#' @param method Character or `NULL`. Passed through to `calcWB_cpp` to select
#'   an algorithmic variant (keep `NULL` for the default).
#' @param ignoreattr Logical. If `FALSE` (default), require a unit attribute
#'   on `airTemp` and validate against `inputunits` when provided. If `TRUE`,
#'   skip attribute checks and rely only on `inputunits`.
#' @param returnWithUnits Logical. If `TRUE`, tag the numeric result with
#'   `attr(, "unit") = outputunits`. Default: `FALSE`.
#' @param debug Logical. If `TRUE`, print a compact summary of normalized inputs.
#'
#' @return Numeric vector of wet-bulb temperature in `outputunits`. If
#'   `returnWithUnits = TRUE`, the numeric is tagged with `attr(, "unit")`.
#'
#' @examples
#' \dontrun{
#' # Strict mode (attribute required):
#' T <- 90; attr(T, "unit") <- "degF"
#' calcWB(T, 70)                        # returns degF by default
#' calcWB(T, 70, outputunits = "degC")  # convert to degC
#'
#' # Kelvin input:
#' Tk <- 305.15; attr(Tk, "unit") <- "K"
#' calcWB(Tk, 0.6, outputunits = "degC")                 # RH as fraction → auto scaled
#'
#' # Ignore attributes and use explicit units:
#' calcWB(32, 40, inputunits = "degC", ignoreattr = TRUE)
#' }
#' @export
calcWB <- function(airTemp, relativeHumidity,
                   inputunits      = "degF",   # "degF" | "degC" | "K"
                   outputunits     = "degF",   # "degF" | "degC"
                   method          = NULL,
                   ignoreattr      = FALSE,
                   returnWithUnits = TRUE,
                   debug           = FALSE) {
  # ---- tiny helpers (local; no external deps) ----
  norm_u <- function(u) {
    if (is.null(u)) return(NA_character_)
    u <- trimws(as.character(u)); if (!nzchar(u)) return(NA_character_)
    u <- gsub("\\\\", "", u); u <- gsub("deg", "", u, fixed = TRUE); u <- toupper(u)
    if (u %in% c("C","DEGC","CELSIUS")) return("degC")
    if (u %in% c("F","DEGF","FAHRENHEIT")) return("degF")
    if (u %in% c("K","KELVIN")) return("K")
    u
  }
  convT <- function(x, from, to) {
    from <- norm_u(from); to <- norm_u(to)
    if (is.na(from) || is.na(to)) stop("Unknown temperature units (from=", from, ", to=", to, ")")
    if (identical(from, to)) return(x)
    # convert via degC hub
    xC <- if (from == "degF") (x - 32) * 5/9 else if (from == "K") x - 273.15 else x
    if (to == "degF") return(xC * 9/5 + 32)
    if (to == "K")    return(xC + 273.15)
    xC
  }
  attr_u <- function(x) {
    u <- attr(x, "unit", exact = TRUE)
    if (is.null(u)) return(NA_character_)
    norm_u(u)
  }

  # ---- normalize unit arguments ----
  u_in  <- norm_u(inputunits)
  u_out <- norm_u(outputunits)
  if (is.na(u_in)  || !u_in %in% c("degF","degC","K")) stop("inputunits must be one of: 'degF','degC','K'.")
  if (is.na(u_out) || !u_out %in% c("degF","degC"))    stop("outputunits must be 'degF' or 'degC'.")

  # ---- strict handling of airTemp units ----
  u_attr <- attr_u(airTemp)
  if (!ignoreattr) {
    if (is.na(u_attr)) stop("airTemp has no 'unit' attribute and ignoreattr = FALSE.")
    if (!is.na(u_in) && u_in != u_attr)
      stop(sprintf("Conflicting units: attr(airTemp)='%s' but inputunits='%s'.", u_attr, u_in))
    u_src <- u_attr
  } else {
    u_src <- if (!is.na(u_in)) u_in else u_attr
    if (is.na(u_src))
      stop("No temperature units provided. With ignoreattr=TRUE you must supply inputunits or set attr(airTemp,'unit').")
  }

  # ---- normalize temperature to the C++ input unit ----
  at <- as.numeric(airTemp)
  cpp_in <- if (u_src == "degF") "degF" else "degC"  # when K, we convert to degC
  if (u_src == "K") {
    at <- convT(at, "K", "degC")
  } else if (u_src != cpp_in) {
    at <- convT(at, u_src, cpp_in)
  }

  # ---- normalize RH to percent (C++ expects percent 0..100) ----
  rh <- as.numeric(relativeHumidity)
  if (length(rh)) {
    # if it looks like a fraction, scale to percent
    if (all(!is.na(rh) & rh <= 1.5)) rh <- rh * 100
    # optional: clamp to [0,100] to avoid pathological values
    rh[!is.na(rh) & rh < 0]   <- 0
    rh[!is.na(rh) & rh > 100] <- 100
  }

  if (debug) {
    message(sprintf("calcWB: cpp_in=%s, output=%s, at[1]=%s, RH[1]=%s",
                    cpp_in, u_out,
                    if (length(at)) at[1] else NA,
                    if (length(rh)) rh[1] else NA))
  }

  # ---- delegate to C++ (returns in outputunits) ----
  # Signature assumed: calcWB_cpp(airTemp, relativeHumidity, inputunits, outputunits, method)
  wb <- calcWB_cpp(at, rh, cpp_in, u_out, method)

  # ---- optional unit tag on result ----
  if (returnWithUnits) attr(wb, "unit") <- u_out
  wb
}


# #' Calculate Wet-Bulb Temperature
# #'
# #' @details
# #' Calculates the wet-bulb temperature based on air temperature and relative humidity.
# #' This function converts the air temperature from Fahrenheit to Celsius (if needed),
# #' computes the wet-bulb temperature leveraging a compiled C++ function (calcWB_cpp), and then converts the result
# #' back to the desired output units.
# #' 
# #' @param airTemp Numeric or units object. The air temperature. The units are specified by `inputunits`.
# #' @param relativeHumidity Numeric. The relative humidity (in percent, 0-100).
# #' @param inputunits Character. The units of `airTemp`. Defaults to `"degF"`. If `"degF"`, the function
# #'   converts the temperature to Celsius for processing.
# #' @param outputunits Character. The desired output units for the wet-bulb temperature. Defaults to `"degF"`.
# #' @param method Character. The method to use for the calculation. Defaults to `NULL`, which corresponds
# #'   to the normal method. This should only be specified (not normal) if investigating differences between formulations.
# #'
# #' @return A units object representing the wet-bulb temperature in the specified `outputunits`.
# #'
# #' @examples
# #' \dontrun{
# #' # Example 1: Calculate wet-bulb temperature for 90 F air temperature and 70% relative humidity.
# #' # must have either installed 'jj' package or source the cpp file directly.
# #' source('calcWB_cpp.pp') 
# #' wb_temp <- calcWB(airTemp = 90, relativeHumidity = 70)
# #' print(wb_temp)
# #'
# #' # Example 2: Calculate wet-bulb temperature and return the result in Celsius.
# #' wb_temp_c <- calcWB(airTemp = 90, relativeHumidity = 70, outputunits = "degC")
# #' print(wb_temp_c)
# #' }
# #'
# #' @export
# calcWB <- function(airTemp, relativeHumidity, inputunits  = "degF", outputunits = "degF", method = NULL) {
  # # 1) Must be a units object
  # if (!inherits(airTemp, "units")) {
    # stop("`airTemp` must be a units object, e.g. set_units(85, 'degF')")
  # }
  
  # # 2) Verify tag matches inputunits
  # cur_u <- deparse_unit(airTemp)
  # if (cur_u != inputunits) {
    # stop(sprintf("Wrong units: got '%s' but expected '%s'", cur_u, inputunits))
  # }
  
  # # 3) Convert to  for the C++ call
  # airTemp_c <- set_units(airTemp, "degC", mode = "standard")
  
  # # 4) Call C++ (expects numeric)
  # hi_c_num <- calcWB_cpp(as.numeric(airTemp_c), relativeHumidity,
                         # inputunits  = "degC",
                         # outputunits = "degC",
                         # method      = method)
  
  # # 5) Wrap back into units and convert to outputunits
  # hi_u <- set_units(hi_c_num, "degC", mode = "standard")
  # if (outputunits == "degF") {
    # hi_u <- set_units(hi_u, "degF", mode = "standard")
  # }
  
  # return(hi_u)
# }
