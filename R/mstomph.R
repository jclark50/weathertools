#' mstomph
#' @export
mstomph <- function(thedata, ignoreattr = FALSE, quiet = TRUE, return_original_as_attr = TRUE, ...) {
  # Assign units to input
  thedata_units <- set_units(thedata, "m/s", mode = "standard", auto_convert = FALSE)
  
  # Check unit attribute if ignoreattr is FALSE
  if (!ignoreattr) {
    current_unit <- units(thedata_units)
    if (tolower(as.character(current_unit)) != "m/s") {
      cat("From unit is", red("ms"), "but unit attribute is", red(as.character(current_unit)), "\n")
      stop("Unit mismatch.")
    }
  }
  
  # Inform the user about the conversion if quiet is FALSE
  if (!quiet) {
    cat("Converted m/s to mph\n")
  }
  
  # Perform the conversion to miles per hour
  mph_units <- set_units(thedata_units, "mph", mode = "standard")
  
  # Remove units to store as numeric, then reassign with lowercase unit
  mph_numeric <- drop_units(mph_units)
  attr(mph_numeric, "unit") <- "mph"
  
  # Optionally retain the original m/s values as an attribute
  if (return_original_as_attr) {
    attr(mph_numeric, "original") <- drop_units(thedata_units)
  }
  
  return(mph_numeric)
}

################################################################################################!
