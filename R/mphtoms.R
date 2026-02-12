#' mphtoms
#' @export
mphtoms <- function(thedata, ignoreattr = FALSE, quiet = TRUE, return_original_as_attr = TRUE, ...) {
  # Assign units to input
  thedata_units <- set_units(thedata, "mph", mode = "standard", auto_convert = FALSE)
  
  # Check unit attribute if ignoreattr is FALSE
  if (!ignoreattr) {
    current_unit <- units(thedata_units)
    if (tolower(as.character(current_unit)) != "mph") {
      cat("From unit is", red("mph"), "but unit attribute is", red(as.character(current_unit)), "\n")
      stop("Unit mismatch.")
    }
  }
  
  # Inform the user about the conversion if quiet is FALSE
  if (!quiet) {
    cat("Converted mph to m/s\n")
  }
  
  # Perform the conversion to meters per second
  ms_units <- set_units(thedata_units, "m/s", mode = "standard")
  
  # Remove units to store as numeric, then reassign with lowercase unit
  ms_numeric <- drop_units(ms_units)
  attr(ms_numeric, "unit") <- "ms"
  
  # Optionally retain the original mph values as an attribute
  if (return_original_as_attr) {
    attr(ms_numeric, "original") <- drop_units(thedata_units)
  }
  
  return(ms_numeric)
}

################################################################################################!
