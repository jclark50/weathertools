#' ctof
#' @export
ctof <- function(c, ignoreattr = FALSE, quiet = TRUE, return_original_as_attr = TRUE, ...) {
  # Assign units to input
  c_units <- set_units(c, "degC", mode = "standard", auto_convert = FALSE)
  
  # Check unit attribute if ignoreattr is FALSE
  if (!ignoreattr) {
    current_unit <- units(c_units)
    if (tolower(as.character(current_unit)) != "degc") {
      cat("From unit is", red("c"), "but unit attribute is", red(as.character(current_unit)), "\n")
      stop("Unit mismatch.")
    }
  }
  
  # Inform user if quiet is FALSE
  if (!quiet) {
    cat("Converted C to F\n")
  }
  
  # Perform conversion to Fahrenheit
  f_units <- set_units(c_units, "degF", mode = "standard")
  
  # Extract numeric values
  f_numeric <- drop_units(f_units)
  
  # Assign lowercase unit
  attr(f_numeric, "unit") <- "f"
  
  # Optionally retain original Celsius values
  if (return_original_as_attr) {
    attr(f_numeric, "original") <- drop_units(c_units)
  }
  
  return(f_numeric)
}

################################################################################################!
