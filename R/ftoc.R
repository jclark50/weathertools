#' ftoc
#' @export
ftoc <- function(f, ignoreattr = FALSE, quiet = TRUE, return_original_as_attr = TRUE, ...) {
  # Assign units to input
  f_units <- set_units(f, "degF", mode = "standard", auto_convert = FALSE)
  
  # Check unit attribute if ignoreattr is FALSE
  if (!ignoreattr) {
    current_unit <- units(f_units)
    if (tolower(as.character(current_unit)) != "degf") {
      cat("From unit is", red("f"), "but unit attribute is", red(as.character(current_unit)), "\n")
      stop("Unit mismatch.")
    }
  }
  
  # Inform user if quiet is FALSE
  if (!quiet) {
    cat("Converted F to C\n")
  }
  
  # Perform conversion to Celsius
  c_units <- set_units(f_units, "degC", mode = "standard")
  
  # Extract numeric values
  c_numeric <- drop_units(c_units)
  
  # Assign lowercase unit
  attr(c_numeric, "unit") <- "c"
  
  # Optionally retain original Fahrenheit values
  if (return_original_as_attr) {
    attr(c_numeric, "original") <- drop_units(f_units)
  }
  
  return(c_numeric)
}


################################################################################################!
