#' mstomh
#' @export
mstomh <- function(windspeed, ignoreattr = FALSE, quiet = TRUE, return_original_as_attr = TRUE, ...) {
  # Assign units to input
  windspeed_units <- set_units(windspeed, "m/s", mode = "standard", auto_convert = FALSE)
  
  # Check unit attribute if ignoreattr is FALSE
  if (!ignoreattr) {
    current_unit <- units(windspeed_units)
    if (tolower(as.character(current_unit)) != "m/s") {
      cat("From unit is", red("ms"), "but unit attribute is", red(as.character(current_unit)), "\n")
      stop("Unit mismatch.")
    }
  }
  
  # Inform the user about the conversion if quiet is FALSE
  if (!quiet) {
    cat("Converted m/s to m/h\n")
  }
  
  # Perform the conversion to meters per hour
  mh_units <- set_units(windspeed_units, "m/h", mode = "standard")
  
  # Remove units to store as numeric, then reassign with lowercase unit
  mh_numeric <- drop_units(mh_units)
  attr(mh_numeric, "unit") <- "mh"
  
  # Optionally retain the original m/s values as an attribute
  if (return_original_as_attr) {
    attr(mh_numeric, "original") <- drop_units(windspeed_units)
  }
  
  return(mh_numeric)
}

# ff =5
# units(ff) <- "m/s"
# set_units(ff, "mh")
# mstomh(5)

################################################################################################!
