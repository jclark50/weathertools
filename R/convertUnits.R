#' convertUnits
#' @export
convertUnits = function(what, from, to, ignoreattr=FALSE, quiet=FALSE, return_original_as_attr = TRUE){
  if (isFALSE(ignoreattr)){
    if (attr(what, "unit") != from){
      cat("From unit is", crayon::red(from), "but unit attribute is", crayon::red(attr(what, "unit")), "\n")
      stop()
    }
  }
  whatog = what
  if (from == "F" & to == "C"){
    what = ( what - 32)/1.8
    attr(what, "unit")<- to
  } else if (from == "C" & to == "F"){
    what = ( what * 1.8 + 32)
    attr(what, "unit")<- to
  } else if (from == "f" & to == "c"){
    what = ( what - 32)/1.8
    attr(what, "unit")<- to
  } else if (from == "c" & to == "f"){
    what = ( what * 1.8 + 32)
    attr(what, "unit")<- to
  } else if (from == "K" & to == "F"){
    what = (what  - 273.15) * 9/5 + 32
    attr(what, "unit")<- to
  } else if (from == "K" & to == "C"){
    what = (what  - 273.15)
    attr(what, "unit")<- to
  } else if (from == "ms" & to == "mph"){
    what = ( what/0.44704)
    attr(what, "unit")<- to
  } else if (from == "ms" & to == "knots"){
    what = ( what*1.944)
    attr(what, "unit")<- to
  } else if (from == "ms" & to == "mh"){
    what = ( what*3600)
    attr(what, "unit")<- to
  } else if (from == "mph" & to == "ms"){
    what = ( what/2.237)
    attr(what, "unit")<- to
  } else if (from == "mph" & to =="knots"){
    what = ( what*0.868976)
    attr(what, "unit")<- to
  } else if (from == "ms" & to == "mh"){
    what = ( what*3600)
    attr(what, "unit")<- to
  } else if (from == "in" & to == "mb"){
    what = ( what*33.8637526)
    attr(what, "unit")<- to
  } else if (from == "mb" & to =="in"){
    what = ( what*0.0295301)
    attr(what, "unit")<- to
  } else if (from == "mm" & to =="in"){
    what = ( what*0.0393701)
    attr(what, "unit")<- to
  } else if (from == "in" & to =="mm"){
    what = ( what*25.4)
    attr(what, "unit")<- to
  } else if (from == "m" & to =="ft"){
    what = ( what*3.28084)
    attr(what, "unit")<- to
  } else if (from == "ft" & to =="m"){
    what = ( what*0.3048)
    attr(what, "unit")<- to
  } else if (from == "knots" & to =="mph"){
    what = ( what*1.15078)
    attr(what, "unit")<- to
  } else if (from == "knots" & to == "ms"){
    what = ( what/1.944)
    attr(what, "unit")<- to
  } else if (from == "Pa" & to == "mb"){
    what = (what/100)
    attr(what, "unit")<- to
  } else {
    cat("Unit conversion not available in function", "\n")
  }
  if (isFALSE(quiet)){
    cat("Converted", from, "to", to, "\n")
  }
  
  if (isFALSE(ignoreattr)){
    attr(whatog, "unit") <- attr(what, "unit") 
  }
  if (return_original_as_attr){
    return(structure(what, original = whatog, unit = to))
  } else {
    return(structure(what, unit = to))
  }
  return(what)
}


################################################################################################!
