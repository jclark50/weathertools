#' Convert numeric vectors between a fixed set of common meteorological units
#'
#' @description
#' `convert_units()` performs fast, vectorized unit conversion for a **fixed**
#' set of unit pairs used in this project. It is intentionally lightweight and
#' does **not** depend on the \pkg{units} package or any unit classes.
#'
#' Units are normalized via the internal helper \code{.norm_u()} (e.g.,
#' \code{"C"}, \code{"degC"}, \code{"celsius"} \eqn{\rightarrow} \code{"degC"}).
#' If \code{from} or \code{to} are \code{NULL}, unknown, or identical after
#' normalization, the input is returned unchanged.
#'
#' @param x Numeric vector. Values to convert. Coercion is not performed; callers
#'   should ensure \code{x} is numeric.
#' @param from Character scalar. Source unit token (e.g. \code{"K"}, \code{"degC"},
#'   \code{"mph"}, \code{"Pa"}). Normalized internally by \code{.norm_u()}.
#' @param to Character scalar. Target unit token (e.g. \code{"degC"}, \code{"m/s"},
#'   \code{"hPa"}). Normalized internally by \code{.norm_u()}.
#'
#' @return A numeric vector of the same length as \code{x}, converted to \code{to}.
#'   If no conversion is performed, returns \code{x} unchanged.
#'
#' @details
#' Supported conversions include (not exhaustive):
#' \itemize{
#'   \item Temperature: \code{degC} to/from \code{degF}; \code{degC} to/from \code{K}; \code{degF} to/from \code{K}
#'   \item Pressure: \code{Pa} to/from \code{hPa}
#'   \item Wind speed: \code{mph} to/from \code{m/s}; \code{kt} to/from \code{m/s}
#'   \item Distance: \code{feet} to/from \code{m}; \code{mile} to/from \code{km}
#'   \item Water-equivalent flux/accumulation: \code{kg/m^2/s} to/from \code{mm/h};
#'         \code{kg/m^2} to/from \code{mm}; plus \code{mm} to/from \code{in} and
#'         \code{kg/m^2} to/from \code{in} (using 1 mm water = 1 kg/m^2)
#'   \item Identity passes for common tokens (e.g. \code{"\%"} and \code{"W/m^2"})
#' }
#'
#' If a requested conversion is not implemented, the function errors with a message
#' like \code{"No converter for degC -> degF"}.
#'
#' @examples
#' # Temperature
#' convert_units(c(0, 20, 30), "degC", "degF")
#' convert_units(c(32, 68), "degF", "degC")
#' convert_units(c(300, 310), "K", "degC")
#'
#' # Wind speed
#' convert_units(c(10, 25), "mph", "m/s")
#' convert_units(c(5, 10), "m/s", "mph")
#' convert_units(c(10, 20), "kt", "m/s")
#'
#' # Pressure
#' convert_units(c(101325, 100800), "Pa", "hPa")
#'
#' # Precip/water equivalents
#' convert_units(c(1.5, 3), "mm", "in")
#' convert_units(c(2e-4, 5e-4), "kg/m^2/s", "mm/h")
#'
#' @export
convert_units <- function (x, from, to)
{
  if (is.null(from) || is.null(to))
    return(x)
  from <- .norm_u(from)
  to <- .norm_u(to)
  if (is.na(from) || is.na(to) || from == to)
    return(x)
  key <- base::paste(from, "->", to)
  switch(key, `degC -> degF` = x * 9/5 + 32, `degF -> degC` = (x -
                                                                 32) * 5/9, `degC -> K` = x + 273.15, `K -> degC` = x -
           273.15, `degF -> K` = (x - 32) * 5/9 + 273.15, `K -> degF` = (x -
                                                                           273.15) * 9/5 + 32, `Pa -> hPa` = x/100, `hPa -> Pa` = x *
           100, `kt -> m/s` = x * 0.514444, `m/s -> kt` = x/0.514444,
         `mph -> m/s` = x/2.2369362921, `m/s -> mph` = x * 2.2369362921,
         `feet -> m` = x * 0.3048, `m -> feet` = x/0.3048, `mile -> km` = x *
           1.609344, `km -> mile` = x/1.609344, `kg/m^2/s -> mm/h` = x *
           3600, `mm/h -> kg/m^2/s` = x/3600, `kg/m^2 -> mm` = x,
         `mm -> kg/m^2` = x, `mm -> in` = x/25.4, `in -> mm` = x *
           25.4, `kg/m^2 -> in` = x/25.4, `in -> kg/m^2` = x *
           25.4, `m/s -> m/s` = x, `mph -> mph` = x, `kt -> kt` = x,
         `hPa -> hPa` = x, `Pa -> Pa` = x, `% -> %` = x, `deg -> deg` = x,
         `W/m^2 -> W/m^2` = x, `K -> K` = x, `degC -> degC` = x,
         `degF -> degF` = x, `mm -> mm` = x, `mm/h -> mm/h` = x,
         `km -> km` = x, `m -> m` = x, `feet -> feet` = x, `mile -> mile` = x,
         `in -> in` = x, `1/m^2/s -> 1/m^2/s` = x, stop(sprintf("No converter for %s",
                                                                key)))
}
