#' Lightweight time zone getter/setter for POSIXt
#'
#' @description
#' Convenience helpers to read or set the \code{"tzone"} attribute on POSIX date-times.
#' Use \code{"Area/Location"} to relabel (same instant), or append \code{"|force"}
#' to re-interpret clock times as local in the new zone (epoch seconds change).
#'
#' @details
#' \itemize{
#'   \item \code{tzone(x)} returns the current time zone tag (character) or \code{""}.
#'   \item \code{tzone(x) <- value} sets the time zone. By default it relabels only.
#'         If \code{value} indicates \code{"force"}, the same printed wall time is re-parsed
#'         in the new zone (epoch seconds change).
#' }
#'
#' Accepted \code{value} forms:
#' \itemize{
#'   \item \code{"America/New_York"} — relabel only
#'   \item \code{"America/New_York|force"} — force reinterpretation
#'   \item \code{"America/New_York; mode=force"} — same as above
#' }
#'
#' @name tzone
#'
#' @param x A \code{POSIXct} or \code{POSIXlt} vector.
#' @param value Character scalar time zone in IANA/Olson form (see \code{OlsonNames()}),
#'   optionally annotated with \code{"|force"} or \code{"; mode=force"}.
#'
#' @return
#' \code{tzone(x)} returns the current tag (character) or \code{""}.
#' The replacement form returns the modified vector.
#'
#' @examples
#' x <- as.POSIXct("2024-07-01 12:00:00", tz = "UTC")
#' tzone(x)
#'
#' # Relabel only (same instant)
#' tzone(x) <- "America/New_York"
#' format(x, tz = tzone(x))
#'
#' # Force reinterpretation (epoch seconds change)
#' y <- as.POSIXct("2024-07-01 12:00:00", tz = "UTC")
#' tzone(y) <- "America/New_York|force"
#' y
#'
#' @export
tzone <- function(x) {
  stopifnot(inherits(x, "POSIXt"))
  tz <- attr(x, "tzone", exact = TRUE)
  if (is.null(tz)) "" else tz
}

# --- internals ---
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
.trim <- function(s) sub("^\\s+|\\s+$", "", s)

.parse_tzone_value <- function(value) {
  stopifnot(is.character(value), length(value) == 1L, !is.na(value))
  val <- .trim(value)

  parts  <- strsplit(val, "\\s*;\\s*", perl = TRUE)[[1]]
  head   <- parts[1]
  params <- tolower(paste(parts[-1], collapse = " "))

  mode <- "relabel"
  if (grepl("|", head, fixed = TRUE)) {
    bits <- strsplit(head, "|", fixed = TRUE)[[1]]
    head <- .trim(bits[1])
    tail <- tolower(.trim(bits[2] %||% ""))
    if (tail %in% c("force", "relabel")) mode <- tail
  }
  if (nzchar(params) && grepl("mode\\s*=\\s*force", params)) mode <- "force"

  tz <- head

  if (nzchar(tz)) {
    if (!tz %in% OlsonNames()) {
      stop(sprintf("tzone<-: unknown IANA time zone '%s'. Try something from OlsonNames().", tz))
    }
  }
  list(tz = tz, mode = mode)
}

.force_tzone <- function(x, tz_to) {
  is_lt  <- inherits(x, "POSIXlt")
  x_ct   <- if (is_lt) as.POSIXct(x, tz = attr(x, "tzone", exact = TRUE) %||% "") else x
  tz_old <- attr(x_ct, "tzone", exact = TRUE) %||% ""

  wall <- ifelse(
    is.na(x_ct),
    NA_character_,
    format(x_ct, "%Y-%m-%d %H:%M:%S", tz = tz_old, usetz = FALSE)
  )

  y <- as.POSIXct(wall, tz = tz_to)
  if (is_lt) y <- as.POSIXlt(y, tz = tz_to)
  y
}

#' @rdname tzone
#' @usage tzone(x) <- value
#' @export
`tzone<-` <- function(x, value) {
  stopifnot(inherits(x, "POSIXt"))
  pv <- .parse_tzone_value(value)

  if (identical(pv$mode, "relabel")) {
    attr(x, "tzone") <- pv$tz
    return(x)
  }

  .force_tzone(x, pv$tz)
}
