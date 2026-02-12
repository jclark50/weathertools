#' Lightweight time zone getter/setter for POSIXt
#'
#' @description
#' Convenience helpers to read or set the \code{"tzone"} attribute on POSIX date-times.
#' Use \code{"Area/Location"} to relabel (same instant), or append \code{"|force"}
#' to re-interpret clock times as local in the new zone (epoch seconds change).
#'
#' @param x A \code{POSIXct} or \code{POSIXlt} vector.
#' @param value Character scalar time zone. Accepted forms:
#'   \itemize{
#'     \item \code{"America/New_York"} — relabel only
#'     \item \code{"America/New_York|force"} — force reinterpretation
#'     \item \code{"America/New_York; mode=force"} — same as above
#'   }
#' @return \code{tzone(x)} returns the current tag (character) or \code{""}.
#'         The replacement form returns the modified vector.
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

  # Split off optional "; ..." params
  parts <- strsplit(val, "\\s*;\\s*", perl = TRUE)[[1]]
  head <- parts[1]
  params <- tolower(paste(parts[-1], collapse = " "))

  # Allow "|force" / "|relabel"
  mode <- "relabel"
  if (grepl("\\|", head, fixed = TRUE)) {
    bits <- strsplit(head, "\\|", fixed = TRUE)[[1]]
    head <- .trim(bits[1])
    tail <- tolower(.trim(bits[2] %||% ""))
    if (tail %in% c("force", "relabel")) mode <- tail
  }
  if (nzchar(params) && grepl("mode\\s*=\\s*force", params)) mode <- "force"

  tz <- head

  # Validate tz (accept empty = system default)
  if (nzchar(tz)) {
    # On Windows/macOS/Linux, OlsonNames() is authoritative
    if (!tz %in% OlsonNames()) {
      stop(sprintf("tzone<-: unknown IANA time zone '%s'. Try something from OlsonNames().", tz))
    }
  }
  list(tz = tz, mode = mode)
}

# Re-interpret current wall times as if they are in tz_to (epoch seconds change)
.force_tzone <- function(x, tz_to) {
  # Coerce to POSIXct to operate
  is_lt  <- inherits(x, "POSIXlt")
  x_ct   <- if (is_lt) as.POSIXct(x, tz = attr(x, "tzone", exact = TRUE) %||% "") else x
  tz_old <- attr(x_ct, "tzone", exact = TRUE) %||% ""

  # Extract wall-clock strings in the old tag (or system default if "")
  wall <- ifelse(
    is.na(x_ct),
    NA_character_,
    format(x_ct, "%Y-%m-%d %H:%M:%S", tz = tz_old, usetz = FALSE)
  )

  # Parse as POSIXct in the new tz (this changes epoch seconds)
  y <- as.POSIXct(wall, tz = tz_to)

  if (is_lt) y <- as.POSIXlt(y, tz = tz_to)
  y
}

#' @export
#' @usage tzone(x) <- value
`tzone<-` <- function(x, value) {
  stopifnot(inherits(x, "POSIXt"))
  pv <- .parse_tzone_value(value)

  if (identical(pv$mode, "relabel")) {
    # Change only the tag; same instant
    attr(x, "tzone") <- pv$tz
    return(x)
  }

  # Force reinterpretation: treat the same printed clock time as local in pv$tz
  .force_tzone(x, pv$tz)
}
