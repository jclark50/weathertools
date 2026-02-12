#' Harmonize/standardize weather fields across feeds (rename + units, in-place)
#'
#' @description
#' `wx.units()` makes a heterogeneous weather feed look consistent:
#' it (optionally) **renames** provider-specific columns to your canonical names
#' and **unit-converts** them to canonical target units — *without* relying on
#' heavy unit classes. It modifies the input [`data.table`][data.table::data.table]
#' **by reference** and also writes a human-friendly attribute
#' `attr(x[[col]], "unit")` on each harmonized column.
#'
#' The function is strict by default:
#' - It will only infer *source* units from the **original provider column name**
#'   (e.g. `TMP_2m_K` → `"K"`).  
#' - It never “guesses”. If a column’s source unit cannot be determined from the
#'   original name or an explicit override (or an existing `"unit"` attribute),
#'   it errors unless `stop_if_unknown = FALSE`.
#'
#' Extras:
#' - If `wind10m` is missing but `ugrd10m` and `vgrd10m` exist, the function
#'   derives wind speed/direction (`wind10m`, `WDIR`).
#' - If `solar` is absent but `dswrf` exists, it creates `solar := dswrf`.
#' - Optional runtime verification catches common scale mistakes (e.g. K→°C
#'   didn’t actually drop by ~273).
#'
#' @param dt A [`data.table`][data.table::data.table]. Modified in place.
#'
#' @param rename_map Optional named character vector mapping **provider names**
#'   to **canonical names** (e.g. `c("TMP_2m_K" = "ta", "DSWRF_surface_Wm^2" = "dswrf")`).
#'   If `NULL` or length-0, renaming is skipped; unit logic still applies via
#'   `src_override` / existing `attr(, "unit")`.
#'
#' @param src_override Optional named list mapping **canonical names** to
#'   *source* units, e.g. `list(ta = "K", td = "K", wind10m = "m/s")`.
#'   Precedence: `src_override` > detection from original provider names
#'   (via `rename_map`) > existing `attr(dt[[col]], "unit")`. Nothing is guessed.
#'
#' @param target_override Optional named list mapping **canonical names** to
#'   *target* units (overrides the built-in canonical table).  
#'   Example: `list(wind10m = "mph", pres = "Pa")`.
#'
#' @param stop_if_unknown Logical (default `TRUE`). If `TRUE`, error when any
#'   column’s source unit is unknown. If `FALSE`, those columns are left
#'   untouched (but will not be re-tagged).
#'
#' @param debug Logical. If `TRUE`, prints a compact conversion report
#'   (variable, from, to, sample before/after).
#'
#' @details
#' ## Canonical schema & units
#' The default canonical names and target units used by this function are:
#'
#' - Temperatures: `ta`, `ta_sfc`, `td` → **degC**
#' - Wind: `wind10m`, `ugrd10m`, `vgrd10m`, `GUST` → **m/s**; `WDIR` → **deg**
#' - Pressure: `pres`, `pres_cloudtop`, `pres_0Cisotherm`, `pres_tropopause_freezing` → **hPa**
#' - Radiation: `dswrf`, `solar` → **W/m^2**
#' - Precipitation: `prate` → **mm/h**, `apcp_sfc` → **mm**
#' - Clouds: `tcdc`, `tcdc_bl`, `lcdc`, `mcdc`, `hcdc` → **%**
#' - Visibility & ceiling: `VIS` → **km**, `ceiling` → **m**
#' - Reflectivity/lightning: `REFC` → **dB**, `LTNG`, `LTNG2` → **1/m^2/s**
#' - Satellite brightness temps: `SBT123`, `SBT124`, `SBT113`, `SBT114` → **K**
#'
#' You can override any of these targets via `target_override`.
#'
#' ## Where source units come from
#' 1. **From `rename_map` original names** (preferred when renaming):
#'    - Suffix patterns like `"_K"`, `"_ms"`, `"_Pa"`, `"_knots"`, `"_feet"`,
#'      `"_statutemile"`, `"percent"`, `"_degtrue"`, `"_Wm^2"`, `"_kgm^2s"`, `"_kgm^2"`
#'      are mapped deterministically to units.
#' 2. **From `src_override`** if you know the input unit and either:
#'    - You aren’t renaming, *or*
#'    - The provider naming is inconsistent.
#' 3. **From existing attribute** `attr(dt[[col]], "unit")` if present.
#'
#' If none of the above provide a unit for a target variable, the column is
#' considered unknown; see `stop_if_unknown`.
#'
#' ## Conversions supported (numeric only)
#' The function implements a fixed set of conversions used in this project:
#' - `K ⇄ degC`, `degC ⇄ degF`
#' - `Pa ⇄ hPa`
#' - `mph ⇄ m/s`, `kt → m/s`
#' - `feet → m`, `mile → km`
#' - `kg/m^2/s → mm/h`, `kg/m^2 → mm` (water equiv.)
#' - Identity passes (e.g., `%`, `deg`, `W/m^2`, `dB`, `1/m^2/s`, `m/s`)
#'
#' Add more as needed inside the function’s internal converter.
#'
#' ## Side effects & performance
#' - **By reference**: `dt` is modified in place; the function returns `dt`
#'   invisibly for pipe friendliness.
#' - **Attributes**: after harmonization, each target column gets
#'   `attr(, "unit")` set to the target unit for easy introspection/logging.
#' - **Vectorized**: all conversions are per-column numeric operations — no
#'   row loops and no `units` S3 overhead.
#'
#' ## Derivations & aliases
#' - If `wind10m` is missing but both `ugrd10m` and `vgrd10m` exist, the function
#'   creates `wind10m` (m/s) and `WDIR` (deg).
#' - If `solar` is absent and `dswrf` exists, `solar := dswrf` (W/m^2) is added.
#'
#' ## Error handling
#' - Unknown source units → error when `stop_if_unknown = TRUE`.
#' - Runtime verification checks (e.g., “still looks like Kelvin after K→°C”)
#'   throw informative errors to catch scale mistakes early.
#'
#' @return
#' Invisibly returns `dt` (same reference), with:
#' - Optional **renaming** applied,
#' - **Numeric** values converted to canonical target units,
#' - `attr(, "unit")` set per harmonized column,
#' - Potentially **derived** `wind10m`, `WDIR`, and **aliased** `solar`.
#'
#' @section Canonical targets (quick reference):
#' See **Details**. Override with `target_override`.
#'
#' @section Typical usage patterns:
#' ### 1) Provider → Canonical (rename + units)
#' ```r
#' library(data.table)
#' dt <- data.table(
#'   TMP_2m_K                = c(298.15, 300.15),
#'   DPT_2m_K                = c(293.15, 295.15),
#'   UGRD_10m_ms             = c( 2.0,  3.5),
#'   VGRD_10m_ms             = c(-1.0, -2.0),
#'   DSWRF_surface_Wm^2      = c(500, 750),
#'   PRES_surface_Pa         = c(101325, 100800),
#'   TCDC_percent            = c(40, 75)
#' )
#'
#' rename_map <- c(
#'   "TMP_2m_K"           = "ta",
#'   "DPT_2m_K"           = "td",
#'   "UGRD_10m_ms"        = "ugrd10m",
#'   "VGRD_10m_ms"        = "vgrd10m",
#'   "DSWRF_surface_Wm^2" = "dswrf",
#'   "PRES_surface_Pa"    = "pres",
#'   "TCDC_percent"       = "tcdc"
#' )
#'
#' wx.units(dt, rename_map, debug = TRUE)
#' # dt now has ta/td in degC, ugrd/vgrd in m/s, wind10m+WDIR derived, pres in hPa, dswrf in W/m^2,
#' # and attr(, "unit") set on each target column.
#' ```
#'
#' ### 2) Unit harmonization only (no renaming)
#' ```r
#' # Suppose your feed already uses canonical names but mixed units:
#' dt <- data.table(ta = c(297, 300), td = c(290, 293), wind10m = c(12, 8))
#' attr(dt$ta, "unit") <- "K"     # mark current units
#' attr(dt$td, "unit") <- "K"
#' attr(dt$wind10m, "unit") <- "mph"
#'
#' spec_in  <- list(ta = "K", td = "K", wind10m = "mph")
#' spec_out <- list(ta = "degC", td = "degC", wind10m = "m/s")
#'
#' wx.units(
#'   dt,
#'   rename_map      = character(),   # skip renaming
#'   src_override    = spec_in,
#'   target_override = spec_out,
#'   debug           = TRUE
#' )
#' ```
#'
#' ### 3) Overriding targets (e.g., store wind as mph)
#' ```r
#' dt <- data.table(wind10m = c(5, 8))  # m/s by provider contract
#' attr(dt$wind10m, "unit") <- "m/s"
#' wx.units(dt,
#'   rename_map      = character(),
#'   target_override = list(wind10m = "mph"),
#'   debug           = TRUE
#' )
#' ```
#'
#' ### 4) Tolerant mode for partially specified inputs
#' ```r
#' dt <- data.table(ta = c(295, 300), foo = 1:2) # ta has no unit info
#' # This will error by default; set stop_if_unknown = FALSE to skip unknowns.
#' wx.units(
#'   dt,
#'   rename_map       = character(),
#'   src_override     = list(ta = "K"),
#'   stop_if_unknown  = FALSE
#' )
#' ```
#'
#' @seealso
#' - Your package’s bias pipeline and ingestion helpers that call this function.
#' - `data.table::setattr()` for inspecting the `"unit"` tag.
#'
#' @examples
#' \dontrun{
#' # See the sections above for runnable examples.
#' }
#'
#' @export
wx.units <- function(dt,
                               rename_map      = NULL,   # old->new, or NULL/character()
                               src_override    = NULL,   # list(name="unit")
                               target_override = NULL,   # list(name="unit")
                               stop_if_unknown = FALSE,
                               debug = FALSE) {
  
  
  
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0L || (length(a)==1L && is.na(a))) b else a
  
	.convert_units <- function(x, from, to) {
	  if (is.null(from) || from == to) return(x)
	  key <- base::paste(from, "->", to)
	  switch(key,
			 "C -> F"       = x * 9/5 + 32,
			 "F -> C"       = (x - 32) * 5/9,
			 "degK -> degC"          = x - 273.15,
			 "K -> degC"          = x - 273.15,
			 "degF -> degC"       = (x - 32) * 5/9,
			 "degC -> degF"       = x * 9/5 + 32,
			 "Pa -> hPa"          = x / 100,
			 "hPa -> Pa"          = x * 100,
			 "kt -> m/s"          = x * 0.514444,
			 "mph -> m/s"         = x / 2.2369362921,
			 "m/s -> mph"         = x * 2.2369362921,
			 "m/s -> mi/h"         = x * 2.2369362921,
			 "feet -> m"          = x * 0.3048,
			 "mile -> km"         = x * 1.609344,
			 "kg/m^2/s -> mm/h"   = x * 3600,
			 "kg/m^2 -> mm"       = x,
			 "kg/m^2 -> in"       = x / 25.4,   # <-- new
			 "mm -> in"           = x / 25.4,   # optional direct
			 # identities
			 "m/s -> m/s"         = x,  "hPa -> hPa" = x, "% -> %" = x,
			 "W/m^2 -> W/m^2"     = x,  "deg -> deg" = x, "K -> K" = x,
			 "dB -> dB"           = x,  "1/m^2/s -> 1/m^2/s" = x,
			 stop(sprintf("No converter for %s", key)))
	}
  
  # detect source unit from the ORIGINAL (pre-rename) column name only
  .detect_src_unit <- function(old_nm, new_nm) {
    if (grepl("_K$", old_nm))                           return("K")
    if (grepl("_ms$", old_nm))                          return("m/s")
    if (grepl("_Pa$", old_nm))                          return("Pa")
    if (grepl("percent", old_nm, ignore.case=TRUE))     return("%")
    if (grepl("_degtrue$", old_nm, ignore.case=TRUE))   return("deg")
    if (grepl("_knots$", old_nm))                       return("kt")
    if (grepl("_statutemile$", old_nm))                 return("mile")
    if (grepl("_feet$", old_nm))                        return("feet")
    if (grepl("_Wm\\^2$", old_nm))                      return("W/m^2")
    if (grepl("_kgm\\^2s$", old_nm))                    return("kg/m^2/s")
    if (grepl("_kgm\\^2$", old_nm))                     return("kg/m^2")
    if (grepl("LTNG", old_nm))                          return("1/m^2/s")
    if (new_nm %in% "REFC")                             return("dB")
    if (new_nm %in% "WDIR")                             return("deg")
    NA_character_
  }
  
  
  .CANONICAL_UNIT <- c(
    ta="degC", ta_sfc="degC", td="degC",
    wind10m="m/s", ugrd10m="m/s", vgrd10m="m/s", GUST="m/s", WDIR="deg",
    pres="hPa", pres_cloudtop="hPa", pres_0Cisotherm="hPa", pres_tropopause_freezing="hPa",
    dswrf="W/m^2", solar="W/m^2", prate="mm/h", apcp_sfc="mm",
    tcdc="%", tcdc_bl="%", lcdc="%", mcdc="%", hcdc="%",
    VIS="km", ceiling="m",
    REFC="dB", LTNG="1/m^2/s", LTNG2="1/m^2/s",
    SBT123="K", SBT124="K", SBT113="K", SBT114="K"
  )
  
  .uv_to_spd_dir <- function(u, v) {
    spd <- sqrt(u*u + v*v)
    dir <- (atan2(-u, -v) * 180/pi) %% 360
    list(spd=spd, dir=dir)
  }
  
  
  
  
  stopifnot(data.table::is.data.table(dt))
  
  # harmless suffix strip present in some feeds
  data.table::setnames(dt, names(dt), gsub("_NTAT_K$", "", names(dt)), skip_absent=TRUE)
  
  # --- renaming (optional) ---
  rename_map <- rename_map %||% character()
  src_by_new <- list()
  
  if (length(rename_map)) {
    present_old <- intersect(names(dt), names(rename_map))
    present_new <- unname(rename_map[present_old])
    
    if (debug) cat("Found", length(present_old), "mapped columns to rename\n")
    
    detected <- vapply(seq_along(present_old),
                       function(i) .detect_src_unit(present_old[i], present_new[i]),
                       character(1))
    src_by_new <- as.list(detected); names(src_by_new) <- present_new
    
    if (length(present_old)) {
      data.table::setnames(dt, old = present_old, new = present_new, skip_absent=TRUE)
    }
  } else {
    if (debug) cat("Found 0 mapped columns to rename (skipping rename)\n")
  }
  
  # explicit source overrides win
  if (!is.null(src_override)) for (nm in names(src_override)) src_by_new[[nm]] <- src_override[[nm]]
  
  # derive wind if absent
  if (!"wind10m" %in% names(dt) && all(c("ugrd10m","vgrd10m") %in% names(dt))) {
    uv <- .uv_to_spd_dir(dt$ugrd10m, dt$vgrd10m)
    dt[, `:=`(wind10m = uv$spd, WDIR = uv$dir)]
    data.table::setattr(dt[["wind10m"]], "unit", "m/s")
    data.table::setattr(dt[["WDIR"]],    "unit", "deg")
    src_by_new[["wind10m"]] <- src_by_new[["wind10m"]] %||% "m/s"
    src_by_new[["WDIR"]]    <- src_by_new[["WDIR"]]    %||% "deg"
  }
  
  # targets
  targets <- .CANONICAL_UNIT
  if (!is.null(target_override)) for (nm in names(target_override)) targets[[nm]] <- target_override[[nm]]
  targets <- targets[intersect(names(targets), names(dt))]
  
  # resolve sources (override > detected > column attr)
  # resolve_from <- function(nm) src_by_new[[nm]] %||% attr(dt[[nm]], "unit")
  resolve_from <- function(nm) {
    val <- src_by_new[[nm]] %||% attr(dt[[nm]], "unit")
    if (is.null(val) || length(val)==0L) NA_character_ else val
  }
  from_vec <- vapply(names(targets), resolve_from, character(1))
  
  unknown <- names(targets)[is.na(from_vec) | from_vec == ""]
  if (length(unknown)) {
    msg <- paste0("Unknown source unit for: ", paste(unknown, collapse=", "),
                  ". Provide src_override=list(name='unit') or add an attribute.")
    if (stop_if_unknown) stop(msg) else if (debug) message(msg)
  }
  
  # convert & tag (with verification + debug samples)
  if (debug) conv_report <- data.table::data.table(var=character(), from=character(), to=character(),
                                                   before=character(), after=character())
  for (nm in names(targets)) {
    to   <- targets[[nm]]
    from <- resolve_from(nm)
    if (is.na(from) || from == "") next
    
    x <- dt[[nm]]
    if (!identical(from, to)) {
      if (!is.double(x)) storage.mode(x) <- "double"
      b_samp <- paste(utils::head(x, 3), collapse=", ")
      y <- .convert_units(x, from, to)
      a_samp <- paste(utils::head(y, 3), collapse=", ")
      
      # assign back & tag
      dt[[nm]] <- y
      data.table::setattr(dt[[nm]], "unit", to)
      
      # verify critical conversions actually changed scale
      if (from=="K" && to=="degC") {
        if (stats::median(y, na.rm=TRUE) > 150) {
          stop("Verification failed: '", nm, "' looks like it is still in Kelvin after K->degC.")
        }
      }
      if (from=="Pa" && to=="hPa") {
        if (stats::median(y, na.rm=TRUE) > 2000) {
          stop("Verification failed: '", nm, "' looks like it is still in Pa after Pa->hPa.")
        }
      }
      
      if (debug) conv_report <- rbind(conv_report,
                                      data.table::data.table(var=nm, from=from, to=to,
                                                             before=b_samp, after=a_samp),
                                      fill=TRUE)
    } else {
      # still tag the attr
      data.table::setattr(dt[[nm]], "unit", to)
      if (debug) conv_report <- rbind(conv_report,
                                      data.table::data.table(var=nm, from=from, to=to,
                                                             before=paste(utils::head(x,3), collapse=", "),
                                                             after ="no-op"),
                                      fill=TRUE)
    }
  }
  
  # alias dswrf -> solar
  if (!"solar" %in% names(dt) && "dswrf" %in% names(dt)) {
    dt[, solar := dswrf]
    data.table::setattr(dt[["solar"]], "unit", "W/m^2")
    if (debug) conv_report <- rbind(conv_report,
                                    data.table::data.table(var="solar", from="dswrf", to="W/m^2 (alias)",
                                                           before="", after=""),
                                    fill=TRUE)
  }
  
  if (debug && exists("conv_report") && nrow(conv_report)) print(conv_report)
  invisible(dt)
}




