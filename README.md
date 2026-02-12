# weathertools

**weathertools** is a small, fast, dependency-light R package for common weather / atmosphere calculations—focused on **practical use in data workflows** (especially `data.table`) rather than on meteorology jargon or heavyweight unit systems. 

It includes utilities for:

* **Heat & humidity metrics:** heat index, wet-bulb temperature, dew point, relative humidity 
* **Pressure conversions:** station ↔ sea-level style adjustments 
* **Wind helpers:** wind chill, u/v → wind speed/direction, rolling-average wind direction 
* **Lightweight unit tagging & conversion:** no `{units}` classes required 
* **Schema/unit harmonization for messy feeds:** rename + convert in-place for a consistent dataset 

---

## Who this is for

This package is designed for people who:

* work with weather-ish data (stations, forecasts, “environmental” sensors, CSVs from vendors),
* **don’t** want to memorize meteorology,
* want **clear, predictable behavior** in scripts and pipelines,
* want **fast vectorized** functions (some using compiled C++ for speed). 

If you *are* a meteorologist: nothing here tries to be a complete atmospheric science library—this is intentionally a “core toolbox.”

---

## Installation

### From GitHub (recommended during development)

```r
install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/weathertools")
```

### From source (local)

```r
install.packages("path/to/weathertools", repos = NULL, type = "source")
```

---

## Quick start (copy/paste)

```r
library(weathertools)

# 1) Heat Index (needs air temperature + relative humidity)
ta <- c(30, 35, 40)
attr(ta, "unit") <- "degC"
rh <- c(50, 60, 65)

hi_c <- calcHI(ta, rh, outputunits = "degC")
hi_c
attr(hi_c, "unit")

# 2) Dew point (temperature + RH)
dp <- calcTD(ta, rh, outputunits = "degC")
dp
attr(dp, "unit")

# 3) Wet-bulb temperature (temperature + RH)
wb <- calcWB(ta, rh, inputunits = "degC", outputunits = "degC", ignoreattr = TRUE)
wb
attr(wb, "unit")
```

> If you’re new to these terms: the next section gives plain-English definitions and when you’d use each metric.

---

## Concepts (plain English)

### Temperature

Just the air temperature—usually **°C** or **°F**.

### Relative Humidity (RH)

A percentage (0–100) describing how “full” the air is with water vapor compared to the maximum possible at that temperature. RH alone can be misleading because “fullness” depends on temperature.

### Dew point

The temperature the air would need to cool to (at constant pressure) for condensation to begin. Dew point is often easier to interpret than RH:

* **Higher dew point = more moisture in the air**, generally.
* People often find dew point aligns better with “mugginess.”

`calcTD()` computes dew point from temperature + RH using a fast compiled core. 

### Heat Index

A “feels-like” hot metric for shaded, light-wind conditions—primarily used in public heat guidance. It depends on temperature + RH.

`calcHI()` computes Heat Index quickly (compiled core) and returns in °F or °C. 

### Wet-bulb temperature

A thermodynamic measure related to evaporative cooling potential. It’s often more physically grounded than heat index for certain heat-stress contexts.

`calcWB()` computes wet-bulb temperature from temperature + RH and supports RH given as percent or as a fraction (e.g., 0.60). 

### Pressure: station vs sea-level

Weather stations at elevation measure a lower pressure than sea level. Some datasets provide “sea-level pressure” (adjusted) for comparing across elevations.

* `calcPres()` estimates sea-level pressure from station pressure + temperature + elevation. 
* `stationpressure()` estimates station pressure from sea-level pressure + elevation. 

### Wind direction averaging

Averaging wind direction is tricky (mean of 350° and 10° is **0°**, not 180°). `avgwdir()` does it properly via vector averaging and can weight by wind speed. 

---

## Unit handling philosophy (important)

This package uses a deliberately simple “units” model:

* Most functions accept **plain numeric vectors**
* You may optionally tag vectors with `attr(x, "unit")`
* Several functions can be **strict** about unit attributes (to prevent silent mistakes)
* You can override strictness with `ignoreattr = TRUE` in many functions 

### Why this approach?

Weather data comes from everywhere (APIs, CSVs, GRIB-derived tables, vendors). Heavy unit systems are powerful, but often add friction in pipelines and `data.table` workflows. This package aims to be:

* lightweight,
* explicit,
* hard to misuse accidentally.

### `unit()` helper

`unit()` is a convenience getter/setter for the `"unit"` attribute and can optionally convert between supported pairs using the package’s internal converter. 

```r
x <- c(0, 10, 20)
unit(x) <- "degC"     # tag
unit(x)              # "degC"
unit(x) <- "degF"     # convert based on existing tag
unit(x)
```

### `convert_units()` helper

For direct unit conversions without attribute logic:

```r
convert_units(c(0, 20, 30), "degC", "degF")
convert_units(c(10, 25), "mph", "m/s")
convert_units(c(101325, 100800), "Pa", "hPa")
```

Supported conversions include common temperature, wind speed, pressure, distance, and water-equivalent conversions. 

---

## Common recipes

### 1) I have temperature and dew point, and I want RH (%)

```r
ta <- c(30, 31); dp <- c(20, 21)
attr(ta, "unit") <- "degC"
attr(dp, "unit") <- "degC"

rh <- calcRH(ta, dewPoint = dp, inputunits = "degC")
rh
```

`calcRH()` can also use vapor pressure deficit (VPD) instead of dew point if that’s what you have. 

---

### 2) I have u and v wind components, and I want speed + direction

```r
uv2wdws(u = c(1, 0, -1), v = c(0, 1, 0))
```

Returns a 2-column matrix: wind direction (degrees) and wind speed. 

---

### 3) I have wind direction degrees and want compass labels

```r
winddeg(c(0, 20, 45, 90, 200, 359.9))
```

Returns labels like `N`, `NNE`, `NE`, etc. 

---

### 4) Proper rolling-average wind direction

```r
wd  <- c(350, 10, 15, 20, 25)
wsp <- c(5,   5,  5,  5,  5)

avgwdir(wd, wsp, movingWindow = 3)
```

Uses vector averaging so wrap-around is handled correctly. 

---

### 5) Sea-level pressure from station pressure

```r
calcPres(pressureMB = 1000, airTemp = 20, elevation = 100)  # temp in degC by default
```

Temperature and elevation units are configurable. 

---

## Working with `data.table` (recommended)

Most functions are vectorized and work well inside `DT[, newcol := ...]`.

```r
library(data.table)
library(weathertools)

DT <- data.table(
  ta = c(30, 35),
  rh = c(50, 60)
)

attr(DT$ta, "unit") <- "degC"

DT[, hi := calcHI(ta, rh, outputunits = "degC")]
DT[, dp := calcTD(ta, rh, outputunits = "degC")]

DT
```

---

## Harmonizing messy weather feeds with `wx.units()`

Real-world feeds often have:

* different column names (`TMP_2m_K` vs `tempC` vs `T2M`)
* different units (Kelvin vs Celsius, Pa vs hPa, mph vs m/s)
* incomplete fields (u/v present, but speed/direction missing)

`wx.units()` is designed to standardize a dataset into a **canonical schema** by:

1. optionally renaming provider columns to canonical names
2. converting numeric values to canonical units
3. tagging harmonized columns with `attr(col, "unit")`
4. deriving missing wind speed/direction from u/v when available 

### Example: rename + convert + derive wind speed/direction

```r
library(data.table)
library(weathertools)

dt <- data.table(
  TMP_2m_K            = c(298.15, 300.15),
  DPT_2m_K            = c(293.15, 295.15),
  UGRD_10m_ms         = c( 2.0, 3.5),
  VGRD_10m_ms         = c(-1.0, -2.0),
  DSWRF_surface_Wm^2  = c(500, 750),
  PRES_surface_Pa     = c(101325, 100800),
  TCDC_percent        = c(40, 75)
)

rename_map <- c(
  "TMP_2m_K"           = "ta",
  "DPT_2m_K"           = "td",
  "UGRD_10m_ms"        = "ugrd10m",
  "VGRD_10m_ms"        = "vgrd10m",
  "DSWRF_surface_Wm^2" = "dswrf",
  "PRES_surface_Pa"    = "pres",
  "TCDC_percent"       = "tcdc"
)

wx.units(dt, rename_map, debug = TRUE)

# After:
# - ta/td are in degC
# - pres is in hPa
# - dswrf is in W/m^2
# - wind10m + WDIR are derived from ugrd10m/vgrd10m
# - attr(, "unit") is set on harmonized columns
```

This function modifies the `data.table` **by reference** for speed. 

### When to use `src_override` and `target_override`

* Use `src_override` when the provider naming doesn’t encode units reliably.
* Use `target_override` when you want canonical outputs in your preferred units (e.g., store wind in mph).

`wx.units()` is strict about not guessing units; it can error on unknowns unless you choose tolerant mode. 

---

## Function map (what to reach for)

### Heat, humidity, comfort/stress

* `calcHI()` — Heat Index (temp + RH) 
* `calcWB()` — Wet-bulb temperature (temp + RH) 
* `calcTD()` — Dew point (temp + RH) 
* `calcRH()` — RH from temp + dew point **or** temp + VPD 
* `calcWindchill()` — wind chill (°F + mph) 

### Pressure

* `calcPres()` — station → sea-level (approx.) 
* `stationpressure()` — sea-level → station (approx.) 
* `intomb()` — inHg → mb/hPa 

### Wind

* `uv2wdws()` — u/v → wind direction + speed 
* `avgwdir()` — rolling average wind direction (correct wrap-around) 
* `winddeg()` — degrees → 16-point compass label 
* `windRun()` — wind run helper 

### Units + data hygiene

* `convert_units()` — direct conversion between supported pairs 
* `unit()` — lightweight unit tagger/converter with safety policies 
* `unitConvertRound()` — convert + round in one step 
* `wx.units()` — rename + unit-harmonize in-place for weather feeds 

### Time zone

* `tzone()` — get/set POSIX time zone label, including “force reinterpretation” mode 

---

## Common pitfalls (and how to avoid them)

### 1) “My results are nonsense” → it’s usually units

Examples:

* Kelvin treated as Celsius (off by ~273)
* Pa treated as hPa (off by 100×)
* m/s treated as mph (off by ~2.237×)

Best practice:

* Tag your columns once with `attr(x, "unit") <- "..."`, then use strict mode defaults.
* If you must override, do it explicitly with `ignoreattr = TRUE` or `unit(x) <- "src -> dst"`.

`wx.units()` also includes optional runtime checks to catch common scale mistakes early. 

### 2) “Why did `wx.units()` change my data without returning anything?”

Because it modifies a `data.table` by reference for speed (standard `data.table` pattern). It returns the same object invisibly for pipe-friendliness. 

### 3) RH provided as 0–1 instead of 0–100

`calcWB()` will auto-scale RH fractions (e.g., 0.60 → 60) when values are small. 

---

## Performance notes

* Many functions are vectorized.
* Several “core” calculations use compiled C++ for speed (especially useful for long time series or grid cells). 
* `wx.units()` is column-wise and in-place, avoiding row loops. 

---

## Contributing

Issues and PRs are welcome—especially for:

* additional unit conversions that are genuinely common in applied work,
* additional canonical mappings in `wx.units()` for real-world provider names,
* new “small but high-value” weather calculations.

If you’re adding functionality, please include:

* clear docs (what it does, what units are expected),
* at least one example,
* tests if practical.

---

## License

MIT (see `LICENSE`). 

---

## Reference: package contents

The package documentation PDF lists the full exported function set and their arguments, including unit-handling rules, examples, and `wx.units()` canonical schema. 
