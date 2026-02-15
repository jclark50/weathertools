# weathertools

> **Package manual (CRAN-style PDF):**  
> For a complete function reference in the standard R “package manual” format, see PDF under /docs.
> Also accessible at: https://jclark50.github.io/weathertools/

**weathertools** is an R package for turning raw “weather-like” columns (temperature, humidity, wind, pressure, time) into **clean, consistent, analysis-ready variables**—without needing a climate or meteorology background.

It is built for people working with:
- weather station exports
- sensor / IoT logs
- weather APIs
- forecast-model-derived tables
- environmental datasets that “include weather columns”

…and who want results that are **fast, reproducible, and hard to mess up by accident**.

---

## What this package helps with (in human terms)

### ✅ 1) “How hot does it feel?” and “How humid is it really?”
Temperature alone is not enough. 30°C can feel very different depending on humidity and wind.

**weathertools** computes widely used derived metrics like:
- **Heat Index** (a “feels-like hot” metric used in public heat guidance)
- **Dew Point** (often easier to interpret than relative humidity for how “muggy” it feels)
- **Wet-bulb temperature** (used in many heat-stress contexts)
- **Wind chill** (a “feels-like cold” metric)

You give it the columns you already have (e.g., temperature + relative humidity), and it returns the derived metric in the unit you want.

---

### ✅ 2) Avoid the “direction wraparound” mistake (wind directions don’t average normally)
Some weather values behave differently than typical numeric variables.

Example:
- 350° and 10° are both “near north,” but a normal mean gives 180° (south), which is wrong.

**weathertools** includes tools that correctly handle direction data so you don’t have to think about the math.

---

### ✅ 3) Fix unit chaos (C vs F vs K, Pa vs hPa, mph vs m/s…)
Weather datasets are often inconsistent:
- one source gives temperature in **Kelvin**
- another in **Celsius**
- another in **Fahrenheit**
- pressure might be **Pa** or **hPa**
- wind speed might be **m/s** or **mph**

**weathertools** provides lightweight unit tagging and conversion helpers, plus a dataset “janitor” function that can rename and harmonize entire tables.

---

## Installation

### From GitHub

```r
install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/weathertools")
````

---

## Quick start (copy/paste)

```r
library(weathertools)

temp_c <- c(30, 35, 40)   # air temperature
rh_pct <- c(50, 60, 65)   # relative humidity (%)

# Optional but recommended: tag unit once
attr(temp_c, "unit") <- "degC"

# Heat Index ("feels-like hot")
hi <- calcHI(temp_c, rh_pct, outputunits = "degC")

# Dew point (often a clearer humidity indicator than RH)
dp <- calcTD(temp_c, rh_pct, outputunits = "degC")

# Wet-bulb temperature (heat + humidity metric)
wb <- calcWB(temp_c, rh_pct, inputunits = "degC", outputunits = "degC", ignoreattr = TRUE)

hi; dp; wb
```

If you only need Heat Index and Dew Point, you can ignore everything else and still get value from the package.

---

## A short glossary (minimal, practical)

* **Temperature**: the air temperature in °C, °F, or Kelvin.
* **Relative humidity (RH)**: a percent (0–100). RH depends on temperature; “60%” does not mean the same moisture content at different temperatures.
* **Dew point**: a single number that tends to match “mugginess” better than RH. Higher dew point usually feels more humid.
* **Heat Index**: “feels-like hot” for warm/humid conditions (common in public heat communication).
* **Wet-bulb temperature**: another heat + humidity metric used in heat-stress contexts.
* **Wind direction**: wraps at 360°, so it must be handled with special averaging logic.

---

## The 3 core “problems” this package solves (and what to use)

### 1) The “comfort / stress” problem

**Use these when you want a single interpretable metric from basic inputs.**

* `calcHI()` — Heat Index (temperature + RH)
* `calcTD()` — Dew point (temperature + RH)
* `calcWB()` — Wet-bulb temperature (temperature + RH)
* `calcRH()` — Relative humidity from (temperature + dew point) or (temperature + VPD)
* `calcWindchill()` — Wind chill

---

### 2) The “wind direction isn’t a normal number” problem

**Use these when wind direction is in degrees (0–360).**

* `avgwdir()` — correct rolling / windowed average of wind direction (handles wraparound)
* `winddeg()` — degrees → compass label (e.g., 270 → "W")

---

### 3) The “messy units + messy column names” problem

**Use these when you need your data to be consistent across sources.**

* `unit()` — set/get/convert a unit attribute on a vector
* `convert_units()` — quick, explicit conversions
* `wx.units()` — standardize a whole dataset (rename + convert units + derive some missing fields)

---

## Unit handling (lightweight on purpose)

This package does **not** require `{units}` objects or special classes. Instead:

* vectors are plain numeric
* units can be tagged via `attr(x, "unit")`
* helpers can convert values while keeping things compatible with typical R workflows

### Example: tagging + converting

```r
x <- c(0, 10, 20)
unit(x) <- "degC"   # tag
unit(x)             # read: "degC"

unit(x) <- "degF"   # convert in place (based on existing tag)
x
```

### Example: direct conversion

```r
convert_units(c(101325, 100800), from = "Pa", to = "hPa")
convert_units(c(10, 20), from = "m/s", to = "mph")
convert_units(c(300, 305), from = "K", to = "degC")
```

**Best practice:** Tag units once on your key columns early in a pipeline. That makes downstream work safer and more readable.

---

## Working with `data.table`

Most functions are vectorized and work well inside `data.table`:

```r
library(data.table)
library(weathertools)

DT <- data.table(
  ta = c(30, 35),
  rh = c(50, 60)
)
attr(DT$ta, "unit") <- "degC"

DT[, heat_index := calcHI(ta, rh, outputunits = "degC")]
DT[, dew_point  := calcTD(ta, rh, outputunits = "degC")]

DT
```

---

## The data harmonizer: `wx.units()` (rename + unit cleanup for real-world feeds)

If you work with multiple weather sources, you quickly run into:

* inconsistent column names
* inconsistent units
* partial wind fields
* vendor/model-specific naming conventions

`wx.units()` is the “janitor” that standardizes a dataset in one call. It is designed for `data.table` and modifies in-place for speed.

### What `wx.units()` can do

1. **Rename** provider columns to canonical names (e.g., `TMP_2m_K` → `ta`)
2. **Convert** values to target units (e.g., Kelvin → °C, Pa → hPa)
3. **Tag** harmonized columns with `attr(, "unit")`
4. **Derive** some missing variables when enough inputs exist (e.g., create wind speed/direction from component columns)

### A practical example

```r
library(data.table)
library(weathertools)

dt <- data.table(
  TMP_2m_K      = c(298.15, 300.15),
  DPT_2m_K      = c(293.15, 295.15),
  PRES_sfc_Pa   = c(101325, 100800),
  U10_ms        = c(2.0, 3.5),
  V10_ms        = c(-1.0, -2.0)
)

rename_map <- c(
  "TMP_2m_K"    = "ta",
  "DPT_2m_K"    = "td",
  "PRES_sfc_Pa" = "pres",
  "U10_ms"      = "ugrd10m",
  "V10_ms"      = "vgrd10m"
)

wx.units(dt, rename_map = rename_map, debug = TRUE)

# After:
# - ta/td are in degC
# - pres is in hPa (if that is your configured target)
# - unit attributes are attached
# - if needed and supported, wind speed/direction may be derived from wind components
```

### “Wind components” (optional explanation)

Some datasets store wind as two perpendicular numeric columns rather than as “speed” and “direction.”
You do **not** need to understand the details to use `wx.units()`. If your dataset has those two component columns and is missing a wind speed/direction field, `wx.units()` can often fill in the “friendly” version.

If your data already has wind speed and wind direction, you can ignore this entirely.

---

## Pressure utilities (when elevation matters)

Pressure varies with elevation. Comparing a high-elevation station to a sea-level station usually requires adjustment.

Common tools:

* `calcPres()` — adjust station pressure to a sea-level equivalent (approximation)
* `stationpressure()` — estimate station pressure from sea-level pressure + elevation

Use these when your dataset mixes stations at different elevations and you need a comparable pressure metric.

---

## Time zone helper

`tzone()` helps read or set time zone attributes on POSIXct objects, including a “force” mode when you truly need to reinterpret a timestamp (use with care).

---

## Function map (quick reference)

**Comfort / moisture**

* `calcHI()` — Heat Index
* `calcTD()` — Dew Point
* `calcWB()` — Wet-bulb temperature
* `calcRH()` — Relative Humidity
* `calcWindchill()` — Wind Chill

**Wind**

* `avgwdir()` — correct averaging of wind direction
* `uv2wdws()` — convert wind component columns to speed/direction (when applicable)
* `winddeg()` — degrees to compass labels
* `windRun()` — wind run helper

**Pressure**

* `calcPres()` — pressure adjustment
* `stationpressure()` — inverse adjustment
* `intomb()` — inHg to mb/hPa helper

**Units + utilities**

* `unit()` — tag/get/convert units via a lightweight attribute
* `convert_units()` — explicit unit conversions
* `unitConvertRound()` — convert + round
* `wx.units()` — rename + harmonize units and derive some fields
* `tzone()` — timezone helper

(See package docs for full argument details.)

---

## Common pitfalls (and how this package helps)

1. **Kelvin mistaken for Celsius**
   A temperature around ~300 is usually Kelvin, not °C. `wx.units()` can help standardize these.

2. **Pa vs hPa**
   Pressure often arrives in Pa but is frequently expected in hPa (millibars). Conversions are included.

3. **Averaging wind direction like normal numbers**
   Use `avgwdir()` to avoid incorrect results around the 360°/0° boundary.

---

## License

MIT (see `LICENSE`).

---

## Contributing

Issues and PRs are welcome, especially for:

* additional real-world provider column mappings for `wx.units()`
* additional common unit conversions
* small, high-value weather calculations that are widely needed

When proposing a change, it helps to include:

* a short “why this is needed” example (real columns from a feed)
* expected units (input/output)
* a minimal reproducible example (MRE)

```
```
