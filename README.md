# weathertools

**weathertools** is an R package that helps you turn raw “weather-like” columns (temperature, humidity, wind, pressure, etc.) into **useful, consistent, analysis-ready values**—without needing a meteorology background. It’s built for everyday data workflows (including `data.table`) and keeps “units” handling lightweight and practical. 

If you have ever asked:

* “Is this temperature in **C**, **F**, or **Kelvin**?”
* “Why is pressure sometimes **Pa** and sometimes **hPa**?”
* “How do I compute dew point / heat index from the columns I already have?”
* “How do I standardize vendor fields into one clean schema?”

…this package is for that.

---

## What you get (in plain terms)

### 1) “Compute common comfort / moisture metrics”

Given temperature and humidity, you can compute:

* **Heat index**: “feels-like hot” temperature used widely in public heat messaging (`calcHI`). 
* **Dew point**: a more intuitive measure of “how humid it actually is” than RH (`calcTD`). 
* **Wet-bulb temperature**: a heat + humidity metric often used in heat stress contexts (`calcWB`). 
* **Relative humidity** from other combos (e.g., temperature + dew point, or temperature + VPD) (`calcRH`). 
* **Wind chill** for cold conditions (`calcWindchill`). 

### 2) “Make unit chaos go away”

It includes a simple unit tag (`attr(x, "unit")`) plus helpers to convert values safely:

* `unit()` reads/sets a `"unit"` attribute and can convert + optionally round. 
* `convert_units()` provides a fixed set of common conversions (C/F/K, Pa/hPa, mph/m/s, etc.). 

### 3) “Standardize messy incoming data to one clean schema”

The function **`wx.units()`** is the workhorse for real-world feeds. It can:

* rename provider columns to canonical names,
* convert numeric values into canonical units,
* tag harmonized columns with a `"unit"` attribute,
* and even derive missing wind fields when enough info is present. 

(Details later—this README starts simple and gradually adds depth.)

---

## Installation

### GitHub

```r
install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/weathertools")
```

### Local source

```r
install.packages("path/to/weathertools", repos = NULL, type = "source")
```

---

## 60-second start (no jargon)

```r
library(weathertools)

# Example inputs
temp_c <- c(30, 35, 40)   # air temperature in °C
rh_pct <- c(50, 60, 65)   # relative humidity in %

# Tell the vector what unit it is (optional but recommended)
attr(temp_c, "unit") <- "degC"

# "Feels-like hot" temperature
hi <- calcHI(temp_c, rh_pct, outputunits = "degC")

# Dew point
dp <- calcTD(temp_c, rh_pct, outputunits = "degC")

# Wet-bulb temperature
wb <- calcWB(temp_c, rh_pct, inputunits = "degC", outputunits = "degC", ignoreattr = TRUE)

hi; dp; wb
```

---

## Key ideas (explained for non-weather folks)

### Temperature

Just the number you already know. The main “gotcha” is unit confusion:

* °C vs °F vs Kelvin (K)

### Relative Humidity (RH)

A percent (0–100) that depends on temperature. RH is useful, but it can be confusing because “50%” does not mean the same amount of moisture at different temperatures.

### Dew point (easy mental model)

Dew point is a single number that usually matches how “humid” it feels:

* higher dew point → “muggier” air
* lower dew point → “drier” air

That’s why many people prefer dew point over RH for interpretation.

### Heat index

A “feels-like” hot temperature—commonly used in public heat guidance and sports safety messaging.

### Wet-bulb temperature

Another heat + humidity measure often used in heat stress contexts. If you do not need it, you can ignore it; it’s included because many applied users do.

---

## Unit handling (practical, not heavy)

This package does **not** require `{units}` objects or a formal unit system. Instead:

* values are plain numeric vectors,
* you can tag a column with `attr(x, "unit") <- "degC"` (recommended),
* functions can either honor those tags or you can override behavior explicitly.

### `unit()` is the quick helper

`unit()` reads/sets a `"unit"` attribute and can convert values when you assign a new unit. 

```r
x <- c(0, 10, 20)
unit(x) <- "degC"     # tag
unit(x)              # read

unit(x) <- "degF"     # convert using existing tag
x
unit(x)
```

### Common conversions supported

`wx.units()` (and the internal converter it uses) supports common, practical pairs like:

* K ↔ °C, °C ↔ °F
* Pa ↔ hPa
* mph ↔ m/s
* feet ↔ meters, miles ↔ km
* and a few common precipitation/rate conversions 

---

## Using with `data.table` (typical workflow)

Most functions are vectorized and work naturally in `data.table`:

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

## The “big one”: cleaning and standardizing messy feeds with `wx.units()`

If you are pulling data from different sources (APIs, forecast models, sensors, vendors), you often get:

* inconsistent column names
* inconsistent units
* partial wind fields
* mixed conventions

`wx.units()` is designed to make that manageable.

### What it does

It modifies your `data.table` **in place** (“by reference”) for speed and simplicity in pipelines. 

It can:

* apply optional renaming,
* convert numeric values into canonical target units,
* tag each harmonized column with `attr(, "unit")`,
* derive missing wind fields when enough info exists, and
* optionally run sanity checks to catch common mistakes (like values that still look like Kelvin after a conversion). 

### Important: it does not “guess”

By default, `wx.units()` only uses **explicit signals** (provider naming patterns, overrides, or existing unit attributes). If it cannot determine a unit, it can error (strict mode) or skip those columns (tolerant mode). 

### Example: rename + harmonize units

```r
library(data.table)
library(weathertools)

dt <- data.table(
  TMP_2m_K           = c(298.15, 300.15),
  DPT_2m_K           = c(293.15, 295.15),
  UGRD_10m_ms        = c( 2.0, 3.5),
  VGRD_10m_ms        = c(-1.0, -2.0),
  DSWRF_surface_Wm^2 = c(500, 750),
  PRES_surface_Pa    = c(101325, 100800),
  TCDC_percent       = c(40, 75)
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
```

After running, `dt` will have canonical names and units; for example:

* `ta` / `td` become °C,
* `pres` becomes hPa,
* `dswrf` stays W/m²,
* and if wind speed/direction are missing but enough wind info exists, they can be derived. 

### Canonical targets (quick reference)

The built-in defaults include (examples):

* temperatures (`ta`, `td`) → °C
* wind (`wind10m`, `ugrd10m`, `vgrd10m`) → m/s and direction in degrees
* pressure (`pres`) → hPa
* radiation (`dswrf`, `solar`) → W/m²
* precipitation rate (`prate`) → mm/h and accumulation (`apcp_sfc`) → mm 

You can override any of these targets with `target_override`. 

---

## “I don’t recognize these wind columns” (no problem)

Many forecast/model datasets store wind as **two perpendicular components** instead of a single “speed” column. You do *not* need to understand the physics to use this package:

* If your source provides two wind component columns, `wx.units()` can create the familiar:

  * **wind speed** and
  * **wind direction**
    when those are missing. 

If you already have wind speed and direction, you can ignore components entirely.

---

## Time zones (small but useful)

`tzone()` is a lightweight helper to read or set the `"tzone"` attribute on POSIX date-times, with an optional “force” mode when you truly want to reinterpret clock time in a new zone. 

---

## Function overview (what to reach for)

The package documentation lists the available functions, including:

* `calcHI`, `calcRH`, `calcTD`, `calcWB`, `calcWindchill` 
* `calcPres` (pressure-related) 
* `avgwdir` (proper averaging of wind direction) 
* `tzone`, `unit` and more 

---

## Common mistakes this package helps prevent

1. **Kelvin vs Celsius**
   A temperature around 300 is usually **Kelvin**, not °C. `wx.units()` includes optional checks to catch “conversion didn’t actually happen” scenarios. 

2. **Pa vs hPa**
   Pressure is often delivered as **Pa** but many workflows expect **hPa** (aka millibars). The converter supports Pa ↔ hPa. 

3. **Averaging directions like normal numbers**
   Direction wraps at 360°, so 350° and 10° are close—not opposite. Use `avgwdir`. 

---

## License

MIT (see `LICENSE`). 

---
