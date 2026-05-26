# Squamish River Bankline

A polygon representing a Squamish River channel footprint used in
package examples and tests. The geometry is intended as a lightweight
demonstration input for generating planimetric cross sections.

## Usage

``` r
Squamish_bankline
```

## Format

An `sfc` polygon geometry in EPSG:3005.

## Source

Natural Resources Canada CanVec Hydro (`BC_Hydro_shp`). The source data
were extracted to the Squamish River demo area and transformed to
EPSG:3005 for package use. See `data-raw/Squamish_bankline.R` for the
local processing script.
