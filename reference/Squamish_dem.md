# Squamish River Demo DEM

A clipped high-resolution digital terrain model used in package examples
to generate profile cross sections from the `Squamish_bankline` demo
geometry. The object is stored as a wrapped `terra` raster so it can be
included as package data; unwrap it with
[`terra::unwrap()`](https://rspatial.github.io/terra/reference/wrap.html)
before use.

## Usage

``` r
Squamish_dem
```

## Format

A wrapped `terra` `SpatRaster`, projected to EPSG:3005.

## Source

CanElevation - Canada Digital Elevation Models (HRDEM 1 m DTM). The
source GeoTIFF is intentionally not tracked in git because it is large;
recreate it with `data-raw/download_dem.py`, then process it with
`data-raw/Squamish_dem.R`.
