#' Squamish River Bankline
#'
#' A polygon representing a Squamish River channel footprint used in package
#' examples and tests. The geometry is intended as a lightweight demonstration
#' input for generating planimetric cross sections.
#'
#' @format An `sfc` polygon geometry in EPSG:3005.
#'
#' @source Natural Resources Canada CanVec Hydro (`BC_Hydro_shp`). The source
#' data were extracted to the Squamish River demo area and transformed to
#' EPSG:3005 for package use. See `data-raw/squamish_bankline.R` for the local
#' processing script.
#'
#' @name squamish_bankline
"squamish_bankline"

#' Squamish River Demo DEM
#'
#' A clipped high-resolution digital terrain model used in package examples to
#' generate profile cross sections from the `squamish_bankline` demo geometry.
#' The object is stored as a wrapped `terra` raster so it can be included as
#' package data; unwrap it with `terra::unwrap()` before use.
#'
#' The DEM is LiDAR-derived and does not include submerged bathymetry. Examples
#' and tests that build Squamish profiles therefore follow
#' [xt_generate_profile()] with [xt_dredge_to()] and [bathy_rectangle()] to
#' insert a synthetic 3 m deep rectangular channel.
#'
#' @format A wrapped `terra` `SpatRaster`, projected to EPSG:3005.
#'
#' @source CanElevation - Canada Digital Elevation Models (HRDEM 1 m DTM).
#' The source GeoTIFF is intentionally not tracked in git because it is large;
#' recreate it with `data-raw/download_dem.py`, then process it with
#' `data-raw/squamish_dem.R`.
#'
#' @name squamish_dem
"squamish_dem"
