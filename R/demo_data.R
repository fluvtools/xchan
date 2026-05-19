#' Fraser Bankline
#'
#' Bankline geometry for a Fraser tributary demo area in British Columbia.
#'
#' @format A length-1 `sfc_MULTIPOLYGON` object in EPSG:3005.
#' @source Canada's CanVec Hydrography Waterbody.
"demo_bankline"

#' Fraser DEM
#'
#' Digital elevation model for a Fraser tributary demo area in British Columbia.
#'
#' @format A `terra::PackedSpatRaster` object.
#' @source Copernicus DEM (GLO-30), cropped/resampled for package examples.
#' @details Convert to a `terra::SpatRaster` with `terra::unwrap(demo_dem)`.
"demo_dem"
