#' Extender Functions
#'
#' Extender functions create operators that extend the topographic frame
#' beyond the current channel extent. These functions return extender
#' objects that can be used in frame extension operations.
#'
#' @param extent Distance to extend (in meters). Can be a vector for varying extensions.
#' @param elevation Elevation specification for the extension
#' @param slope Slope of the extension (rise/run). Can be a vector for varying slopes.
#' @returns An extender object that can be used in frame extension
#' @rdname extenders
#' @export
extender_flat <- function(extent, elevation = elevation_bank()) {
  checkmate::assert_numeric(extent, lower = 0, any.missing = FALSE)
  checkmate::assert_class(elevation, "xchan_elevation")

  f <- function(channel, side = "both") {
    # Implementation will create flat extensions
    # For now, return a placeholder
    list(extent = extent, elevation = elevation, type = "flat")
  }
  structure(
    f,
    name = "flat",
    params = list(extent = extent, elevation = elevation),
    class = "xchan_extender"
  )
}

#' @rdname extenders
#' @export
extender_slope <- function(extent, slope, elevation = elevation_bank()) {
  checkmate::assert_numeric(extent, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(slope, any.missing = FALSE)
  checkmate::assert_class(elevation, "xchan_elevation")

  f <- function(channel, side = "both") {
    # Implementation will create sloped extensions
    # For now, return a placeholder
    list(extent = extent, slope = slope, elevation = elevation, type = "slope")
  }
  structure(
    f,
    name = "slope",
    params = list(extent = extent, slope = slope, elevation = elevation),
    class = "xchan_extender"
  )
}
