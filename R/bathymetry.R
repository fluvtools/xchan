#' Bathymetry specifications for dredging profile cross sections
#'
#' These functions construct **bathymetry specification** objects (class
#' `"xchan_bathymetry"`). Pass one to [xt_dredge_to()] to replace the wetted bed
#' of each profile cross section with a synthetic channel shape.
#'
#' @section Water surface and depth:
#'
#' Both constructors take a `depth` measured downward from a water-surface
#' elevation (`wse`, water surface elevation). By default, `wse` is
#' [elevation_bank()], which uses the lower of the two outer bank elevations on
#' each cross section.
#'
#' @section Dredging vs filling:
#'
#' Dredging adjusts profile geometry toward the target bathymetry. Material is
#' removed where the existing bed lies above the target, and added where the
#' existing channel is deeper than the target. Bank positions and elevations are
#' preserved; spans between consecutive bank contacts are modified.
#'
#' @section Islands and multi-channel profiles:
#'
#' When a cross section has more than one wetted interval (for example a mid-channel
#' island), every span between consecutive bank contacts is dredged independently,
#' including the island interior between inner bank pairs. The same target
#' bathymetry is applied to each span on that section.
#'
#' @param depth Channel depth below `wse`. Must be non-negative.
#' @param wse Water surface elevation (WSE) specification (class
#'   `"xchan_elevation"`).
#' @param thalweg_frac Position of the thalweg as a fraction of the distance
#'   from the left bank to the right bank within each dredged span (`0` = left
#'   bank, `1` = right bank). Values of `0` or `1` place the full channel depth
#'   at that bank with a vertical bed cliff there.
#'
#' @returns An object inheriting `"xchan_bathymetry"`: a small bathymetry
#'   specification consumed by [xt_dredge_to()].
#'
#' @rdname bathymetry
#' @aliases bathymetry
#' @seealso [xt_dredge_to()], [elevation_bank()]
#' @export
bathy_rectangle <- function(depth, wse = elevation_bank()) {
  checkmate::assert_number(depth, lower = 0)
  assert_bathymetry_wse(wse)
  structure(
    list(
      shape = "rectangle",
      depth = depth,
      wse = wse
    ),
    name = "rectangle",
    params = list(depth = depth, wse = wse),
    class = "xchan_bathymetry"
  )
}

#' @describeIn bathymetry V-shaped channel with zero depth at the banks and
#'   `depth` below `wse` at the thalweg. Use `thalweg_frac = 0` or `1` for a
#'   vertical bed cliff at the left or right bank.
#' @export
bathy_vshape <- function(depth, thalweg_frac, wse = elevation_bank()) {
  checkmate::assert_number(depth, lower = 0)
  checkmate::assert_number(thalweg_frac, lower = 0, upper = 1)
  assert_bathymetry_wse(wse)
  structure(
    list(
      shape = "vshape",
      depth = depth,
      thalweg_frac = thalweg_frac,
      wse = wse
    ),
    name = "vshape",
    params = list(
      depth = depth,
      thalweg_frac = thalweg_frac,
      wse = wse
    ),
    class = "xchan_bathymetry"
  )
}

#' @noRd
assert_bathymetry_wse <- function(wse) {
  if (!inherits(wse, "xchan_elevation")) {
    stop(
      "`wse` must be an elevation specification (for example ",
      "`elevation_bank()`).",
      call. = FALSE
    )
  }
  invisible(wse)
}
