#' Create 3D coordinates by mapping profile distances to plan positions
#'
#' Maps profile elevation data to plan-view coordinates. Horizontal positions use
#' the same chord mapping as plan export with \code{extent = "full"} in
#' \code{\link[=xt_as_sfc]{xt_as_sfc()}} (first to last vertex of the plan line).
#'
#' @param plan Planimetric cross section (`LINESTRING`).
#' @param profile Profile cross section (`xs_profile`).
#' @param extent `"banks"` uses samples whose horizontal coordinate lies between
#'   the outer bank distances; `"full"` uses all profile samples; `"wetted"` uses
#'   each water interval between consecutive banks (one part per interval).
#' @returns `MULTILINESTRING` with XYZ vertices (same CRS as `plan` for x,y).
#' @noRd
create_3d_coords <- function(
  plan,
  profile,
  extent = c("banks", "full", "wetted")
) {
  extent <- match.arg(extent)
  parts <- profile_coord_parts_for_extent(profile, extent)
  lines <- lapply(parts, function(coords) {
    if (nrow(coords) < 2L) {
      stop(
        "Not enough profile vertices for a line in this `extent`.",
        call. = FALSE
      )
    }
    n <- nrow(coords)
    xy <- matrix(NA_real_, n, 2)
    for (i in seq_len(n)) {
      xy[i, ] <- transect_xy_from_relative(plan, coords[i, 1])
    }
    cbind(x = xy[, 1], y = xy[, 2], z = coords[, 2])
  })
  sf::st_multilinestring(lines)
}

#' Profile polyline in distance–elevation space for sf export
#'
#' @noRd
profile_linestring_for_extent <- function(
  profile,
  extent = c("banks", "full", "wetted")
) {
  extent <- match.arg(extent)
  parts <- profile_coord_parts_for_extent(profile, extent)
  lines <- lapply(parts, function(coords) {
    if (nrow(coords) < 2L) {
      stop(
        "Not enough profile vertices for a line in this `extent`.",
        call. = FALSE
      )
    }
    sf::st_linestring(coords)
  })
  as_line_or_multilinestring(lines)
}
