#' Distance along the channel axis to cross-section midpoints
#'
#' @description
#' **`xt_distance_downstream()`** returns distance measured along the axis **from its start**
#' to each cross section's bank-to-bank midpoint. **`xt_distance_upstream()`** returns
#' distance along the axis **from each midpoint to the end** of the axis (equivalently:
#' axis length minus downstream distance). Together they satisfy
#' `xt_distance_downstream(x) + xt_distance_upstream(x) == axis_length` at each section when
#' lengths are numeric.
#'
#' @param channel An [`xchan`] with planimetric cross sections.
#' @param axis Optional **LINESTRING** (`sfc` / `sfg`). Resolution matches
#'   [xt_trace_centerline()]: use this geometry, else `xt_axis(channel)`, else an
#'   error (set an axis with `xt_axis(channel) <- ...` or use [xt_generate_plan()]).
#' @returns A numeric vector of length `length(channel)` (same section order as `channel`). The
#'   result carries [units::units()] when the channel has a CRS with a defined linear unit;
#'   plain numeric otherwise.
#' @note Use [xt_arrange_downstream()] if you need sections ordered by downstream chainage.
#'
#' @examples
#' \donttest{
#' ch <- xt_generate_plan(fraser_bankline, n = 5)
#' xt_distance_downstream(ch)
#' xt_distance_upstream(ch)
#' }
#'
#' @export
xt_distance_downstream <- function(channel, axis = NULL) {
  raw <- axis_distances_numeric(channel, axis)
  plan <- channel_plan(channel)
  with_length_units(raw, crs_length_unit(plan))
}

#' @rdname xt_distance_downstream
#' @export
xt_distance_upstream <- function(channel, axis = NULL) {
  raw <- axis_distances_upstream_numeric(channel, axis)
  plan <- channel_plan(channel)
  with_length_units(raw, crs_length_unit(plan))
}

#' @noRd
axis_distances_numeric <- function(channel, axis = NULL) {
  if (!xt_is_channel(channel)) {
    stop("Input must be a channel object")
  }
  plan <- channel_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }
  axis_line <- resolve_channel_axis(channel, axis)
  mid_pts <- plan_midpoints_sfc(plan)
  as.numeric(sf::st_line_project(axis_line, mid_pts))
}

#' @noRd
axis_distances_upstream_numeric <- function(channel, axis = NULL) {
  d <- axis_distances_numeric(channel, axis)
  axis_line <- resolve_channel_axis(channel, axis)
  as.numeric(sf::st_length(axis_line)) - d
}
