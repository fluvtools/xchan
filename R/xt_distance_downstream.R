#' Distance along the channel axis to cross-section stations
#'
#' @description
#' **`xt_distance_downstream()`** returns distance measured along the axis
#' **from its start** to the intersection of the axis with each cross section’s
#' **extended** bank-to-bank chord (the line through the first and last plan
#' vertices, extended if needed so it meets the axis). If that infinite line
#' does not intersect the axis, the chainage of the **nearest** point on the
#' axis to the bank midpoint is used instead. **`xt_distance_upstream()`**
#' returns distance along the axis **from that station to the end** of the axis
#' (equivalently: axis length minus downstream distance). Together they satisfy
#' `xt_distance_downstream(x) + xt_distance_upstream(x) == axis_length` at each
#' section when lengths are numeric.
#'
#' @param channel An [`xchan`] with planimetric cross sections.
#' @param axis Optional **LINESTRING** (`sfc` / `sfg`). If supplied, distances
#'   are
#'   measured along this line; otherwise `xt_axis(channel)` is used; if that is
#'   `NULL`, an error is raised (set an axis with `xt_axis(channel) <- ...` or
#'   use [xt_generate_plan()]).
#' @returns A numeric vector of length `length(channel)` (same section order as
#'   `channel`). The
#'   result carries [units::units()] when the channel has a defined length unit
#'   (from its CRS or from manual unit-bearing widths/profile input); plain
#'   numeric otherwise.
#' @note Use [xt_arrange_downstream()] if you need sections ordered by
#' downstream chainage.
#'
#' @examples
#' \donttest{
#' ch <- xt_generate_plan(squamish_bankline, n = 5)
#' xt_distance_downstream(ch)
#' xt_distance_upstream(ch)
#' }
#'
#' @export
xt_distance_downstream <- function(channel, axis = NULL) {
  raw <- axis_distances_numeric(channel, axis)
  with_length_units(raw, channel_length_unit(channel))
}

#' @rdname xt_distance_downstream
#' @export
xt_distance_upstream <- function(channel, axis = NULL) {
  raw <- axis_distances_upstream_numeric(channel, axis)
  with_length_units(raw, channel_length_unit(channel))
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
  as.numeric(plan_chainage_on_axis(plan, axis_line))
}

#' @noRd
axis_distances_upstream_numeric <- function(channel, axis = NULL) {
  d <- axis_distances_numeric(channel, axis)
  axis_line <- resolve_channel_axis(channel, axis)
  as.numeric(sf::st_length(axis_line)) - d
}
