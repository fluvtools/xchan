#' Calculate Downstream Distance of Cross Sections
#'
#' Distance along the channel axis from the **start** of the axis line to each
#' cross section’s station (midpoint between bank endpoints), in axis units.
#'
#' @param channel A channel object with planimetric cross sections.
#' @param axis Optional **LINESTRING** (`sfc` / `sfg`). Resolution matches
#'   [xt_trace_centerline()]: use this geometry, else `xt_axis(channel)`, else an
#'   error (set an axis with `xt_axis(channel) <- ...` or use [xt_generate_plan()]).
#' @returns A numeric vector of length `nrow(channel)`, distances downstream along
#'   `axis` (same row order as `channel`).
#' @note Use [xt_arrange_downstream()] if you need rows ordered by these distances.
#' @examples
#' \donttest{
#' ch <- xt_generate_plan(fraser_bankline, n = 5)
#' ds <- xt_distance_ds(ch)
#' }
#' @export
xt_distance_ds <- function(channel, axis = NULL) {
  if (!is_channel(channel)) {
    stop("Input must be a channel object")
  }

  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  axis_line <- resolve_channel_axis(channel, axis)
  mid_pts <- plan_midpoints_sfc(plan)
  as.numeric(sf::st_line_project(axis_line, mid_pts))
}
