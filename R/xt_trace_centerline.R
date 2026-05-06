#' Trace centerline from channel
#'
#' Trace a polyline through planimetric cross-section midpoints in **downstream**
#' order along the channel axis (see **Details**).
#'
#' @param channel Channel object with planimetric cross sections.
#' @param axis Optional LINESTRING axis (`sfc` / `sfg`). If `NULL`, uses
#'   `xt_axis(channel)`; if that is also `NULL`, an error is raised.
#'
#' @returns An sf `LINESTRING` `sfc` (length 1) through section midpoints. Vertex
#'   order follows increasing **`chainage`** when that column is present (see
#'   [xt_generate_plan()]); otherwise increasing projection onto `axis`.
#'
#' @details Midpoints use the mean of each section’s **first and last** vertices
#'   (bank-to-bank). If the channel has a valid numeric **`chainage`** column
#'   ([xt_has_chainage()][xt_has_profile]), downstream vertex order follows **`chainage`** and an
#'   axis need not be stored for this trace. Otherwise order follows
#'   [sf::st_line_project()] along `axis` ([xt_axis()] or explicit argument). Use
#'   [xt_arrange_downstream()] to sort rows by **`chainage`** or projection.
#'
#' @examples
#' centerline <- xt_trace_centerline(channel)
#' plot(centerline, col = "red", lwd = 2)
#' @export
xt_trace_centerline <- function(channel, axis = NULL) {
  if (!is_channel(channel)) {
    stop("Input must be a channel object")
  }

  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  mid_pts <- plan_midpoints_sfc(plan)
  if (has_chainage_column(channel)) {
    ord <- order(channel[["chainage"]])
  } else {
    axis_line <- resolve_channel_axis(channel, axis, axis_arg_name = "axis")
    ord <- order(as.numeric(sf::st_line_project(axis_line, mid_pts)))
  }

  xy <- sf::st_coordinates(mid_pts)[ord, 1:2, drop = FALSE]
  centerline <- sf::st_linestring(xy)
  sf::st_sfc(centerline, crs = sf::st_crs(plan))
}
