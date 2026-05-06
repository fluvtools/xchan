#' Trace centerline from channel
#'
#' Trace a polyline through planimetric cross-section midpoints in **downstream**
#' order along the channel axis (see **Details**).
#'
#' @param channel Channel object with planimetric cross sections.
#' @param axis Optional LINESTRING axis (`sfc` / `sfg`). If `NULL`, uses
#'   `xt_axis(channel)`; if that is also `NULL`, an error is raised.
#'
#' @returns An sf `LINESTRING` `sfc` (length 1) through section midpoints, ordered
#'   by increasing distance projected onto `axis`.
#'
#' @details Midpoints use the mean of each section’s **first and last** vertices
#'   (bank-to-bank). Vertex order follows **increasing** [sf::st_line_project()]
#'   distance along `axis` from its start — **not** data-frame row order. Store or
#'   pass an axis ([xt_axis()]; [xt_generate_plan()] records one). To align table
#'   rows with that order, use [xt_arrange_downstream()].
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

  axis_line <- resolve_channel_axis(channel, axis, axis_arg_name = "axis")
  mid_pts <- plan_midpoints_sfc(plan)
  d <- as.numeric(sf::st_line_project(axis_line, mid_pts))
  ord <- order(d)

  xy <- sf::st_coordinates(mid_pts)[ord, 1:2, drop = FALSE]
  centerline <- sf::st_linestring(xy)
  sf::st_sfc(centerline, crs = sf::st_crs(plan))
}
