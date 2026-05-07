#' Sort channel rows by distance along the axis
#'
#' Reorders rows so cross sections progress downstream by projection onto `axis`
#' ([xt_axis()] or explicit `axis` argument).
#'
#' @param channel A channel object (`xchan`).
#' @param axis Optional LINESTRING axis (`sfc` / `sfg`) passed through for
#'   projection ordering.
#'
#' @returns The same channel object with rows permuted (attributes such as
#'   `axis` and `plan_col` preserved).
#'
#' @seealso [xt_axis()], [xt_trace_centerline()]
#' @export
#' @examples
#' \donttest{
#' ch <- xt_generate_plan(fraser_bankline, n = 15)
#' ch_shuf <- ch[sample.int(xt_n_sections(ch)), ]
#' ch_back <- xt_arrange_downstream(ch_shuf)
#' }
xt_arrange_downstream <- function(channel, axis = NULL) {
  checkmate::assert_class(channel, "xchan")
  axis_line <- resolve_channel_axis(channel, axis)
  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections.", call. = FALSE)
  }

  mid_pts <- plan_midpoints_sfc(plan)
  d <- as.numeric(sf::st_line_project(axis_line, mid_pts))
  ord <- order(d)
  channel[ord, , drop = FALSE]
}
