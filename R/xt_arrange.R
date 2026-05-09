#' Sort channel rows by distance along the axis
#'
#' `xt_arrange_downstream()` permutes rows so cross sections follow **increasing**
#' projected distance along `axis` from the axis **start** ([xt_distance_downstream()]). After
#' [xt_reverse_flow()], the stored axis (if any) is reversed, so that zero chainage is at the
#' opposite end of the same geographic line — increasing distance is then hydrologic
#' downstream with reversed flow. `xt_arrange_upstream()` uses **decreasing** distance from
#' the axis start.
#'
#' @param channel An \code{xchan_tbl} or [`xchan`][xchan()]. For a bare
#'   [`xchan`][xchan()], if no axis is stored on the coerced table, pass `axis` explicitly
#'   (the same line you would use for [xt_distance_downstream()]).
#' @param axis Optional LINESTRING (`sfc` / `sfg`) for projection ordering (same rules as
#'   [xt_distance_downstream()]).
#'
#' @returns Object of the same class as `channel`, rows permuted. For [`xchan`][xchan()],
#'   returns the reordered geometry vector only (same length as input).
#'
#' @details
#' Midpoints for ordering are bank-to-bank means on each planimetric cross section.
#'
#' **Reverse flow:** [xt_reverse_flow()] does not permute rows but reverses the stored axis,
#' so `xt_arrange_downstream()` orders sections for the new downstream direction.
#' [elevation_bank()] with default `min` is unchanged until you reorder rows.
#'
#' @seealso [xt_axis()], [xt_distance_downstream()], [xt_reverse_flow()]
#' @rdname xt_arrange_downstream
#' @aliases xt_arrange_upstream
#' @examples
#' \donttest{
#' ch <- xt_generate_plan(fraser_bankline, n = 15)
#' ch_shuf <- ch[sample.int(xt_n_sections(ch)), ]
#' ch_down <- xt_arrange_downstream(ch_shuf)
#' ch_up <- xt_arrange_upstream(ch_shuf)
#' }
#' @export
xt_arrange_downstream <- function(channel, axis = NULL) {
  UseMethod("xt_arrange_downstream")
}

#' @rdname xt_arrange_downstream
#' @export
xt_arrange_upstream <- function(channel, axis = NULL) {
  UseMethod("xt_arrange_upstream")
}

#' @rdname xt_arrange_downstream
#' @usage NULL
#' @export
xt_arrange_downstream.xchan_tbl <- function(channel, axis = NULL) {
  arrange_channel_rows_by_ds(channel, axis, upstream = FALSE)
}

#' @rdname xt_arrange_downstream
#' @usage NULL
#' @export
xt_arrange_upstream.xchan_tbl <- function(channel, axis = NULL) {
  arrange_channel_rows_by_ds(channel, axis, upstream = TRUE)
}

#' @rdname xt_arrange_downstream
#' @usage NULL
#' @export
xt_arrange_downstream.xchan <- function(channel, axis = NULL) {
  tbl <- xt_as_channel(channel)
  out_tbl <- xt_arrange_downstream(tbl, axis = axis)
  out_tbl[[attr(out_tbl, "xsection_col", exact = TRUE)]]
}

#' @rdname xt_arrange_downstream
#' @usage NULL
#' @export
xt_arrange_upstream.xchan <- function(channel, axis = NULL) {
  tbl <- xt_as_channel(channel)
  out_tbl <- xt_arrange_upstream(tbl, axis = axis)
  out_tbl[[attr(out_tbl, "xsection_col", exact = TRUE)]]
}

#' @rdname xt_arrange_downstream
#' @usage NULL
#' @exportS3Method xt_arrange_downstream default
xt_arrange_downstream.default <- function(channel, axis = NULL) {
  stop(
    "No `xt_arrange_downstream()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan_tbl` or `xchan` object.",
    call. = FALSE
  )
}

#' @rdname xt_arrange_downstream
#' @usage NULL
#' @exportS3Method xt_arrange_upstream default
xt_arrange_upstream.default <- function(channel, axis = NULL) {
  stop(
    "No `xt_arrange_upstream()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan_tbl` or `xchan` object.",
    call. = FALSE
  )
}

#' @noRd
arrange_channel_rows_by_ds <- function(channel, axis, upstream = FALSE) {
  checkmate::assert_class(channel, "xchan_tbl")
  axis_line <- resolve_channel_axis(channel, axis)
  plan <- channel_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections.", call. = FALSE)
  }

  mid_pts <- plan_midpoints_sfc(plan)
  d <- as.numeric(sf::st_line_project(axis_line, mid_pts))
  ord <- if (upstream) {
    order(d, decreasing = TRUE)
  } else {
    order(d)
  }

  reorder_xchan_tbl_rows(channel, ord)
}

#' Reorder rows and preserve xchan_tbl attributes
#'
#' @noRd
reorder_xchan_tbl_rows <- function(channel, ord) {
  out <- channel[ord, , drop = FALSE]
  ensure_xchan_tbl_attrs(channel, out)
}

#' @noRd
ensure_xchan_tbl_attrs <- function(template, x) {
  attr(x, "xsection_col") <- attr(template, "xsection_col", exact = TRUE)
  attr(x, "axis") <- attr(template, "axis", exact = TRUE)
  if ("xchan_tbl" %in% class(template)) {
    class(x) <- c("xchan_tbl", setdiff(class(x), "xchan_tbl"))
  }
  x
}
