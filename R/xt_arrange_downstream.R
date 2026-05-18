#' Sort channel rows by distance along the axis
#'
#' `xt_arrange_downstream()` permutes cross sections so they follow **increasing**
#' projected distance along `axis` from the axis **start** ([xt_distance_downstream()]). After
#' [xt_reverse_flow()], the stored axis (if any) is reversed, so that zero chainage is at the
#' opposite end of the same geographic line — increasing distance is then hydrologic
#' downstream with reversed flow. `xt_arrange_upstream()` uses **decreasing** distance from
#' the axis start.
#'
#' @param channel An [`xchan`][xchan()] object.
#' @param axis Optional LINESTRING (`sfc` / `sfg`) for projection ordering (same rules as
#'   [xt_distance_downstream()]).
#'
#' @returns An [`xchan`] with sections reordered along the axis (attributes `crs` and
#'   `axis` preserved).
#'
#' @details
#' Ordering uses the same chainage as [xt_distance_downstream()]: intersection of the
#' extended bank-to-bank chord with the axis (nearest intersection to the bank midpoint when
#' there are several), otherwise the nearest point on the axis to the bank midpoint.
#'
#' **Reverse flow:** [xt_reverse_flow()] does not permute sections but reverses the stored axis,
#' so `xt_arrange_downstream()` orders sections for the new downstream direction.
#' [elevation_bank()] with default `min` is unchanged until you reorder.
#'
#' @seealso [xt_axis()], [xt_distance_downstream()], [xt_reverse_flow()]
#' @rdname xt_arrange_downstream
#' @aliases xt_arrange_upstream
#' @examples
#' \donttest{
#' ch <- xt_generate_plan(demo_bankline, n = 15)
#' ch_down <- xt_arrange_downstream(ch)
#' ch_up <- xt_arrange_upstream(ch)
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
xt_arrange_downstream.xchan <- function(channel, axis = NULL) {
  arrange_xchan_by_ds(channel, axis, upstream = FALSE)
}

#' @rdname xt_arrange_downstream
#' @usage NULL
#' @export
xt_arrange_upstream.xchan <- function(channel, axis = NULL) {
  arrange_xchan_by_ds(channel, axis, upstream = TRUE)
}

#' @rdname xt_arrange_downstream
#' @usage NULL
#' @exportS3Method xt_arrange_downstream default
xt_arrange_downstream.default <- function(channel, axis = NULL) {
  stop(
    "No `xt_arrange_downstream()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan` object.",
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
    ". Use an `xchan` object.",
    call. = FALSE
  )
}

#' @noRd
arrange_xchan_by_ds <- function(channel, axis, upstream = FALSE) {
  checkmate::assert_class(channel, "xchan")
  axis_line <- resolve_channel_axis(channel, axis)
  plan <- channel_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections.", call. = FALSE)
  }

  d <- plan_chainage_on_axis(plan, axis_line)
  ord <- if (upstream) {
    order(d, decreasing = TRUE)
  } else {
    order(d)
  }

  xchan_reorder_sections(channel, ord)
}

#' @noRd
xchan_reorder_sections <- function(x, ord) {
  checkmate::assert_class(x, "xchan")
  n <- length(x)
  checkmate::assert_integerish(
    ord,
    len = n,
    unique = TRUE,
    sorted = FALSE,
    lower = 1,
    upper = n
  )
  secs <- vector("list", n)
  for (i in seq_along(ord)) {
    secs[[i]] <- x[[ord[i]]]
  }
  sid <- attr(x, "section_i", exact = TRUE)
  if (is.null(sid) || length(sid) != n) {
    sid <- seq_len(n)
  }
  new_sid <- sid[ord]
  structure(
    secs,
    crs = attr(x, "crs", exact = TRUE),
    axis = attr(x, "axis", exact = TRUE),
    bankline = attr(x, "bankline", exact = TRUE),
    section_i = new_sid,
    class = class(x)
  )
}
