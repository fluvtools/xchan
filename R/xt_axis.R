#' Channel axis (LINESTRING)
#'
#' Get or set the reference axis used for downstream distance and ordering:
#' [xt_trace_centerline()], [xt_arrange_downstream()], [xt_distance_downstream()],
#' [xt_gradient()], etc. Channels built with [xt_generate_plan()] store the
#' sampling axis automatically.
#'
#' @param channel An \code{xchan_tbl} object (the channel table), not a
#'   bare \link[=xchan]{xchan} geometry vector.
#' @param value A single **LINESTRING** as `sfc` or `sfg`, same CRS as the plan
#'   column (else transformed with a warning).
#'
#' @details
#' The axis is **reach-scale** geometry (one polyline along the channel). It is
#' stored only on the `xchan_tbl` wrapper (attribute `"axis"`), not duplicated
#' onto each `xchan` row. Call `xt_axis(bar)`, not `xt_axis(bar$xsection)`.
#'
#' @returns For `xt_axis()`, the stored `sfc_LINESTRING` or `NULL`. For
#'   assignment, an updated channel with attribute `axis`.
#'
#' @seealso [xt_arrange_downstream()], [xt_trace_centerline()]
#' @export
#' @examples
#' \donttest{
#' library(sf)
#' ch <- xt_generate_plan(fraser_bankline, n = 20)
#' ax <- xt_axis(ch)
#' plot(ax)
#' }
xt_axis <- function(channel) {
  UseMethod("xt_axis")
}

#' @rdname xt_axis
#' @export
xt_axis.xchan_tbl <- function(channel) {
  checkmate::assert_class(channel, "xchan_tbl")
  attr(channel, "axis", exact = TRUE)
}

#' @rdname xt_axis
#' @export
xt_axis.xchan <- function(channel) {
  stop(
    "The channel axis is stored on the channel table (`xchan_tbl`), not on ",
    "individual cross-section geometry (`xchan`). Use `xt_axis(<tbl>)` ",
    "(e.g. `xt_axis(bar)` instead of `xt_axis(bar$xsection)`).",
    call. = FALSE
  )
}

#' @rdname xt_axis
#' @export
xt_axis.default <- function(channel) {
  stop(
    "`xt_axis()` expects an `xchan_tbl`. Got class(es): ",
    paste(class(channel), collapse = ", "),
    ".",
    call. = FALSE
  )
}

#' @rdname xt_axis
#' @export
`xt_axis<-` <- function(channel, value) {
  UseMethod("xt_axis<-")
}

#' @rdname xt_axis
#' @export
`xt_axis<-.xchan_tbl` <- function(channel, value) {
  checkmate::assert_class(channel, "xchan_tbl")
  if (is.null(value)) {
    attr(channel, "axis") <- NULL
    return(channel)
  }
  plan <- channel_plan(channel)
  crs_hint <- if (!is.null(plan)) sf::st_crs(plan) else NULL
  attr(channel, "axis") <- validate_axis_sf(value, crs_hint)
  channel
}

#' @rdname xt_axis
#' @export
`xt_axis<-.xchan` <- function(channel, value) {
  stop(
    "Assign the axis on the `xchan_tbl`, not on `xchan` geometry: ",
    "`xt_axis(bar) <- value`, not `xt_axis(bar$xsection) <- value`.",
    call. = FALSE
  )
}

#' @rdname xt_axis
#' @export
`xt_axis<-.default` <- function(channel, value) {
  stop(
    "`xt_axis<-()` expects an `xchan_tbl`. Got class(es): ",
    paste(class(channel), collapse = ", "),
    ".",
    call. = FALSE
  )
}

#' @noRd
validate_axis_sf <- function(x, crs_hint = NULL) {
  if (inherits(x, "sfg")) {
    x <- sf::st_sfc(x)
  }
  if (!inherits(x, "sfc")) {
    stop("`axis` must be an sf geometry (`sfc` or `sfg`).", call. = FALSE)
  }
  x <- sf::st_cast(x, "LINESTRING")
  if (length(x) != 1L) {
    stop("`axis` must be a single LINESTRING feature.", call. = FALSE)
  }
  if (
    !is.null(crs_hint) && !is.na(sf::st_crs(x)) && !is.na(sf::st_crs(crs_hint))
  ) {
    if (sf::st_crs(x) != sf::st_crs(crs_hint)) {
      warning(
        "Transforming axis to the channel plan CRS.",
        call. = FALSE
      )
      x <- sf::st_transform(x, crs_hint)
    }
  }
  x
}

#' @noRd
plan_midpoints_sfc <- function(plan) {
  n <- length(plan)
  pts <- vector("list", n)
  for (i in seq_len(n)) {
    coords <- sf::st_coordinates(plan[i])
    pts[[i]] <- sf::st_point(colMeans(rbind(
      coords[1L, 1:2],
      coords[nrow(coords), 1:2]
    )))
  }
  sf::st_sfc(pts, crs = sf::st_crs(plan))
}

#' @noRd
resolve_channel_axis <- function(channel, axis = NULL, axis_arg_name = "axis") {
  plan <- channel_plan(channel)
  crs <- sf::st_crs(plan)
  if (!is.null(axis)) {
    return(validate_axis_sf(axis, crs))
  }
  ax <- xt_axis(channel)
  if (!is.null(ax)) {
    return(validate_axis_sf(ax, crs))
  }
  stop(
    "No axis stored on `channel` and none supplied to `",
    axis_arg_name,
    "`. Set one with `xt_axis(channel) <- <LINESTRING>` or use ",
    "`xt_generate_plan()`, which stores an axis automatically.",
    call. = FALSE
  )
}
