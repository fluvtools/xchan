#' Channel axis (LINESTRING)
#'
#' Get or set the reference axis used for downstream distance and ordering:
#' [xt_trace_centerline()], [xt_arrange_downstream()], [xt_distance_ds()],
#' [xt_gradient()], etc. Channels built with [xt_generate_plan()] store the
#' sampling axis automatically.
#'
#' @param channel An object of class `"xchan"`.
#' @param value A single **LINESTRING** as `sfc` or `sfg`, same CRS as the plan
#'   column (else transformed with a warning).
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
  checkmate::assert_class(channel, "xchan")
  attr(channel, "axis", exact = TRUE)
}

#' @rdname xt_axis
#' @export
`xt_axis<-` <- function(channel, value) {
  checkmate::assert_class(channel, "xchan")
  if (is.null(value)) {
    attr(channel, "axis") <- NULL
    return(channel)
  }
  pc <- attributes(channel)$plan_col
  crs_hint <- if (!is.null(pc)) sf::st_crs(channel[[pc]]) else NULL
  attr(channel, "axis") <- validate_axis_sf(value, crs_hint)
  channel
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
  plan <- xt_column_plan(channel)
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
