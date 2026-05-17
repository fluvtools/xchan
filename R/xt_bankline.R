#' Channel bank footprint (polygon)
#'
#' Get or set the plan-view channel footprint stored on an [`xchan`]. When
#' present (for example after [xt_generate_plan()]), it can be drawn under
#' transects in [plot.xchan()].
#'
#' @param channel An [`xchan`] object.
#' @param value `NULL` clears the footprint. Otherwise an `sfc` or `sfg` with
#'   only **POLYGON** / **MULTIPOLYGON** geometries, in the same CRS as the plan
#'   (or transformed with a warning).
#'
#' @details
#' The footprint is stored as attribute `"bankline"` on the [`xchan`].
#'
#' @returns For `xt_bankline()`, the stored `sfc` or `NULL`. For assignment, an
#'   updated [`xchan`] with attribute `bankline`.
#'
#' @seealso [xt_generate_plan()], [xt_axis()]
#' @export
#' @examples
#' \donttest{
#' library(sf)
#' ch <- xt_generate_plan(fraser_bankline, n = 12)
#' bl <- xt_bankline(ch)
#' plot(bl)
#' }
xt_bankline <- function(channel) {
  UseMethod("xt_bankline")
}

#' @rdname xt_bankline
#' @export
xt_bankline.xchan <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  attr(channel, "bankline", exact = TRUE)
}

#' @rdname xt_bankline
#' @export
xt_bankline.default <- function(channel) {
  stop(
    "`xt_bankline()` expects an `xchan`. Got class(es): ",
    paste(class(channel), collapse = ", "),
    ".",
    call. = FALSE
  )
}

#' @rdname xt_bankline
#' @export
`xt_bankline<-` <- function(channel, value) {
  UseMethod("xt_bankline<-")
}

#' @rdname xt_bankline
#' @export
`xt_bankline<-.xchan` <- function(channel, value) {
  checkmate::assert_class(channel, "xchan")
  if (is.null(value)) {
    attr(channel, "bankline") <- NULL
    return(channel)
  }
  plan <- channel_plan(channel)
  crs_hint <- if (!is.null(plan)) sf::st_crs(plan) else NULL
  attr(channel, "bankline") <- validate_bankline_sf(value, crs_hint)
  channel
}

#' @rdname xt_bankline
#' @export
`xt_bankline<-.default` <- function(channel, value) {
  stop(
    "`xt_bankline<-()` expects an `xchan`. Got class(es): ",
    paste(class(channel), collapse = ", "),
    ".",
    call. = FALSE
  )
}

#' @noRd
validate_bankline_sf <- function(x, crs_hint = NULL) {
  if (inherits(x, "sfg")) {
    x <- sf::st_sfc(x)
  }
  if (!inherits(x, "sfc")) {
    stop("`bankline` must be polygon `sfc` or `sfg`.", call. = FALSE)
  }
  gt <- unique(as.character(sf::st_geometry_type(x, by_geometry = TRUE)))
  ok <- gt %in% c("POLYGON", "MULTIPOLYGON")
  if (length(gt) == 0L || !all(ok)) {
    stop(
      "`bankline` must contain only POLYGON or MULTIPOLYGON geometries.",
      call. = FALSE
    )
  }
  if (
    !is.null(crs_hint) && !is.na(sf::st_crs(crs_hint)) &&
      !is.na(sf::st_crs(x))
  ) {
    if (sf::st_crs(x) != sf::st_crs(crs_hint)) {
      warning(
        "Transforming bankline to the channel plan CRS.",
        call. = FALSE
      )
      x <- sf::st_transform(x, crs_hint)
    }
  }
  x
}
