#' Coerce to a channel object (`xchan`)
#'
#' Convert widths, line geometries, or an existing [`xchan`] into
#' cross-section geometry. Width and `sfc` methods return an [`xchan`].
#'
#' @param x Object to coerce (`numeric` vector of widths, `sfc`, or existing
#'   [`xchan`]).
#' @param ... Must be empty except where documented below.
#'
#' @returns An [`xchan`] for `numeric`, `units`, `sfc`, `sfg`, and `xchan` methods.
#'
#' @seealso [xchan()], [xsection()]
#'
#' @examples
#' # Synthetic widths (integer positions along the channel)
#' xt_as_channel(c(10, 15, 12, 8))
#'
#' library(sf)
#' seg <- st_sfc(
#'   st_linestring(matrix(c(-0.2, 0.3, 0.2, 1), nrow = 2, byrow = TRUE)),
#'   st_linestring(matrix(c(0.1, 0.1, 1, 1), nrow = 2, byrow = TRUE)),
#'   crs = 3005
#' )
#' xt_as_channel(seg)
#'
#' @export
xt_as_channel <- function(x, ...) {
  UseMethod("xt_as_channel")
}

#' @rdname xt_as_channel
#' @param profile Optional list of `xs_profile` objects (same length as plan).
#' @param axis Optional channel axis (`sfc`/`sfg` LINESTRING, length 1); see [xt_axis()].
#'   Used when coercing from `numeric`, `sfc`, or `sfg`.
#' @param spacing Single positive value used by the `numeric` method only when
#'   `axis` is `NULL`: spacing between consecutive synthetic cross sections.
#'   Plain numeric is interpreted in the supplied `crs`'s length unit (or
#'   unitless when `crs` is `NULL`); a [units::units()] length object is
#'   converted automatically. If both `axis` and `spacing` are supplied, an
#'   error is raised. When `axis` is `NULL` and `spacing` is omitted,
#'   `spacing = 1` is used.
#' @param crs For `numeric` and `sfc` methods:
#'   CRS applied to plan geometries via [sf::st_set_crs()]. `NULL` leaves
#'   existing CRS unchanged.
#' @export
xt_as_channel.numeric <- function(
  x,
  ...,
  profile = NULL,
  crs = NULL,
  axis = NULL,
  spacing = NULL
) {
  rlang::check_dots_empty()
  checkmate::assert_numeric(x, lower = 0, any.missing = FALSE)
  spacing_supplied <- !missing(spacing) && !is.null(spacing)
  if (!is.null(axis) && spacing_supplied) {
    stop("`spacing` cannot be supplied when `axis` is provided.", call. = FALSE)
  }
  unit <- if (!is.null(crs)) crs_length_unit(crs) else NULL
  if (is.null(axis)) {
    if (is.null(spacing)) {
      spacing <- 1
    } else {
      spacing <- to_numeric_length(spacing, unit, arg = "spacing")
    }
    checkmate::assert_number(spacing, lower = .Machine$double.eps, finite = TRUE)
  }
  n <- length(x)
  station <- if (is.null(axis)) (seq_len(n) - 1) * spacing else NULL

  if (is.null(axis)) {
    plan <- sf::st_sfc(Map(
      function(w, y) {
        sf::st_linestring(matrix(c(-w / 2, w / 2, y, y), ncol = 2))
      },
      x,
      station
    ))
    if (!is.null(crs)) {
      plan <- sf::st_set_crs(plan, crs)
    }
    if (length(station) == 1L) {
      axis_y <- c(station[1] - spacing / 2, station[1] + spacing / 2)
    } else {
      axis_y <- station
    }
    axis_obj <- sf::st_sfc(
      sf::st_linestring(cbind(rep(0, length(axis_y)), axis_y)),
      crs = sf::st_crs(plan)
    )
  } else {
    axis_obj <- validate_axis_sf(axis, if (!is.null(crs)) crs else NULL)
    if (!is.null(crs)) {
      axis_obj <- sf::st_set_crs(axis_obj, crs)
    }
    plan <- build_plan_from_widths_axis(x, axis_obj)
  }

  xsec <- xchan_from_plan_profile(plan, profile)
  xsec <- `xchan_crs<-`(xsec, sf::st_crs(plan))
  attr(xsec, "axis") <- axis_obj
  validate_plan_profile_widths(xsec)
  xsec
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.units <- function(
  x,
  ...,
  profile = NULL,
  crs = NULL,
  axis = NULL,
  spacing = NULL
) {
  rlang::check_dots_empty()
  unit <- if (!is.null(crs)) crs_length_unit(crs) else NULL
  x <- to_numeric_length(x, unit, arg = "x")
  xt_as_channel(
    x,
    ...,
    profile = profile,
    crs = crs,
    axis = axis,
    spacing = spacing
  )
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.sfg <- function(x, ..., profile = NULL, crs = NULL, axis = NULL) {
  rlang::check_dots_empty()
  xt_as_channel(sf::st_sfc(x), profile = profile, crs = crs, axis = axis)
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.sfc <- function(x, ..., profile = NULL, crs = NULL, axis = NULL) {
  rlang::check_dots_empty()
  if (!is.null(crs)) {
    x <- sf::st_set_crs(x, crs)
  }
  if (!inherits(x, "sfc_LINESTRING")) {
    x <- sf::st_cast(x, "LINESTRING")
  }

  xsec <- xchan_from_plan_profile(x, profile)
  xsec <- `xchan_crs<-`(xsec, sf::st_crs(x))
  axis_obj <- if (!is.null(axis)) validate_axis_sf(axis, sf::st_crs(x)) else NULL
  if (!is.null(axis_obj)) {
    attr(xsec, "axis") <- axis_obj
  }
  validate_plan_profile_widths(xsec)
  xsec
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.xchan <- function(x, ..., crs = NULL) {
  rlang::check_dots_empty()
  if (!is.null(crs)) {
    x <- `xchan_crs<-`(x, crs)
  }
  validate_plan_profile_widths(x)
  x
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.default <- function(x, ...) {
  rlang::check_dots_empty()
  stop(
    "Can't coerce an object of class ",
    paste(class(x), collapse = "/"),
    " to a channel; use `xt_as_channel()` or define an S3 method.",
    call. = FALSE
  )
}

#' @noRd
build_plan_from_widths_axis <- function(widths, axis) {
  n <- length(widths)
  axis_len <- as.numeric(sf::st_length(axis))
  stations <- if (n == 1L) axis_len / 2 else seq(0, axis_len, length.out = n)
  eps <- max(axis_len * 1e-8, 1e-6)

  segs <- vector("list", n)
  for (i in seq_len(n)) {
    s <- stations[i]
    p <- sf::st_line_interpolate(axis, s)
    s0 <- max(0, s - eps)
    s1 <- min(axis_len, s + eps)
    if (s1 <= s0) {
      s0 <- max(0, s - axis_len * 1e-8)
      s1 <- min(axis_len, s + axis_len * 1e-8)
    }
    p0 <- sf::st_line_interpolate(axis, s0)
    p1 <- sf::st_line_interpolate(axis, s1)
    c0 <- sf::st_coordinates(p0)[1L, 1:2, drop = TRUE]
    c1 <- sf::st_coordinates(p1)[1L, 1:2, drop = TRUE]
    t <- c1 - c0
    nrm <- sqrt(sum(t^2))
    if (nrm < 1e-15) {
      t <- c(0, 1)
    } else {
      t <- t / nrm
    }
    left_n <- c(-t[2], t[1])
    ctr <- sf::st_coordinates(p)[1L, 1:2, drop = TRUE]
    half_w <- widths[i] / 2
    left <- ctr + half_w * left_n
    right <- ctr - half_w * left_n
    segs[[i]] <- sf::st_linestring(rbind(left, right))
  }
  sf::st_sfc(segs, crs = sf::st_crs(axis))
}
