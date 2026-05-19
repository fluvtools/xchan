#' Coerce to a channel object (`xchan`)
#'
#' Convert widths, line geometries, a list of [`xsection`] objects, or an
#' existing [`xchan`] into cross-section geometry. Width, `sfc`, and `list`
#' methods return an [`xchan`].
#'
#' @param x Object to coerce (`numeric` vector of widths, `sfc`, `list` of
#'   [`xsection`], or existing [`xchan`]).
#' @param ... Must be empty except where documented below.
#'
#' @details
#' For **`sfg`** / **`sfc`** inputs, coercion targets **planimetric-only**
#' cross sections at this stage of package development: geometries are cast to
#' **LINESTRING** bank-to-bank segments. Users should supply line geometries
#' (not polygons or points). Profile views must be attached separately (for
#' example with [xchan()] / [xsection()]).
#'
#' When coercing **`numeric`** widths without an explicit `axis`, cross sections
#' are placed on **vertical** transects (constant \eqn{x}, width in \eqn{y}) so
#' the synthetic channel runs **horizontally** along \eqn{x}. Each transect’s
#' first vertex is the **left** bank and the second the **right** bank, facing
#' downstream (increasing \eqn{x} along the default axis). Consecutive
#' stations are spaced by **twice** a reference width: twice
#' [stats::median()] of \code{x} when that value is positive, otherwise twice
#' \code{mean(x)}. If every width is zero, a reference width of \code{1} is used
#' (so consecutive stations lie 2 map units apart). The same length unit applies
#' as for \code{x} (typically metres under a projected CRS).
#'
#' @returns An [`xchan`] for `numeric`, `units`, `sfc`, `sfg`, `list`, and
#'   `xchan` methods.
#'
#' @seealso [xchan()], [xsection()]
#'
#' @examples
#' # Synthetic widths (stations spaced ~2 median widths along x by default)
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
#' @param axis Optional channel axis (`sfc`/`sfg` LINESTRING, length 1); see [xt_axis()].
#'   When `NULL`, a default axis is built (synthetic spacing for `numeric` widths;
#'   midpoints connected in section order for `sfc`, `sfg`, and `list`). For an
#'   existing [`xchan`], `NULL` leaves the stored axis unchanged.
#' @param bankline Optional bankline polygon (`sfc`/`sfg`); stored on the [`xchan`]
#'   via [xt_bankline()]. `NULL` leaves any existing footprint unchanged.
#' @param crs For `numeric`, `sfc`, and `list` methods:
#'   CRS applied to plan geometries via [sf::st_set_crs()]. `NULL` leaves
#'   existing CRS unchanged (for `list`, sets the container CRS on the [`xchan`]).
#' @export
xt_as_channel.numeric <- function(x, ..., crs = NULL, axis = NULL, bankline = NULL) {
  rlang::check_dots_empty()
  checkmate::assert_numeric(x, lower = 0, any.missing = FALSE)
  spacing_default <- default_numeric_channel_spacing(x)
  n <- length(x)
  station <- if (is.null(axis)) (seq_len(n) - 1L) * spacing_default else NULL

  if (is.null(axis)) {
    plan <- sf::st_sfc(Map(
      function(w, x_coord) {
        sf::st_linestring(matrix(
          c(x_coord, w / 2, x_coord, -w / 2),
          ncol = 2L,
          byrow = TRUE
        ))
      },
      x,
      station
    ))
    if (!is.null(crs)) {
      plan <- sf::st_set_crs(plan, crs)
    }
    if (length(station) == 1L) {
      axis_x <- c(station[1L] - spacing_default / 2, station[1L] + spacing_default / 2)
    } else {
      axis_x <- station
    }
    axis_obj <- sf::st_sfc(
      sf::st_linestring(cbind(axis_x, rep(0, length(axis_x)))),
      crs = sf::st_crs(plan)
    )
  } else {
    axis_obj <- validate_axis_sf(axis, if (!is.null(crs)) crs else NULL)
    if (!is.null(crs)) {
      axis_obj <- sf::st_set_crs(axis_obj, crs)
    }
    plan <- build_plan_from_widths_axis(x, axis_obj)
  }

  xsec <- xchan_from_plan_profile(plan, NULL)
  xsec <- finalize_xt_as_channel(xsec, crs = sf::st_crs(plan), axis = axis_obj, bankline = bankline)
  validate_plan_profile_widths(xsec)
  xsec
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.units <- function(x, ..., crs = NULL, axis = NULL, bankline = NULL) {
  rlang::check_dots_empty()
  manual_unit <- if (is.null(crs)) units_deparse(x) else NULL
  unit <- if (!is.null(crs)) crs_length_unit(crs) else manual_unit
  x <- to_numeric_length(x, unit, arg = "x")
  out <- xt_as_channel(x, ..., crs = crs, axis = axis, bankline = bankline)
  set_xchan_length_unit(out, manual_unit)
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.sfg <- function(x, ..., crs = NULL, axis = NULL, bankline = NULL) {
  rlang::check_dots_empty()
  xt_as_channel(sf::st_sfc(x), ..., crs = crs, axis = axis, bankline = bankline)
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.sfc <- function(x, ..., crs = NULL, axis = NULL, bankline = NULL) {
  rlang::check_dots_empty()
  if (!is.null(crs)) {
    x <- sf::st_set_crs(x, crs)
  }
  if (!inherits(x, "sfc_LINESTRING")) {
    x <- sf::st_cast(x, "LINESTRING")
  }

  xsec <- xchan_from_plan_profile(x, NULL)
  axis_obj <- if (!is.null(axis)) {
    validate_axis_sf(axis, sf::st_crs(x))
  } else {
    axis_from_plan_midpoints(x)
  }
  xsec <- finalize_xt_as_channel(xsec, crs = sf::st_crs(x), axis = axis_obj, bankline = bankline)
  validate_plan_profile_widths(xsec)
  xsec
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.list <- function(x, ..., crs = NULL, axis = NULL, bankline = NULL) {
  rlang::check_dots_empty()
  if (length(x) == 0L) {
    out <- xchan(list(), crs = crs, axis = NULL)
    if (!is.null(axis)) {
      stop(
        "`axis` cannot be set when coercing an empty list of cross sections.",
        call. = FALSE
      )
    }
    return(out)
  }
  bad <- !vapply(x, is_xsection, logical(1L))
  if (any(bad)) {
    stop(
      "Each list element must be an `xsection` object. Bad indices: ",
      paste(which(bad), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  assert_section_profiles_homogeneous(x)
  out <- xchan(x, crs = crs, axis = NULL)
  plan <- channel_plan(out)
  crs_hint <- if (!is.null(plan)) sf::st_crs(plan) else if (!is.null(crs)) sf::st_crs(crs) else NULL
  axis_obj <- if (!is.null(axis)) {
    validate_axis_sf(axis, crs_hint)
  } else if (!is.null(plan) && length(plan) > 0L) {
    axis_from_plan_midpoints(plan)
  } else {
    NULL
  }
  out <- finalize_xt_as_channel(out, crs = NULL, axis = axis_obj, bankline = bankline)
  validate_plan_profile_widths(out)
  out
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.xchan <- function(x, ..., crs = NULL, axis = NULL, bankline = NULL) {
  rlang::check_dots_empty()
  if (!is.null(crs)) {
    x <- `xchan_crs<-`(x, crs)
  }
  if (!is.null(axis)) {
    plan <- channel_plan(x)
    crs_hint <- if (!is.null(plan)) sf::st_crs(plan) else attr(x, "crs")
    attr(x, "axis") <- validate_axis_sf(axis, crs_hint)
  }
  if (!is.null(bankline)) {
    x <- `xt_bankline<-`(x, bankline)
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
finalize_xt_as_channel <- function(xsec, crs = NULL, axis = NULL, bankline = NULL) {
  if (!is.null(crs)) {
    xsec <- `xchan_crs<-`(xsec, crs)
  }
  if (!is.null(axis)) {
    attr(xsec, "axis") <- axis
  }
  if (!is.null(bankline)) {
    xsec <- `xt_bankline<-`(xsec, bankline)
  }
  xsec
}

#' @noRd
default_numeric_channel_spacing <- function(x) {
  checkmate::assert_numeric(x, lower = 0, any.missing = FALSE, min.len = 1L)
  w_ref <- stats::median(x)
  if (!is.finite(w_ref) || w_ref <= 0) {
    w_ref <- mean(x)
  }
  if (!is.finite(w_ref) || w_ref <= 0) {
    w_ref <- 1
  }
  2 * w_ref
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
