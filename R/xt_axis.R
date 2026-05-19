#' Channel axis (LINESTRING)
#'
#' Get or set the reference axis used for downstream distance and ordering:
#' [xt_arrange_downstream()], [xt_distance_downstream()],
#' [xt_gradient()], etc. Channels built with [xt_generate_plan()] store the
#' sampling axis automatically on the [`xchan`] object.
#'
#' @param channel An [`xchan`] object (cross-section geometry container).
#' @param value A single **LINESTRING** as `sfc` or `sfg`, same CRS as the plan
#'   geometry (else transformed with a warning).
#'
#' @details
#' The axis is **reach-scale** geometry (one polyline along the channel). It is
#' stored as attribute `"axis"` on the [`xchan`] object.
#'
#' **What “downstream” means:** [xt_distance_downstream()], [xt_arrange_downstream()],
#' and related helpers treat **increasing distance along the stored
#' `LINESTRING`**, measured from its **first vertex to its last** (vertex
#' digitization order), as downstream. The package does **not** infer flow from
#' a DEM, network topology, or a separate flag—only from this polyline. Use
#' [xt_reverse_flow()] to reverse the axis (and flip transects) so the same
#' geographic line is traversed from the opposite end.
#'
#' **Assigning a new axis** (`xt_axis(channel) <- value`): the stored polyline’s
#' **first vertex is chainage zero** and **increasing distance along the line**
#' is the package’s downstream direction for sorting and metrics—but **local
#' left/right on each transect** still come from **planimetric encoding**: the
#' first plan vertex is the **left bank** and the last is the **right bank**,
#' defined relative to the downstream **tangent** of the axis at the station where the
#' extended bank-to-bank chord meets the axis (same cross-product rule as [xt_generate_plan()]).
#' After the axis is set, each transect is **re-oriented** if needed (end-for-end) so vertex order
#' matches that convention when the axis pierces the segment from the “wrong”
#' side, matching profiles are mirrored so profile
#' distance still increases toward the right bank, and sections are **re-sorted**
#' with [xt_arrange_downstream()] by increasing projected chainage. A highly
#' curved, U-shaped, or self-overlapping axis can still yield counter-intuitive
#' chainages; the package does not validate global monotonicity against terrain.
#'
#' @returns For `xt_axis()`, the stored `sfc_LINESTRING` or `NULL`. For
#'   assignment, an updated [`xchan`] with attribute `axis`.
#'
#' @seealso [xt_arrange_downstream()]
#' @export
#' @examples
#' \donttest{
#' library(sf)
#' ch <- xt_generate_plan(demo_bankline, n = 20)
#' ax <- xt_axis(ch)
#' plot(ax)
#' }
xt_axis <- function(channel) {
  UseMethod("xt_axis")
}

#' @rdname xt_axis
#' @export
xt_axis.xchan <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  attr(channel, "axis", exact = TRUE)
}

#' @rdname xt_axis
#' @export
xt_axis.default <- function(channel) {
  stop(
    "`xt_axis()` expects an `xchan`. Got class(es): ",
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
`xt_axis<-.xchan` <- function(channel, value) {
  checkmate::assert_class(channel, "xchan")
  if (is.null(value)) {
    attr(channel, "axis") <- NULL
    return(channel)
  }
  plan <- channel_plan(channel)
  crs_hint <- if (!is.null(plan)) sf::st_crs(plan) else NULL
  attr(channel, "axis") <- validate_axis_sf(value, crs_hint)
  if (length(channel) < 1L) {
    return(channel)
  }
  align_xchan_to_stored_axis(channel)
}

#' @rdname xt_axis
#' @export
`xt_axis<-.default` <- function(channel, value) {
  stop(
    "`xt_axis<-()` expects an `xchan`. Got class(es): ",
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
    pts[[i]] <- sf::st_geometry(transect_bank_midpoint_sfc(plan[[i]]))[[1L]]
  }
  sf::st_sfc(pts, crs = sf::st_crs(plan))
}

#' Axis through planimetric cross-section midpoints (vertex order as given).
#'
#' @noRd
axis_from_plan_midpoints <- function(plan) {
  n <- length(plan)
  if (n < 1L) {
    stop("`plan` must contain at least one transect.", call. = FALSE)
  }
  if (n == 1L) {
    seg <- plan[[1L]]
    m <- sf::st_coordinates(seg)[, 1L:2L, drop = FALSE]
    p1 <- m[1L, , drop = TRUE]
    pn <- m[nrow(m), , drop = TRUE]
    mid <- 0.5 * (p1 + pn)
    v <- pn - p1
    lv <- sqrt(sum(v^2))
    if (!is.finite(lv) || lv < 1e-15) {
      v <- c(1, 0)
    } else {
      v <- v / lv
    }
    eps <- if (is.finite(lv) && lv > 0) lv * 0.5 else 1
    xy <- rbind(mid - v * eps, mid + v * eps)
    return(sf::st_sfc(sf::st_linestring(xy), crs = sf::st_crs(plan)))
  }
  mid_pts <- plan_midpoints_sfc(plan)
  xy <- sf::st_coordinates(mid_pts)
  sf::st_sfc(sf::st_linestring(xy), crs = sf::st_crs(plan))
}

#' List positions of sections in downstream (flow) order along the axis.
#'
#' @noRd
channel_flow_order <- function(channel, axis = NULL) {
  plan <- channel_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections.", call. = FALSE)
  }
  axis_line <- resolve_channel_axis(channel, axis)
  order(plan_chainage_on_axis(plan, axis_line))
}

#' Bank-to-bank midpoint of one plan transect (`sfg` / length-one `sfc`).
#'
#' @noRd
transect_bank_midpoint_sfc <- function(seg) {
  m <- sf::st_coordinates(seg)[, 1L:2L, drop = FALSE]
  n <- nrow(m)
  if (n < 2L) {
    stop("Each plan line must have at least two coordinates.", call. = FALSE)
  }
  xy <- 0.5 * (m[1L, , drop = TRUE] + m[n, , drop = TRUE])
  sf::st_sfc(sf::st_point(xy), crs = sf::st_crs(seg))
}

#' Very long chord collinear with first–last plan vertices (extended transect).
#'
#' @noRd
extended_transect_chord_sfc <- function(seg, axis_line) {
  m <- sf::st_coordinates(seg)[, 1L:2L, drop = FALSE]
  n <- nrow(m)
  p1 <- m[1L, , drop = TRUE]
  pn <- m[n, , drop = TRUE]
  mid <- 0.5 * (p1 + pn)
  v <- pn - p1
  lv <- sqrt(sum(v^2))
  ext <- 1e8
  if (!is.null(axis_line)) {
    axl <- suppressWarnings(as.numeric(sf::st_length(axis_line)))
    if (is.finite(axl) && axl > 0) {
      ext <- max(ext, axl * 500)
    }
  }
  if (!is.finite(lv) || lv < .Machine$double.eps) {
    return(sf::st_sfc(sf::st_linestring(rbind(p1, pn)), crs = sf::st_crs(seg)))
  }
  u <- v / lv
  far1 <- mid - u * ext
  far2 <- mid + u * ext
  sf::st_sfc(sf::st_linestring(rbind(far1, far2)), crs = sf::st_crs(seg))
}

#' @noRd
intersection_xy_matrix <- function(g) {
  if (is.null(g)) {
    return(matrix(0, 0, 2))
  }
  if (!inherits(g, "sfc")) {
    g <- sf::st_sfc(g, crs = NA)
  }
  if (length(g) < 1L) {
    return(matrix(0, 0, 2))
  }
  empty <- sf::st_is_empty(g)
  if (length(empty) < 1L || isTRUE(all(empty))) {
    return(matrix(0, 0, 2))
  }
  pts <- tryCatch(
    sf::st_cast(g, "POINT"),
    error = function(e) {
      tryCatch(
        sf::st_cast(sf::st_collection_extract(g, "POINT"), "POINT"),
        error = function(e2) sf::st_sfc(crs = sf::st_crs(g))
      )
    }
  )
  if (length(pts) < 1L) {
    return(matrix(0, 0, 2))
  }
  pts_empty <- sf::st_is_empty(pts)
  if (length(pts_empty) < 1L || isTRUE(all(pts_empty))) {
    return(matrix(0, 0, 2))
  }
  sf::st_coordinates(pts)[, 1L:2L, drop = FALSE]
}

#' Point on the axis used for chainage and orientation: extended chord ∩ axis,
#' else nearest point on axis to the bank midpoint.
#'
#' @param require_intersection If `TRUE`, error when the extended chord does not
#'   meet the axis (used by `xt_axis<-` alignment).
#' @noRd
transect_axis_station_sfc <- function(
  seg,
  axis_line,
  require_intersection = FALSE
) {
  seg_sfc <- if (inherits(seg, "sfc")) seg else sf::st_sfc(seg)
  crs_ax <- sf::st_crs(axis_line)
  if (!is.na(crs_ax)) {
    if (is.na(sf::st_crs(seg_sfc))) {
      seg_sfc <- sf::st_set_crs(seg_sfc, crs_ax)
    } else if (sf::st_crs(seg_sfc) != crs_ax) {
      seg_sfc <- sf::st_transform(seg_sfc, crs_ax)
    }
  }
  crs <- sf::st_crs(seg_sfc)
  mid_pt <- transect_bank_midpoint_sfc(seg_sfc)
  ext <- extended_transect_chord_sfc(seg_sfc, axis_line)
  hit <- tryCatch(
    sf::st_intersection(sf::st_geometry(ext), sf::st_geometry(axis_line)),
    error = function(e) NULL
  )
  xy <- if (is.null(hit)) {
    matrix(0, 0, 2)
  } else {
    intersection_xy_matrix(hit)
  }
  if (nrow(xy) > 0L) {
    mid <- sf::st_coordinates(mid_pt)[1L, 1L:2, drop = TRUE]
    d2 <- (xy[, 1L] - mid[1L])^2 + (xy[, 2L] - mid[2L])^2
    j <- which.min(d2)
    return(sf::st_sfc(sf::st_point(xy[j, , drop = TRUE]), crs = crs))
  }
  if (isTRUE(require_intersection)) {
    stop(
      "The axis does not intersect at least one plan cross section ",
      "(extended bank-to-bank line). Extend or reposition the axis so it ",
      "crosses every transect along the reach.",
      call. = FALSE
    )
  }
  nearest_point_on_axis_from_mid(axis_line, mid_pt)
}

#' @noRd
nearest_point_on_axis_from_mid <- function(axis_line, mid_pt) {
  crs_ax <- sf::st_crs(axis_line)
  if (!is.na(crs_ax) && is.na(sf::st_crs(mid_pt))) {
    mid_pt <- sf::st_set_crs(mid_pt, crs_ax)
  } else if (
    !is.na(crs_ax) &&
      !is.na(sf::st_crs(mid_pt)) &&
      sf::st_crs(mid_pt) != crs_ax
  ) {
    mid_pt <- sf::st_transform(mid_pt, crs_ax)
  }
  np <- sf::st_nearest_points(axis_line, mid_pt)
  if (length(np) < 1L || sf::st_is_empty(np[[1L]])) {
    s <- as.numeric(sf::st_line_project(axis_line, mid_pt))
    return(sf::st_line_interpolate(axis_line, s))
  }
  co <- sf::st_coordinates(np[[1L]])
  sf::st_sfc(sf::st_point(co[1L, 1L:2]), crs = sf::st_crs(axis_line))
}

#' Chainage along axis (from axis start) for each plan transect.
#'
#' @noRd
plan_chainage_on_axis <- function(plan, axis_line) {
  vapply(
    seq_along(plan),
    function(i) {
      stn <- transect_axis_station_sfc(plan[[i]], axis_line)
      as.numeric(sf::st_line_project(axis_line, stn))
    },
    numeric(1)
  )
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

#' Re-orient transects to the stored axis and sort by chainage (after `xt_axis<-`)
#'
#' @noRd
align_xchan_to_stored_axis <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  ax <- xt_axis(channel)
  if (is.null(ax)) {
    return(channel)
  }
  plan <- channel_plan(channel)
  if (is.null(plan) || length(plan) < 1L) {
    return(channel)
  }
  line_length <- as.numeric(sum(sf::st_length(ax)))
  new_lines <- vector("list", length(plan))
  flip_prof <- logical(length(plan))
  for (i in seq_along(plan)) {
    seg <- plan[[i]]
    stn <- transect_axis_station_sfc(seg, ax, require_intersection = TRUE)
    t_down <- axis_unit_tangent_downstream(ax, stn, line_length)
    ori <- orient_plan_xs_left_first(seg, stn, t_down)
    new_lines[[i]] <- ori
    c0 <- as.numeric(sf::st_coordinates(seg)[1L, 1L:2])
    c1 <- as.numeric(sf::st_coordinates(ori)[1L, 1L:2])
    flip_prof[i] <- sum((c0 - c1)^2) > 1e-10
  }
  new_plan <- sf::st_sfc(new_lines, crs = sf::st_crs(plan))
  out <- xchan_with_plan(channel, new_plan)
  prof <- channel_profile(out)
  if (!is.null(prof)) {
    prof2 <- prof
    for (i in seq_along(flip_prof)) {
      if (flip_prof[i]) {
        prof2[[i]] <- flip_profile(prof2[[i]])
      }
    }
    out <- xchan_with_profile(out, prof2)
  }
  xt_arrange_downstream(out)
}
