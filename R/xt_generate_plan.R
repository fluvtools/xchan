#' Generate channel object from banklines
#'
#' Generate a channel object with planimetric cross sections from a
#' single closed polygon outlining the channel footprint (both banks).
#'
#' @param banks Channel footprint as **one closed polygon** (`POLYGON` or
#'   `MULTIPOLYGON`), typically as [`sf::st_sf()`], [`sf::st_sfc()`], or bare
#'   [`sf::st_geometry()`]. The ring(s) enclose the plan-view channel area; left
#'   and right banks are **not** supplied as separate inputs—they are inferred
#'   from this boundary together with the sampling axis (see **Details**).
#'
#'   **Not supported as `banks`:** two independent open bank polylines (e.g. one
#'   \code{LINESTRING} per bank without closing the corridor). Automatic axis
#'   generation uses [`centerline::cnt_path_guess()`], which expects polygon
#'   geometry; bank-to-bank segments are found by intersecting trial transects
#'   with this closed boundary.
#' @param ... Additional arguments (ignored).
#' @param n Number of cross sections to generate (mutually exclusive with
#'   spacing and at)
#' @param spacing Distance between cross sections (mutually exclusive with `n`
#'   and `at`). Plain numeric is interpreted in the `banks` CRS length unit; a
#'   [units::units()] length object is converted automatically.
#' @param at Specific distances along the channel axis for cross sections
#'   (mutually exclusive with `n` and `spacing`). Same units treatment as
#'   `spacing`.
#' @param axis Channel axis as a multilinestring: the line along which cross
#'   sections are placed. If `NULL` (the default), an axis is generated
#'   automatically from the **island-free** footprint (\code{polygon_sans_holes()})
#'   using the **centerline** package (`centerline::cnt_path_guess()`).
#' @param progress Logical; if `TRUE`, display a text progress bar while
#'   generating planimetric cross sections.
#' @returns An [`xchan`] with one [`xsection`] per list position, in downstream order along the sampling axis.
#'   Cross-section identity keys are **not** set; use [xt_section_id()] if you need stable
#'   keys (for example when joining tabular profiles with [xt_add_profile()]).
#'   After subsetting, restore order with [xt_arrange_downstream()]. Use [xt_distance_downstream()]
#'   for distance along the axis from its start to each section (requires the axis
#'   from [xt_axis()], which this function sets). The sampling axis is stored on the
#'   [`xchan`] object, and the **bank footprint** polygon(s) from `banks` on
#'   [xt_bankline()] (for plan plotting).
#' @details **Bank geometry:** Supply the channel as one polygon (or
#' multipolygon) so its boundary is a closed loop around the wetted/plan
#' corridor. If you only have two bank polylines, convert them to a closed
#' polygon (e.g. connect upstream/downstream ends) before calling this function.
#'
#' Interior rings (islands) are dropped with \code{polygon_sans_holes()} for axis
#' generation (when `axis` is `NULL`) and for the minimum-width transect search:
#' each station gets the **shortest bank-to-bank segment** through that point on
#' the filled corridor, matching the original algorithm and ignoring islands by
#' construction.
#'
#' The holed footprint (`banks`) is then used to **refine** that transect:
#' the filled chord is intersected with the polygon boundary (outer bank plus
#' island outlines). Distinct intersection points are merged with the chord
#' endpoints, ordered along the transect, and deduplicated, producing a plan
#' `LINESTRING` with **two or more** vertices (extra vertices where the transect
#' meets island banks). Relative distances for profiles still use the chord from
#' **first to last** vertex (\code{transect_xy_from_relative()}).
#'
#' To define the spacing of the cross sections, a channel axis is
#' first calculated, and equally spaced points are sampled along that
#' axis. Cross sections are calculated at these points.
#'
#' **Downstream** is the direction of increasing distance along that axis
#' (the same direction used when stations are sorted by
#' [sf::st_line_project()]). If you supply `axis`, downstream follows the
#' storage order and digitization of that line; if the axis is generated
#' automatically, downstream follows the geometry returned by the centerline
#' routine.
#'
#' **Left and right bank** mean left and right when standing at the station on
#' the axis and **facing downstream**, in the map plane of the CRS (planar
#' coordinates).
#'
#' **Vertex order (planimetric convention):** each output is a `LINESTRING` from
#' left bank to right bank: the **first** coordinate is on the left bank, the
#' **last** on the right bank (with optional intermediate vertices on island
#' banks). Increasing distance along the polyline (first to last vertex) matches
#' profile cross sections where distance increases from left to right along the
#' overall chord.
#'
#' **Terminal stations:** Cross sections at the upstream and/or downstream ends
#' of the axis are sometimes awkward (very short transects, odd intersections
#' with the bank polygon, or mouth artifacts). Inspect the result and **subset**
#' the returned [`xchan`] (single-bracket indexing; see \code{\link{xchan}}) to
#' drop the first and/or last rows if you do not want those sections in later
#' analysis or plotting.
#'
#' **How orientation is computed:** for each station we take a unit tangent to
#' the axis pointing downstream (`axis_unit_tangent_downstream()`). For each
#' endpoint of the bank-to-bank segment we form the vector from the station to
#' that endpoint and compute the 2D scalar cross product with the tangent,
#' \eqn{D_x (E_y - C_y) - D_y (E_x - C_x)}{D_x*(E_y-C_y) - D_y*(E_x-C_x)}
#' where \eqn{(D_x,D_y)} is the tangent and \eqn{(C_x,C_y)} / \eqn{(E_x,E_y)}
#' are the station and an endpoint. Under the usual planar orientation, the
#' endpoint with the **larger** value lies on the **left** bank. If that is not
#' already the first vertex, the segment is reversed with [sf::st_reverse()]
#' (`orient_plan_xs_left_first()`). This does not require splitting the bank
#' polygon by the axis.
#' @examples
#' bl <- sf::st_sfc(demo_bankline, crs = 3005)
#' channel <- xt_generate_plan(bl, n = 100)
#'
#' # With a custom axis (e.g. user-defined line along the channel)
#' # channel <- xt_generate_plan(bl, n = 100, axis = my_axis)
#' @export
xt_generate_plan <- function(
  banks,
  ...,
  n,
  spacing,
  at,
  axis = NULL,
  progress = FALSE
) {
  rlang::check_dots_empty()
  if (!rlang::is_bool(progress)) {
    stop("`progress` must be TRUE or FALSE.")
  }

  # Validate input parameters: exactly one of n, spacing, or at is required.
  n_specified <- !missing(n)
  spacing_specified <- !missing(spacing)
  at_specified <- !missing(at)

  if (sum(n_specified, spacing_specified, at_specified) != 1) {
    stop("Exactly one of n, spacing, or at must be specified.")
  }

  banks_filled <- polygon_sans_holes(banks)

  if (is.null(axis)) {
    cl <- banks_to_centerline(banks_filled)
  } else {
    cl <- axis
  }

  len <- as.numeric(sum(sf::st_length(cl)))

  # Strip units from length-bearing inputs into the CRS length unit so the
  # density / sample arithmetic below stays plain numeric.
  unit <- crs_length_unit(banks)
  if (spacing_specified) {
    spacing <- to_numeric_length(spacing, unit, arg = "spacing")
  }
  if (at_specified) {
    at <- to_numeric_length(at, unit, arg = "at")
  }

  # Determine sampling points based on input parameters
  if (n_specified) {
    pts <- sf::st_line_sample(cl, density = n / len)
  } else if (spacing_specified) {
    pts <- sf::st_line_sample(cl, density = 1 / spacing)
  } else if (at_specified) {
    pts <- sf::st_line_sample(cl, sample = at / len)
  }

  # Only take points that are not empty, and split apart multipoints
  # into individual points.
  pts <- pts[!vapply(pts, sf::st_is_empty, logical(1))]
  pts <- sf::st_cast(pts, "POINT")

  # Sort pts in order along the axis. This is important so that neighbouring
  # cross sections can be later ensured not to cross.
  dists_raw <- as.numeric(sf::st_line_project(cl, pts))
  ord_st <- order(dists_raw)
  pts <- pts[ord_st]

  # Get maximum distance based on bounding box
  bb <- sf::st_bbox(banks)
  maxd <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2
  )

  xs <- list()
  n_pts <- length(pts)
  pb <- NULL
  if (progress && n_pts > 0) {
    pb <- utils::txtProgressBar(min = 0, max = n_pts, style = 3)
    on.exit(close(pb), add = TRUE)
  }
  for (i in seq_along(pts)) {
    calc_width <- function(angle) {
      seg <- span_banks_engine(
        pts[i],
        angle,
        bankline = banks_filled,
        maxd = maxd,
        intersect = TRUE,
        reposition = FALSE
      )
      as.numeric(sf::st_length(seg))
    }

    angles <- seq(0, pi, length.out = 10)
    widths <- vapply(angles, calc_width, numeric(1))
    i_min <- which(widths == min(widths))
    if (length(i_min) > 1) {
      angles <- seq(0, pi, length.out = 100)
      widths <- vapply(angles, calc_width, numeric(1))
      i_min <- which(widths == min(widths))[1]
    }
    delta <- pi / (length(angles) - 1)
    rng <- angles[i_min] + c(-delta, delta)
    res <- stats::optimize(calc_width, rng)$minimum

    chord_filled <- span_banks_engine(
      pts[i],
      res,
      bankline = banks_filled,
      maxd = maxd,
      intersect = TRUE,
      reposition = TRUE
    )[[1]]
    xs[[i]] <- transect_refine_with_island_boundaries(chord_filled, banks)
    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  ## Combine list of segments in xs into a single sf geometry
  geoms <- sf::st_as_sfc(xs)
  sf::st_crs(geoms) <- sf::st_crs(banks)

  for (i in seq_along(pts)) {
    t_down <- axis_unit_tangent_downstream(cl, pts[i], len)
    geoms[i] <- orient_plan_xs_left_first(geoms[i], pts[i], t_down)
  }

  attr(geoms, "left_to_right") <- TRUE

  # Sampling axis for downstream order and distance geometry (see xt_distance_downstream)
  out <- xt_as_channel(geoms)
  xt_axis(out) <- cl
  bl <- sf::st_geometry(banks)
  if (!inherits(bl, "sfc")) {
    bl <- sf::st_as_sfc(list(bl))
  }
  plan_crs <- sf::st_crs(geoms)
  if (!is.na(plan_crs)) {
    if (is.na(sf::st_crs(bl))) {
      bl <- sf::st_set_crs(bl, plan_crs)
    } else if (sf::st_crs(bl) != plan_crs) {
      bl <- sf::st_transform(bl, plan_crs)
    }
  }
  xt_bankline(out) <- bl
  xt_section_id(out) <- NULL
  out
}

#' Drop interior rings (islands) from polygon geometry; outer shells only.
#'
#' @param banks `sfc` or `sf` with `POLYGON` / `MULTIPOLYGON` features.
#' @returns `sfc` of the same structure with only the first ring of each polygon
#'   part kept.
#' @noRd
polygon_sans_holes <- function(banks) {
  g <- sf::st_geometry(banks)
  crs <- sf::st_crs(g)
  g <- sf::st_make_valid(g)
  out <- vector("list", length(g))
  for (i in seq_along(g)) {
    out[[i]] <- polygon_sans_holes_one(g[[i]])
  }
  sf::st_sfc(out, crs = crs)
}

polygon_sans_holes_one <- function(sfg) {
  if (inherits(sfg, "POLYGON")) {
    sf::st_polygon(list(sfg[[1]]))
  } else if (inherits(sfg, "MULTIPOLYGON")) {
    parts <- vector("list", length(sfg))
    for (j in seq_along(sfg)) {
      poly_j <- sfg[[j]]
      parts[[j]] <- sf::st_polygon(list(poly_j[[1]]))
    }
    sf::st_multipolygon(parts)
  } else {
    stop(
      "Expected POLYGON or MULTIPOLYGON in channel footprint (got ",
      paste(class(sfg), collapse = ", "),
      ").",
      call. = FALSE
    )
  }
}

#' Refine filled chord with holed footprint: add vertices where transect meets
#' island (hole) boundaries.
#'
#' @noRd
transect_refine_with_island_boundaries <- function(chord_filled, banks_holed) {
  crs <- sf::st_crs(banks_holed)
  chord <- if (inherits(chord_filled, "sfg")) {
    sf::st_sfc(chord_filled, crs = crs)
  } else {
    sf::st_crs(chord_filled) <- crs
    chord_filled
  }
  banks_geom <- sf::st_make_valid(sf::st_geometry(banks_holed))
  chord <- sf::st_make_valid(chord)

  co <- sf::st_coordinates(chord)[, 1:2, drop = FALSE]
  if (nrow(co) < 2L) {
    stop("Transect chord must have at least two vertices.", call. = FALSE)
  }
  start <- co[1L, , drop = TRUE]
  end <- co[nrow(co), , drop = TRUE]
  dvec <- end - start
  chord_len <- sqrt(sum(dvec^2))
  if (chord_len < .Machine$double.eps) {
    stop("Zero-length transect chord.", call. = FALSE)
  }
  u <- dvec / chord_len

  proj_along <- function(xy) {
    sum((xy - start) * u)
  }

  bd <- sf::st_boundary(banks_geom)
  hit <- tryCatch(
    sf::st_intersection(chord, bd),
    error = function(e) NULL
  )

  xy_bd <- matrix(numeric(0), 0, 2)
  if (!is.null(hit) && length(hit) > 0L && !all(sf::st_is_empty(hit))) {
    hc <- sf::st_coordinates(hit)
    if (nrow(hc) > 0L) {
      xy_bd <- hc[, 1:2, drop = FALSE]
    }
  }

  mat <- rbind(co, xy_bd)
  tvals <- apply(mat, 1L, function(r) proj_along(r))
  keep <- tvals >= -1e-5 & tvals <= chord_len + 1e-5
  mat <- mat[keep, , drop = FALSE]
  tvals <- tvals[keep]

  ord <- order(tvals)
  mat <- mat[ord, , drop = FALSE]
  tvals <- tvals[ord]

  tq <- round(tvals, 8)
  keep_u <- !duplicated(tq)
  mat <- mat[keep_u, , drop = FALSE]

  if (nrow(mat) < 2L) {
    chord_filled
  } else {
    sf::st_linestring(mat)
  }
}

#' Unit tangent along the channel axis, downstream
#'
#' Approximates the downstream direction on `cl` at `pt` by taking a
#' short finite difference between two nearby interpolations along the line.
#' Used to define left/right relative to flow when orienting cross sections.
#' The full downstream / left–right convention is documented under **Details**
#' in [xt_generate_plan()].
#'
#' @param cl Channel axis as `sfc` LINESTRING (or compatible).
#' @param pt `sfc`/`sfg` POINT where the tangent is evaluated (typically a cross
#'   section station on `cl`).
#' @param line_length Total length of `cl` (same units as coordinates); passed so
#'   the finite-difference step scales safely near endpoints.
#' @returns Numeric vector of length two `(dx, dy)` with Euclidean norm 1, or
#'   `(1, 0)` if the tangent is degenerate.
#' @noRd
axis_unit_tangent_downstream <- function(cl, pt, line_length) {
  s <- as.numeric(sf::st_line_project(cl, pt))
  eps <- max(line_length * 1e-10, 1e-4)
  s0 <- max(0, s - eps)
  s1 <- min(line_length, s + eps)
  if (s1 <= s0) {
    s0 <- max(0, s - line_length * 1e-8)
    s1 <- min(line_length, s + line_length * 1e-8)
  }
  p0 <- sf::st_line_interpolate(cl, s0)
  p1 <- sf::st_line_interpolate(cl, s1)
  m0 <- sf::st_coordinates(p0)[1L, 1:2, drop = TRUE]
  m1 <- sf::st_coordinates(p1)[1L, 1:2, drop = TRUE]
  v <- m1 - m0
  nrm <- sqrt(sum(v^2))
  if (nrm < 1e-15) {
    return(c(1, 0))
  }
  v / nrm
}

#' Orient a planimetric cross section so the left bank is first
#'
#' Uses the 2D scalar cross product `tangent_x * vy - tangent_y * vx` from the
#' station to each endpoint. Under the usual map orientation, the endpoint with
#' the larger value lies on the left when facing downstream along `tangent`. If
#' the first vertex is not that endpoint, the line is reversed with
#' [sf::st_reverse()].
#' See **Details** in [xt_generate_plan()] for the convention and formula.
#'
#' @param seg Bank-to-bank segment: `LINESTRING` `sfg` or length-one `sfc`.
#' @param station_sf Station on the axis (`POINT`), same CRS as `seg`.
#' @param tangent Unit downstream tangent from `axis_unit_tangent_downstream()`.
#' @returns `seg`, possibly reversed so vertex order is left bank then right bank.
#' @noRd
orient_plan_xs_left_first <- function(seg, station_sf, tangent) {
  m <- sf::st_coordinates(seg)
  if (nrow(m) < 2L) {
    return(seg)
  }
  cs <- sf::st_coordinates(station_sf)[1L, , drop = FALSE]
  ctr <- c(cs[1L, 1L], cs[1L, 2L])
  e1 <- m[1L, 1:2]
  e2 <- m[nrow(m), 1:2]
  cross1 <- tangent[1L] * (e1[2L] - ctr[2L]) - tangent[2L] * (e1[1L] - ctr[1L])
  cross2 <- tangent[1L] * (e2[2L] - ctr[2L]) - tangent[2L] * (e2[1L] - ctr[1L])
  if (cross1 < cross2) {
    sf::st_reverse(seg)
  } else {
    seg
  }
}
