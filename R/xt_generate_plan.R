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
#' bl <- sf::st_sfc(Squamish_bankline, crs = 3005)
#' channel <- xt_generate_plan(bl, n = 20)
#'
#' # With a custom axis (e.g. user-defined line along the channel)
#' # channel <- xt_generate_plan(bl, n = 20, axis = my_axis)
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

  bank_chains <- plan_bank_boundary_chains(banks_filled, cl)
  chain_tol <- plan_bank_chain_tolerance(banks_filled)
  len_axis <- as.numeric(sum(sf::st_length(cl)))
  banks_centroid <- sf::st_coordinates(
    sf::st_centroid(sf::st_geometry(banks_filled))
  )[1L, 1:2, drop = TRUE]

  xs <- list()
  n_pts <- length(pts)
  pb <- NULL
  if (progress && n_pts > 0) {
    pb <- utils::txtProgressBar(min = 0, max = n_pts, style = 3)
    on.exit(close(pb), add = TRUE)
  }
  for (i in seq_along(pts)) {
    ctx <- station_transect_context(
      pts[i],
      cl,
      len_axis,
      banks_centroid,
      chain_tol
    )
    chord_filled <- minimum_width_opposite_bank_transect(
      pts[i],
      banks_filled,
      maxd,
      bank_chains,
      ctx
    )
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

#' Tolerance for collapsing along-transect breakpoint duplicates.
#'
#' @noRd
transect_breakpoint_tolerance <- function(chord_len) {
  max(1e-8, chord_len * 1e-8)
}

#' Collapse sorted along-transect distances within a tolerance.
#'
#' @noRd
collapse_sorted_tvals <- function(tvals, tol) {
  n <- length(tvals)
  if (n <= 1L) {
    return(tvals)
  }
  out <- numeric(0)
  i <- 1L
  while (i <= n) {
    j <- i
    while (j < n && (tvals[j + 1L] - tvals[j]) <= tol) {
      j <- j + 1L
    }
    out <- c(out, mean(tvals[i:j]))
    i <- j + 1L
  }
  out
}

#' Keep only breakpoints where wet/dry state changes along the transect.
#'
#' @noRd
transect_transition_tvals <- function(tvals, start, u, banks_geom, crs) {
  if (length(tvals) < 2L) {
    return(tvals)
  }
  mid_t <- (tvals[-1L] + tvals[-length(tvals)]) / 2
  mid_xy <- cbind(
    start[1L] + mid_t * u[1L],
    start[2L] + mid_t * u[2L]
  )
  mid_pts <- sf::st_sfc(
    lapply(seq_len(nrow(mid_xy)), function(i) sf::st_point(mid_xy[i, ])),
    crs = crs
  )
  wet <- lengths(sf::st_intersects(mid_pts, banks_geom)) > 0L
  keep <- rep(FALSE, length(tvals))
  keep[1L] <- wet[1L]
  if (length(wet) > 1L) {
    keep[2L:(length(tvals) - 1L)] <- wet[-length(wet)] != wet[-1L]
  }
  keep[length(tvals)] <- wet[length(wet)]
  tvals[keep]
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
  tvals <- tvals[keep]
  tvals <- pmin(pmax(tvals, 0), chord_len)
  tvals <- sort(tvals)

  tol_t <- transect_breakpoint_tolerance(chord_len)
  tvals <- collapse_sorted_tvals(tvals, tol_t)
  tvals[abs(tvals) <= tol_t] <- 0
  tvals[abs(tvals - chord_len) <= tol_t] <- chord_len
  tvals <- collapse_sorted_tvals(sort(tvals), tol_t)
  tvals <- transect_transition_tvals(tvals, start, u, banks_geom, crs)

  mat <- cbind(
    start[1L] + tvals * u[1L],
    start[2L] + tvals * u[2L]
  )

  if (nrow(mat) < 2L) {
    chord_filled
  } else {
    sf::st_linestring(mat)
  }
}

#' Cumulative distances along a closed polygon ring (vertices only).
#' @noRd
boundary_ring_cumdist <- function(ring_sfc) {
  co <- sf::st_coordinates(ring_sfc)[, 1:2, drop = FALSE]
  if (nrow(co) < 2L) {
    stop("Empty boundary.", call. = FALSE)
  }
  if (isTRUE(all.equal(co[1L, ], co[nrow(co), ]))) {
    co <- co[-nrow(co), , drop = FALSE]
  }
  n <- nrow(co)
  nxt <- co[c(2L:n, 1L), , drop = FALSE]
  seg_len <- sqrt(rowSums((nxt - co)^2))
  list(
    co = co,
    cum = c(0, cumsum(seg_len)),
    len = sum(seg_len)
  )
}

#' Point at distance `d` along a closed ring (modulo length).
#' @noRd
boundary_ring_point_at <- function(ring_info, d) {
  co <- ring_info$co
  cum <- ring_info$cum
  len <- ring_info$len
  d <- d %% len
  if (d <= 0 || abs(d - len) < 1e-9) {
    return(co[1L, , drop = TRUE])
  }
  j <- max(which(cum <= d + 1e-9))
  if (j >= length(cum) - 1L) {
    return(co[1L, , drop = TRUE])
  }
  frac <- (d - cum[j]) / (cum[j + 1L] - cum[j])
  co[j, , drop = TRUE] * (1 - frac) + co[j + 1L, , drop = TRUE] * frac
}

#' Extract a boundary sub-arc between two distances along a closed ring line.
#' @noRd
boundary_ring_arc <- function(ring_sfc, d0, d1) {
  info <- boundary_ring_cumdist(ring_sfc)
  len <- info$len
  co <- info$co
  cum <- info$cum
  n <- nrow(co)
  d0 <- d0 %% len
  d1 <- d1 %% len
  if (abs(d0 - d1) < 1e-9) {
    stop("Degenerate boundary arc.", call. = FALSE)
  }

  pts <- list(boundary_ring_point_at(info, d0))
  in_arc <- function(d) {
    if (d1 > d0) {
      d > d0 + 1e-9 && d < d1 - 1e-9
    } else {
      d > d0 + 1e-9 || d < d1 - 1e-9
    }
  }
  for (k in seq_len(n)) {
    if (in_arc(cum[k])) {
      pts[[length(pts) + 1L]] <- co[k, , drop = TRUE]
    }
  }
  pts[[length(pts) + 1L]] <- boundary_ring_point_at(info, d1)
  mat <- do.call(rbind, pts)
  sf::st_linestring(mat)
}

#' Tolerance for snapping boundary points to left/right bank chains (plan CRS units).
#' @noRd
plan_bank_chain_tolerance <- function(banks) {
  bb <- sf::st_bbox(banks)
  diag <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2
  )
  max(1e-4, diag * 1e-8)
}

#' Split the outer channel boundary into left- and right-bank chains using the axis.
#'
#' Uses intersections of the axis with the footprint boundary (channel ends), not
#' extrema of vertex projections, so concave bulges do not place both chains on
#' the same bank arc.
#' @noRd
plan_bank_boundary_chains <- function(banks_filled, cl) {
  crs <- sf::st_crs(banks_filled)
  bd <- sf::st_boundary(sf::st_geometry(banks_filled))
  hit <- sf::st_intersection(cl, bd)
  xy <- matrix(0, 0, 2)
  if (length(hit) > 0L) {
    hit_empty <- sf::st_is_empty(hit)
    if (!any(is.na(hit_empty)) && !all(hit_empty)) {
      xy <- intersection_xy_matrix(hit)
    }
  }
  if (nrow(xy) < 2L) {
    g <- sf::st_geometry(banks_filled)[[1L]]
    if (inherits(g, "MULTIPOLYGON")) {
      g <- g[[1L]]
    }
    ring <- g[[1L]]
    if (isTRUE(all.equal(ring[1L, ], ring[nrow(ring), ]))) {
      ring <- ring[-nrow(ring), , drop = FALSE]
    }
    n <- nrow(ring)
    pts_sfc <- sf::st_sfc(
      lapply(seq_len(n), function(i) sf::st_point(ring[i, ])),
      crs = crs
    )
    s_vert <- as.numeric(sf::st_line_project(cl, pts_sfc))
    i_up <- which.min(s_vert)[1L]
    i_down <- which.max(s_vert)[1L]
    if (i_up == i_down) {
      stop(
        "Could not split channel boundary into upstream and downstream ends.",
        call. = FALSE
      )
    }
    pt_up <- pts_sfc[i_up]
    pt_down <- pts_sfc[i_down]
  } else {
    pts_end <- sf::st_sfc(
      lapply(seq_len(nrow(xy)), function(i) sf::st_point(xy[i, ])),
      crs = crs
    )
    s_end <- as.numeric(sf::st_line_project(cl, pts_end))
    ord <- order(s_end)
    pt_up <- pts_end[ord[1L]]
    pt_down <- pts_end[ord[length(ord)]]
  }

  ring_line <- sf::st_cast(bd, "LINESTRING")[[1L]]
  ring_sfc <- sf::st_sfc(ring_line, crs = crs)
  d_up <- as.numeric(sf::st_line_project(ring_sfc, pt_up))
  d_down <- as.numeric(sf::st_line_project(ring_sfc, pt_down))
  if (abs(d_up - d_down) < 1e-8) {
    stop(
      "Channel axis meets the footprint boundary at a single location; cannot split banks.",
      call. = FALSE
    )
  }

  arc_a <- boundary_ring_arc(ring_sfc, d_up, d_down)
  arc_b <- boundary_ring_arc(ring_sfc, d_down, d_up)

  len <- as.numeric(sum(sf::st_length(cl)))
  arc_a_sfc <- sf::st_sfc(arc_a, crs = crs)
  mid_a <- sf::st_line_interpolate(
    arc_a_sfc,
    as.numeric(sf::st_length(arc_a_sfc)) / 2
  )
  tangent <- axis_unit_tangent_downstream(cl, mid_a, len)
  mid_xy <- sf::st_coordinates(mid_a)[1L, 1:2, drop = TRUE]
  s_mid <- as.numeric(sf::st_line_project(cl, mid_a))
  axis_pt <- sf::st_coordinates(sf::st_line_interpolate(cl, s_mid))[
    1L,
    1:2,
    drop = TRUE
  ]
  cross_mid <- tangent[1L] *
    (mid_xy[2L] - axis_pt[2L]) -
    tangent[2L] * (mid_xy[1L] - axis_pt[1L])
  if (cross_mid > 0) {
    list(
      left = arc_a,
      right = arc_b,
      crs = crs,
      left_sfc = sf::st_sfc(arc_a, crs = crs),
      right_sfc = sf::st_sfc(arc_b, crs = crs)
    )
  } else {
    list(
      left = arc_b,
      right = arc_a,
      crs = crs,
      left_sfc = sf::st_sfc(arc_b, crs = crs),
      right_sfc = sf::st_sfc(arc_a, crs = crs)
    )
  }
}

#' Per-station values reused while testing candidate transects.
#' @noRd
station_transect_context <- function(
  station_sf,
  cl,
  len_axis,
  banks_centroid,
  tol
) {
  axis_pt <- sf::st_coordinates(
    sf::st_line_interpolate(cl, sf::st_line_project(cl, station_sf))
  )[1L, 1:2, drop = TRUE]
  to_ctr <- banks_centroid - axis_pt
  n_in_len <- sqrt(sum(to_ctr^2))
  tangent <- axis_unit_tangent_downstream(cl, station_sf, len_axis)
  if (n_in_len > 1e-3) {
    n_in <- to_ctr / n_in_len
  } else {
    n_in <- c(-tangent[2L], tangent[1L])
  }
  list(
    tol = tol,
    n_in = n_in,
    n_width = c(-tangent[2L], tangent[1L])
  )
}

#' Classify a boundary point as left or right bank chain (or ambiguous).
#' @noRd
point_bank_chain <- function(xy, chains, tol) {
  pt <- sf::st_sfc(sf::st_point(xy), crs = chains$crs)
  dl <- as.numeric(sf::st_distance(pt, chains$left_sfc))
  dr <- as.numeric(sf::st_distance(pt, chains$right_sfc))
  if (abs(dl - dr) <= tol) {
    return(NA_character_)
  }
  if (dl < dr) "left" else "right"
}

#' @noRd
transect_connects_opposite_banks <- function(seg, chains, ctx, strict = FALSE) {
  m <- sf::st_coordinates(seg)
  if (nrow(m) < 2L) {
    return(FALSE)
  }
  e1 <- m[1L, 1:2]
  e2 <- m[nrow(m), 1:2]
  s1 <- point_bank_chain(e1, chains, ctx$tol)
  s2 <- point_bank_chain(e2, chains, ctx$tol)
  if (is.na(s1) || is.na(s2) || identical(s1, s2)) {
    return(FALSE)
  }

  chord <- e2 - e1
  chord_len <- sqrt(sum(chord^2))
  if (chord_len < ctx$tol) {
    return(FALSE)
  }
  toward_ctr <- abs(sum(chord * ctx$n_in)) / chord_len
  along_width <- abs(sum(chord * ctx$n_width)) / chord_len
  # Concave bulges: minimum-width chords can run along one bank (high
  # along_width, weak toward_ctr) while still hitting opposite chains.
  toward_min <- if (strict) 0.42 else 0.03
  if (along_width > 0.85 && toward_ctr < toward_min) {
    return(FALSE)
  }
  TRUE
}

#' Original minimum-width search (no opposite-bank filter).
#' @noRd
minimum_width_transect_raw <- function(pt, banks_filled, maxd) {
  calc_width <- function(angle) {
    seg <- span_banks_engine(
      pt,
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
  span_banks_engine(
    pt,
    res,
    bankline = banks_filled,
    maxd = maxd,
    intersect = TRUE,
    reposition = TRUE
  )[[1]]
}

#' Shortest bank-to-bank segment through `pt` that hits opposite boundary chains.
#' @noRd
shortest_opposite_bank_transect <- function(
  pt,
  banks_filled,
  maxd,
  bank_chains,
  ctx
) {
  best <- NULL
  best_w <- Inf
  angles <- seq(0, pi, length.out = 36)
  for (ang in angles) {
    seg <- span_banks_engine(
      pt,
      ang,
      bankline = banks_filled,
      maxd = maxd,
      intersect = TRUE,
      reposition = TRUE
    )[[1]]
    if (!transect_connects_opposite_banks(seg, bank_chains, ctx, strict = TRUE)) {
      next
    }
    w <- as.numeric(sf::st_length(seg))
    if (w < best_w) {
      best_w <- w
      best <- seg
    }
  }
  if (is.null(best)) {
    stop(
      "Could not find a transect through the station that connects opposite banks.",
      call. = FALSE
    )
  }
  best
}

#' Minimum-width transect through `pt` between opposite banks (not the same bank chain).
#' @noRd
minimum_width_opposite_bank_transect <- function(
  pt,
  banks_filled,
  maxd,
  bank_chains,
  ctx
) {
  chord <- minimum_width_transect_raw(pt, banks_filled, maxd)
  if (transect_connects_opposite_banks(chord, bank_chains, ctx)) {
    return(chord)
  }

  valid_transect <- function(seg) {
    transect_connects_opposite_banks(seg, bank_chains, ctx, strict = TRUE)
  }

  calc_width <- function(angle) {
    seg <- span_banks_engine(
      pt,
      angle,
      bankline = banks_filled,
      maxd = maxd,
      intersect = TRUE,
      reposition = TRUE
    )[[1]]
    if (!valid_transect(seg)) {
      return(Inf)
    }
    as.numeric(sf::st_length(seg))
  }

  angles <- seq(0, pi, length.out = 10)
  widths <- vapply(angles, calc_width, numeric(1))
  valid <- is.finite(widths)
  if (!any(valid)) {
    return(shortest_opposite_bank_transect(
      pt,
      banks_filled,
      maxd,
      bank_chains,
      ctx
    ))
  }
  i_min <- which(widths == min(widths[valid]))
  if (length(i_min) > 1) {
    angles <- seq(0, pi, length.out = 100)
    widths <- vapply(angles, calc_width, numeric(1))
    valid <- is.finite(widths)
    if (!any(valid)) {
      return(shortest_opposite_bank_transect(
        pt,
        banks_filled,
        maxd,
        bank_chains,
        ctx
      ))
    }
    i_min <- which(widths == min(widths[valid]))[1]
  }
  delta <- pi / (length(angles) - 1)
  rng <- angles[i_min] + c(-delta, delta)
  res <- stats::optimize(calc_width, rng)$minimum
  chord_filled <- span_banks_engine(
    pt,
    res,
    bankline = banks_filled,
    maxd = maxd,
    intersect = TRUE,
    reposition = TRUE
  )[[1]]
  if (!valid_transect(chord_filled)) {
    return(shortest_opposite_bank_transect(
      pt,
      banks_filled,
      maxd,
      bank_chains,
      ctx
    ))
  }
  chord_filled
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
