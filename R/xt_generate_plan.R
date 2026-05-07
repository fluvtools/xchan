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
#' @param spacing Distance between cross sections (mutually exclusive with n
#'   and at)
#' @param at Specific distances along the channel axis for cross sections (mutually
#'   exclusive with n and spacing)
#' @param axis Channel axis as a multilinestring: the line along which cross
#'   sections are placed. If `NULL` (the default), an axis is generated
#'   automatically using the **centerline** package (`centerline::cnt_path_guess()`).
#' @param progress Logical; if `TRUE`, display a text progress bar while
#'   generating planimetric cross sections.
#' @returns A channel object with planimetric cross sections in the plan column.
#'   Rows follow downstream order along the sampling axis. After `arrange()` or
#'   subsetting, restore order with [xt_arrange_downstream()]. Use [xt_distance_ds()]
#'   for distance along the axis from its start to each section (requires the axis
#'   from [xt_axis()], which this function sets). The sampling axis is stored for
#'   geometry that requires it.
#' @details **Bank geometry:** Supply the channel as one polygon (or
#' multipolygon) so its boundary is a closed loop around the wetted/plan
#' corridor. If you only have two bank polylines, convert them to a closed
#' polygon (e.g. connect upstream/downstream ends) before calling this function.
#'
#' This function takes the definition of "cross section" relative
#' to a point in the channel to be the line segment intersecting the point
#' whose bank-to-bank segment width is the smallest. Note that this does not
#' imply that the cross section is unique, and in this case the cross section
#' is arbitrarily taken to be the one closest to a 0-degree angle --
#' although in almost all cases this should not be an issue.
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
#' **Vertex order (planimetric convention):** each output segment is a
#' `LINESTRING` from left bank to right bank: the **first** coordinate is on the
#' left bank, the **last** on the right bank. Increasing distance along the
#' segment (first to last vertex) thus matches profile cross sections where
#' distance increases from left to right bank.
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

  if (is.null(axis)) {
    cl <- banks_to_centerline(banks)
  } else {
    cl <- axis
  }

  len <- as.numeric(sum(sf::st_length(cl)))

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
    # Make a function to calculate the width of a bank-to-bank line for a
    # given angle, for the first point along the axis.
    calc_width <- function(angle) {
      seg <- span_banks_engine(
        pts[i],
        angle,
        bankline = banks,
        maxd = maxd,
        intersect = TRUE,
        reposition = FALSE
      )
      sf::st_length(seg)
    }

    # Optimize on a grid of 50 points first, because this function
    # is riddled with local minima.
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
    # Use optimization to find the angle that minimizes the width
    res <- stats::optimize(calc_width, rng)$minimum
    xs[[i]] <- span_banks_engine(
      pts[i],
      res,
      bankline = banks,
      maxd = maxd,
      intersect = TRUE,
      reposition = TRUE
    )[[1]]
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

  # Sampling axis for downstream order and distance geometry (see xt_distance_ds)
  out <- xt_as_channel(geoms)
  xt_axis(out) <- cl
  out
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
