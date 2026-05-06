#' Generate channel object from banklines
#'
#' Generate a channel object with planimetric cross sections from a
#' bankline polygon.
#'
#' @param banks Bankline polygon (sf POLYGON object)
#' @param ... Additional arguments (ignored).
#' @param n Number of cross sections to generate (mutually exclusive with
#'   spacing and at)
#' @param spacing Distance between cross sections (mutually exclusive with n
#'   and at)
#' @param at Specific distances along centerline for cross sections (mutually
#'   exclusive with n and spacing)
#' @param centerline Centerline multilinestring object. If NULL (the default),
#' will be generated automatically using the centerline package.
#' @returns A channel object with planimetric cross sections in the plan column.
#' @details This function takes the definition of "cross section" relative
#' to a point in the channel to be the line segment intersecting the point
#' whose bank-to-bank segment width is the smallest. Note that this does not
#' imply that the cross section is unique, and in this case the cross section
#' is arbitrarily taken to be the one closest to a 0-degree angle --
#' although in almost all cases this should not be an issue.
#'
#' To define the spacing of the cross sections, a centerline is
#' first calculated, and equally spaced points are sampled along the
#' centerline. Cross sections are calculated at these points.
#' @examples
#' bl <- sf::st_sfc(demo_bankline, crs = 3005)
#' channel <- xt_generate_plan(bl, n = 100)
#'
#' # With custom centerline
#' channel <- xt_generate_plan(bl, n = 100, centerline = demo_centerline)
#' @export
xt_generate_plan <- function(banks, ..., n, spacing, at, centerline = NULL) {
  rlang::check_dots_empty()

  # Validate input parameters: exactly one of n, spacing, or at is required.
  n_specified <- !missing(n)
  spacing_specified <- !missing(spacing)
  at_specified <- !missing(at)

  if (sum(n_specified, spacing_specified, at_specified) != 1) {
    stop("Exactly one of n, spacing, or at must be specified.")
  }

  if (is.null(centerline)) {
    cl <- banks_to_centerline(banks)
  } else {
    cl <- centerline
  }

  lr <- split_bankline(banks, centerline = cl)
  len <- sum(sf::st_length(cl))

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

  # Sort pts in order along centerline. This is important so that neighbouring
  # cross sections can be later ensured not to cross.
  dists <- sf::st_line_project(cl, pts)
  pts <- pts[order(dists)]

  # Get maximum distance based on bounding box
  bb <- sf::st_bbox(banks)
  maxd <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2
  )

  xs <- list()
  for (i in seq_along(pts)) {
    # Make a function to calculate the width of a bank-to-bank line for a
    # given angle, for the first point in the centerline.
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
  }

  ## Combine list of segments in xs into a single sf geometry
  geoms <- sf::st_as_sfc(xs)
  sf::st_crs(geoms) <- sf::st_crs(banks)

  for (i in seq_along(pts)) {
    this_xs <- geoms[i]
    d1 <- sf::st_distance(sf::st_cast(this_xs, "POINT")[1], lr$left)
    d2 <- sf::st_distance(sf::st_cast(this_xs, "POINT")[2], lr$left)
    if (d2 < d1) {
      geoms[i] <- sf::st_reverse(geoms[i])
    }
  }

  attr(geoms, "left_to_right") <- TRUE

  # Create channel object
  xt_channel(geoms)
}
