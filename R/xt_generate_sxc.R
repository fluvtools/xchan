#' Calculate Cross Sections from Banklines
#'
#' Calculate cross sections from banklines that are roughly equally spaced
#' apart.
#' @param n The number of cross sections to calculate.
#' @param bankline An sf object with banklines.
#' @param centerline Centerline multilinestring object. If NULL (the default),
#' will be generated automatically using the centerline package.
#' @return An "sxc" object, with CRS equal to that of the banklines object.
#' @details This function takes the definition of "cross section" relative
#' to a point in the channel to be the line segment intersecting the point
#' whose bank-to-bank segment width is the smallest. Note that this does not
#' imply that the cross section is unique, and in this case the cross section
#' is arbitrarily taken to be the one closest to a 0-degree angle --
#' although in almost all cases this should not be an issue.
#'
#' To define the spacing of the cross sections, a centerline is
#' first calculated, and equally spaced points are sampled along the
#' centerline. Cross sections are calculated at these points. Note that
#' cross sections are not a necessary part of choosing cross section spacing,
#' but it is useful.
#' @note The function the calculates bank-to-bank width for a given angle of
#' the line crossing through a specified point is riddled with local minima.
#' Currently, a remedial approach is taken to mitigating this:
#' it first finds the minimum width on a grid of 10 angles between 0 and pi
#' (inclusive) (or 100 if the minimum is not unique at first), and then
#' optimizes the width function around the minimum found on the grid. This
#' slows down the algorithm noticeably, but not
#' It's still possible, although likely very rare, for the global minimum
#' to be missed. An improvement might involve choosing angles that do
#' not intersect with the neighbouring cross section.
#'
#' Another improvement to consider is to ensure cross sections go between
#' left bank and right bank. Since points are being sampled along the
#' centerline, this should be less of an issue, but there's still a chance
#' where a cross section will be identified for a "bay" in the channel.
#' @examples
#' bl <- sf::st_sfc(demo_bankline, crs = 3005)
#' cross <- xt_generate_sxc(bl, n = 100)
#' plot(bl); plot(cross, add = TRUE, col = "blue")
#' # Inherits spatial properties of the bankline polygon, such as CRS:
#' sf::st_crs(cross)
#' @export
xt_generate_sxc <- function(bankline, n, centerline = NULL) {
  if (is.null(centerline)) {
    cl <- sf::st_geometry(centerline::cnt_path_guess(bankline, keep = 1))
  } else {
    cl <- centerline
  }
  lr <- split_bankline(bankline, centerline = cl)
  len <- sum(sf::st_length(cl))
  pts <- sf::st_line_sample(cl, density = n / len)
  # Only take points that are not empty, and split apart multipoints
  # into individual points.
  pts <- pts[sapply(pts, function(x) !sf::st_is_empty(x))]
  pts <- sf::st_cast(pts, "POINT")
  # Get maximum distance from bounding box
  bb <- sf::st_bbox(bankline)
  maxd <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2
  )
  xs <- list()
  for (i in seq_along(pts)) {
    # Make a function to calculate the width of a bank-to-bank line for a
    # given angle, for the first point in the centerline.
    calc_width <- function(angle) {
      seg <- span_banks_engine(
        pts[i], angle, bankline = bankline, maxd = maxd, intersect = TRUE,
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
      pts[i], res, bankline = bankline, maxd = maxd, intersect = TRUE,
      reposition = TRUE
    )[[1]]
  }
  ## Combine list of segments in xs into a single sf geometry
  geoms <- sf::st_as_sfc(xs)
  sf::st_crs(geoms) <- sf::st_crs(bankline)
  for (i in seq_along(pts)) {
    this_xs <- geoms[i]
    d1 <- sf::st_distance(sf::st_cast(this_xs, "POINT")[1], lr$left)
    d2 <- sf::st_distance(sf::st_cast(this_xs, "POINT")[2], lr$left)
    if (d2 < d1) {
      geoms[i] <- sf::st_reverse(geoms[i])
    }
  }
  attr(geoms, "left_to_right") <- TRUE
  xt_sxc(geoms)
}
