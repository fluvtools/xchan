#' Create a spline tracer for bankline generation
#'
#' @param ngrid Number of points to generate along each bankline.
#' @returns A tracer object that takes a channel and returns banklines.
#' @export
tracer_spline <- function(ngrid = 200) {
  f <- function(channel) {
    seg <- xt_as_sfc(channel, "plan")
    # but assume the cross sections are not in order
    # us xt_distant_ds(channel)
    # if we are generating the x-sections, we can
    # assume that the start point is the first point
    # but we have to force the user to get them in order
    n <- xt_n_sections(channel)
    pts <- sf::st_cast(seg, "POINT")
    right <- pts[1:n * 2]
    left <- pts[1:n * 2 - 1]

    # connect cross sections' edges to generate new banklines
    start_edge <- list()
    end_edge <- list()

    for (i in seq_along(left)) {
      # Extract start and end points manually
      start_point <- sf::st_cast(sf::st_geometry(left[i]), "POINT")[1]
      end_point <- sf::st_cast(sf::st_geometry(right[i]), "POINT")[1]
      start_edge[[i]] <- start_point
      end_edge[[i]] <- end_point
    }

    # Smoothen and convert start and end points into a valid sf object
    for (i in 1:2) {
      if (i == 1) {
        edge <- start_edge
        out_text <- "start_edge"
      } else {
        edge <- end_edge
        out_text <- "end_edge"
      }

      edge_sf <- sf::st_sf(
        data.frame(id = seq_along(edge)),
        geometry = sf::st_sfc(do.call(c, edge))
      )
      coords <- lapply(edge, sf::st_coordinates)
      edge_sf$LONG <- vapply(coords, function(x) x[1], FUN.VALUE = numeric(1L))
      edge_sf$LAT <- vapply(coords, function(x) x[2], FUN.VALUE = numeric(1L))

      x <- edge_sf$LONG
      y <- edge_sf$LAT

      # Parameter t: cumulative arc-length
      t <- cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2)))

      # Fit cubic splines for x(t) and y(t)
      x_spline <- stats::splinefun(t, x)
      y_spline <- stats::splinefun(t, y)

      # Generate interpolated path
      t_fine <- seq(min(t), max(t), length.out = ngrid)
      x_smooth <- x_spline(t_fine)
      y_smooth <- y_spline(t_fine)

      # remake x and y into spatial
      geometry <- Map(
        function(x_, y_) sf::st_point(c(x_, y_)),
        x_smooth,
        y_smooth
      )
      new_edge <- sf::st_combine(sf::st_as_sfc(geometry))
      new_edge <- sf::st_cast(new_edge, "LINESTRING")
      if (i == 1) {
        start_edge_lines <- new_edge
      } else {
        end_edge_lines <- new_edge
      }
    }
    sf::st_as_sfc(rbind(start_edge_lines, end_edge_lines))
  }
  structure(
    f,
    name = "Spline Tracer",
    params = list(ngrid = ngrid),
    class = "xchan_tracer"
  )
}
