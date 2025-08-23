#' Plot profile cross section
#'
#' Plot a profile cross section showing the elevation profile.
#' This function is primarily for internal use or advanced users.
#' Most users should work with channel objects using plot_plan() or plot_3d().
#'
#' @param x An xs_profile object
#' @param ... Additional arguments passed to plot
#' @param add Logical. Add to existing plot?
#' @param exaggerate Single positive numeric. Vertical exaggeration factor;
#' defaults to 2. See details.
#' @returns A plot of the profile cross section
#' @details
#' The exaggeration factor is applied to the vertical dimension of the profile,
#' and defaults to 2 so that changes in topography can be more easily
#' perceived. It is strongly recommended not going beyond 3, because
#' exaggeration beyond this point can distort the perception of the profile.
#'
#' @examples
#' # Plot a profile cross section (advanced use)
#' plot(profile_object)
#'
#' # Plot with vertical exaggeration
#' plot(profile_object, exaggerate = 2)
#' @exportS3Method base::plot
plot.xs_profile <- function(x, ..., extent, add = FALSE, exaggerate = 2) {
  x <- exaggerate_relief(x, exaggerate)
  if (!add) {
    plot(sf::st_sfc(
      sf::st_multilinestring(list(x$left$coordinates)),
      sf::st_multilinestring(list(x$right$coordinates))
    ), ...)
  }
  plot(sf::st_sfc(sf::st_multilinestring(list(x$left$coordinates))), add = TRUE, col = "blue", ...)
  plot(sf::st_sfc(sf::st_multilinestring(list(x$right$coordinates))), add = TRUE, col = "red", ...)
  plot(sf::st_linestring(rbind(x$left$thalweg, x$right$thalweg)), add = TRUE, ...)
  plot(sf::st_point(x$left$bank_point), add = TRUE, col = "blue", ...)
  plot(sf::st_point(x$right$bank_point), add = TRUE, col = "red", ...)
}
