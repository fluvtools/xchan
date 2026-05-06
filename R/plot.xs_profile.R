#' Plot profile cross section
#'
#' Plot a profile cross section showing the elevation profile.
#' This function is primarily for internal use or advanced users.
#' Most users should work with channel objects using plot_plan() or plot_3d().
#'
#' @param x An xs_profile object
#' @param ... Additional arguments passed to plot
#' @param extent Character string indicating the extent of the plot: "full" or
#'   "bankline". Defaults to "full".
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
plot.xs_profile <- function(
  x,
  ...,
  extent = c("full", "bankline"),
  add = FALSE,
  exaggerate = 2
) {
  extent <- rlang::arg_match(extent)
  x <- exaggerate_relief(x, exaggerate)

  if (!add) {
    # Create empty plot
    plot(
      x$coordinates[, 1],
      x$coordinates[, 2],
      type = "n",
      xlab = "Distance",
      ylab = "Elevation",
      ...
    )
  }

  # Plot the main profile line
  graphics::lines(
    x$coordinates[, 1],
    x$coordinates[, 2],
    col = "black",
    lwd = 2,
    ...
  )

  # Plot bank points
  bank_coords <- get_bank_coords(x)
  graphics::points(
    bank_coords[, 1],
    bank_coords[, 2],
    col = "red",
    pch = 19,
    cex = 1.5,
    ...
  )

  # Plot thalweg points
  thalweg_coords <- get_thalweg_coords(x)
  graphics::points(
    thalweg_coords[, 1],
    thalweg_coords[, 2],
    col = "blue",
    pch = 17,
    cex = 1.2,
    ...
  )

  # Add legend
  graphics::legend(
    "topright",
    legend = c("Profile", "Banks", "Thalwegs"),
    col = c("black", "red", "blue"),
    lty = c(1, NA, NA),
    pch = c(NA, 19, 17),
    cex = 0.8
  )
}
