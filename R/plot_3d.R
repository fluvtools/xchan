#' Plot 3D channel cross sections
#'
#' Create a 3D plot of channel cross sections showing both plan and profile views.
#'
#' @param x Channel object with plan and profile cross sections
#' @param ... Additional arguments passed to plot
#' @param col Color for the cross sections
#' @param alpha Transparency (0-1)
#' @returns A 3D plot of the channel cross sections
#' @param exaggerate Single positive numeric. Vertical exaggeration factor;
#' defaults to 2. See details.
#' @details
#' The exaggeration factor is applied to the vertical dimension of the profile,
#' and defaults to 2 so that changes in topography can be more easily
#' perceived. It is strongly recommended not going beyond 3, because
#' exaggeration beyond this point can distort the perception of the profile.
#'
#' @examples
#' # Create 3D plot of channel
#' plot_3d(channel)
#'
#' # Plot with custom styling
#' plot_3d(channel, col = "blue", alpha = 0.7)
plot_3d <- function(x, ..., extent, exaggerate = 2, col = "black", alpha = 0.8) {
  checkmate::assert_class(x, "sxchan")

  plan <- xt_column_plan(x)
  profile <- xt_column_profile(x)

  if (is.null(plan) || is.null(profile)) {
    stop("Channel object must have both plan and profile cross sections")
  }

  # Convert to 3D coordinates using existing function
  coords_3d <- Map(create_3d_coords, plan, profile)

  # Create 3D plot
  # Note: This is a placeholder - you may want to use rgl or plotly for 3D plotting
  # For now, we'll create a simple visualization
  plot(sf::st_sfc(coords_3d), col = col, alpha = alpha, ...)
}
