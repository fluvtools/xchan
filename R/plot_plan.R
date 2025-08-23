#' Plot planimetric cross sections
#'
#' Plot the planimetric cross sections from a channel object.
#'
#' @param channel Channel object with planimetric cross sections
#' @param ... Additional arguments passed to plot
#' @param add Logical. Add to existing plot?
#' @param col Color for the cross sections
#' @param lwd Line width
#' @returns A plot of the planimetric cross sections
#' @examples
#' # Plot planimetric cross sections
#' plot_plan(channel)
#'
#' # Plot with custom styling
#' plot_plan(channel, col = "blue", lwd = 2)
plot_plan <- function(channel, ..., extent, add = FALSE, col = "black", lwd = 1) {
  checkmate::assert_class(channel, "sxchan")

  plan <- xt_column_plan(channel)
  if (is.null(plan)) {
    stop("Channel object must have planimetric cross sections")
  }

  if (!add) {
    plot(plan, col = col, lwd = lwd, ...)
  } else {
    plot(plan, col = col, lwd = lwd, add = TRUE, ...)
  }
}
