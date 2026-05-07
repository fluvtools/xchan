#' Plot a channel object
#'
#' @param x Channel object
#' @param extent Character string indicating the extent of the plot: "full" or "bankline".
#' @param ... Additional arguments passed to the specific plot methods.
#' @details
#' For "profile" and "3d" views, you can specify `exaggerate` to vertically
#' exaggerate the relief. For example, `exaggerate = 2` doubles the vertical scale.
#' The default is `exaggerate = 1` (no exaggeration). It is strongly recommended not going beyond 3, because
#' exaggeration beyond this point can distort the perception of the profile.
#' @export
plot.xchan <- function(x, ..., extent = c("full", "bankline")) {
  extent <- rlang::arg_match(extent)
  plot_plan(x, extent = extent, ...)
}
