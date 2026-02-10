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
plot.sxchan <- function(x, ..., extent = c("full", "bankline")) {
  extent <- rlang::arg_match(extent)
  if (!xt_has_plan(x)) {
    stop("Channel object does not have planimetric cross sections for viewing.")
  }
  plot_plan(x, extent = extent, ...)
}
