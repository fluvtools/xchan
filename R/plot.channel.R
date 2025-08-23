#' Plot a channel object
#'
#' @param x Channel object
#' @param view Character string indicating the view type: "plan", "profile", or "3d".
#' @param extent Character string indicating the extent of the plot: "full" or "bankline".
#' @param ... Additional arguments passed to the specific plot methods.
#' @details
#' For "profile" and "3d" views, you can specify `exaggerate` to vertically 
#' exaggerate the relief. For example, `exaggerate = 2` doubles the vertical scale.
#' The default is `exaggerate = 1` (no exaggeration). It is strongly recommended not going beyond 3, because
#' exaggeration beyond this point can distort the perception of the profile.
#' @export
plot.channel <- function(x, view = c("plan", "profile", "3d"), extent = c("full", "bankline"), ...) {
  view <- rlang::arg_match(view)
  if (view == "plan") {
    if (!xt_has_plan(x)) {
      stop("Channel object does not have planimetric cross sections.")
    }
    return(plot_plan(x, extent = extent,...))
  }
  if (view == "profile") {
    if (!xt_has_profile(x)) {
      stop("Channel object does not have profile cross sections.")
    }
    return(plot_profile(x, extent = extent, ...))
  }
  # 3D remains
  if (!(xt_has_profile(x) && xt_has_plan(x))) {
    stop("Channel object needs both planimetric and profile cross sections to construct 3D geometry.")
  }
  plot_3d(x, extent = extent, ...)
}
