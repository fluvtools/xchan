#' Plot a channel object
#'
#' @param x Channel object
#' @param extent One of `"banks"` (default) or `"full"`.
#'   `"full"` draws each transect out to the ends of its profile (when profiles
#'   are present) and marks bank positions; uses bank-to-bank segments only if
#'   there is no profile column (with a warning).
#' @param ... Additional arguments passed to [plot()] for plan geometries and
#'   to bank-marker styling (`col_bank_water`, `col_bank_land`, `pch_bank`,
#'   `cex_bank`) when `extent = "full"`.
#' @details
#' For "profile" and "3d" views, you can specify `exaggerate` to vertically
#' exaggerate the relief. For example, `exaggerate = 2` doubles the vertical scale.
#' The default is `exaggerate = 1` (no exaggeration). It is strongly recommended not going beyond 3, because
#' exaggeration beyond this point can distort the perception of the profile.
#' @export
plot.xchan <- function(x, ..., extent = c("banks", "full")) {
  plot_plan(x, extent = extent, ...)
}
