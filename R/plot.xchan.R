#' Plot a channel object
#'
#' @param x An [`xchan`] object.
#' @param extent One of `"banks"` (default) or `"full"`.
#'   `"full"` draws each transect out to the ends of its profile (when profiles
#'   are present) and marks bank positions; uses bank-to-bank segments only if
#'   there is no profile geometry (with a warning).
#' @param axis How to draw the stored channel axis ([xt_axis()]), when one is
#'   present: `"line"` (default) draws the axis as a plain line, `"arrows"` draws
#'   flow direction along the axis, `"none"` omits it.
#' @param ... Additional arguments passed to [plot()] for plan geometries and
#'   to bank-marker styling (`col_bank_water`, `col_bank_land`, `pch_bank`,
#'   `cex_bank`) when `extent = "full"`.
#' @param add If `TRUE`, draw on the current plot (same rules as [graphics::plot()]).
#' @details
#' For the plan view, when `add = FALSE` and you do not pass `xlim` / `ylim`,
#' the plot limits are adjusted so the on-screen aspect is not extremely
#' stretched (for example when synthetic width-only channels use a large
#' default station spacing relative to transect width). Pass your own
#' `xlim` and `ylim` to reproduce the raw map scale.
#'
#' For "profile" and "3d" views, you can specify `exaggerate` to vertically
#' exaggerate the relief. For example, `exaggerate = 2` doubles the vertical scale.
#' The default is `exaggerate = 1` (no exaggeration). It is strongly recommended not going beyond 3, because
#' exaggeration beyond this point can distort the perception of the profile.
#' @export
plot.xchan <- function(
  x,
  ...,
  extent = c("banks", "full"),
  axis = c("line", "arrows", "none"),
  add = FALSE
) {
  axis <- match.arg(axis)
  plot_plan(x, extent = extent, axis = axis, add = add, ...)
}
