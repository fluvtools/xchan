#' Plot a channel object
#'
#' @param x An [`xchan`] object.
#' @param extent One of `"banks"` (default) or `"full"`.
#'   `"full"` draws each transect out to the ends of its profile (when profiles
#'   are present); uses bank-to-bank segments only if there is no profile
#'   geometry (with a warning). Profile bank markers are drawn only when `extent
#'   = "full"` and bank markers are enabled (see `banks`).
#' @param axis How to draw the stored channel axis ([xt_axis()]), when one is
#'   present: `"line"` (default) draws the axis as a plain line, `"arrows"`
#'   draws flow direction along the axis, `"none"` omits it.
#'
#'   When [xt_bankline()] is set and `add = FALSE`, the footprint is drawn first
#'   (filled polygon under transects), then cross sections and the axis overlay.
#' @param banks One of `"auto"`, `"show"`, or `"hide"`: whether to draw bank
#'   markers on planimetric transects (endpoints, and all profile banks when
#'   `extent = "full"`). `"auto"` draws markers when there is no [xt_bankline()]
#'   footprint, and omits them when a footprint is present (markers are
#'   redundant with the polygon boundary). `"show"` / `"hide"` override that
#'   rule.
#' @param ... Additional arguments passed to [plot()] for plan geometries and
#'   to bank-marker styling (`col_bank`, `pch_bank`, `cex_bank`).
#' @param add If `TRUE`, draw on the current plot (same rules as
#'   [graphics::plot()]).
#' @details
#' For the plan view, when `add = FALSE` and you do not pass `xlim` / `ylim`,
#' the plot limits are adjusted so the on-screen aspect is not extremely
#' stretched (for example when synthetic width-only channels use a large default
#' station spacing relative to transect width). Pass your own `xlim` and `ylim`
#' to reproduce the raw map scale.
#'
#' For "profile" and "3d" views, you can specify `exaggerate` to vertically
#' exaggerate the relief. For example, `exaggerate = 2` doubles the vertical
#' scale. The default is `exaggerate = 1` (no exaggeration). It is strongly
#' recommended not going beyond 3, because exaggeration beyond this point can
#' distort the perception of the profile.
#' @examples
#' channel <- xt_as_channel(rep(1, 6))
#' channel <- xt_add_profile(
#'   channel,
#'   distance = distance,
#'   elevation = elevation,
#'   section = id,
#'   banks = is_bank,
#'   data = profile_survey
#' )
#' plot(channel, extent = "full")
#' @export
plot.xchan <- function(
  x,
  ...,
  extent = c("banks", "full"),
  axis = c("line", "arrows", "none"),
  banks = c("auto", "show", "hide"),
  add = FALSE
) {
  axis <- match.arg(axis)
  banks <- match.arg(banks)
  plot_plan(x, extent = extent, axis = axis, add = add, banks = banks, ...)
}
