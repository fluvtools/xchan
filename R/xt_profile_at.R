#' Get one cross section by downstream flow order
#'
#' `xt_xsection_at()` extracts a single [xsection] by **flow order**: the
#' `i`th section when rows are sorted by increasing [xt_distance_downstream()]
#' along the channel axis (see `axis`). Internal helper `xt_profile_at()`
#' returns the embedded `xs_profile` for that section (requires profile
#' geometry).
#'
#' @param channel A channel object ([`xchan`]).
#' @param i Single positive integer: position in downstream flow order (1 =
#'   smallest chainage along `axis`).
#' @param axis Optional LINESTRING axis (`sfc` / `sfg`); if `NULL`, uses
#'   [xt_axis()] on `channel`.
#' @returns `xt_xsection_at()`: one [xsection].
#'
#' @name xt_xsection_at
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
#' xt_xsection_at(channel, 1)
#' @export
xt_xsection_at <- function(channel, i, axis = NULL) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_int(i, lower = 1L, .var.name = "i")
  if (i > length(channel)) {
    stop("`i` is out of bounds for this channel.", call. = FALSE)
  }
  ord <- channel_flow_order(channel, axis)
  channel[[ord[i]]]
}

#' Profile at one section by downstream flow order (not exported).
#'
#' @inheritParams xt_xsection_at
#' @returns One `xs_profile`.
#' @keywords internal
#' @noRd
xt_profile_at <- function(channel, i, axis = NULL) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_int(i, lower = 1L, .var.name = "i")
  if (i > length(channel)) {
    stop("`i` is out of bounds for this channel.", call. = FALSE)
  }
  if (!xt_has_profile(channel)) {
    stop("Channel object must have profile cross sections", call. = FALSE)
  }
  ord <- channel_flow_order(channel, axis)
  channel[[ord[i]]]$profile
}
