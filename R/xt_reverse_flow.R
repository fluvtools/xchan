#' Reverse a Channel's Flow Direction
#'
#' Swaps what is called the "left" and "right" banks (from the perspective of
#' someone looking downstream).
#'
#' @param channel An [`xchan`] object.
#' @returns An [`xchan`] with flow direction reversed (see **Details**).
#' @details
#' Planimetric segments are reversed end-for-end (`flip_plan()`), so the first
#' vertex now corresponds to what was the right bank (and vice versa). Profile
#' cross sections, when present, are flipped with `flip_profile()` so signed
#' distances across the section stay aligned with the plan. When there is no
#' profile geometry, only the planimetric geometries are updated.
#'
#' If a channel axis is stored ([xt_axis()]), it is reversed end-for-end with
#' [sf::st_reverse()] so downstream distance ([xt_distance_downstream()]) is
#' still measured from the **start** of the axis line — which is now the
#' opposite physical end of the reach. Cross sections keep the same projected
#' positions along the same geographic line, but chainage is recomputed from the
#' new zero at the former downstream end. Then [xt_arrange_downstream()] sorts
#' by increasing chainage along **hydrologic** downstream after the reversal
#' (for example order D, C, B, A instead of A, B, C, D when those letters ran
#' upstream-to-downstream before). Section order in the [`xchan`] list is
#' unchanged by this function.
#' @note Summaries that treat left and right symmetrically (e.g.
#' [elevation_bank()] with
#' default `min`) are unchanged until you reorder sections (e.g.
#' [xt_arrange_downstream()]).
#' @export
xt_reverse_flow <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  channel <- set_channel_plan(channel, flip_plan(channel_plan(channel)))
  if (xt_has_profile(channel)) {
    channel <- set_channel_profile(
      channel,
      lapply(channel_profile(channel), flip_profile)
    )
  }
  ax <- attr(channel, "axis", exact = TRUE)
  if (!is.null(ax)) {
    attr(channel, "axis") <- sf::st_reverse(ax)
  }
  channel
}
