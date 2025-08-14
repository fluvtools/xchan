#' @title Widen a river cross section
#' @description Widens a cross section by a specified width or volume,
#'   distributing the change according to a given scheme.
#'
#' @param cross_section An object of class `sx` or a subclass like `sx_1d` or `sx_2d`.
#' @param ... Additional arguments. Not currently used, but required for S3 dispatch.
#' @param width The total width to add to the cross section. Must be a numeric value
#'   and cannot be used with `volume`.
#' @param volume The total volume to remove to widen the cross section. Must be a
#'   numeric value and cannot be used with `width`. Only applicable for 2D
#'   cross sections.
#' @param side A specification for how to distribute the widening, such as
#' "both" (the default), "left", or "right". For more granularity, use
#' `distribute_p_*()` functions to distribute proportions for each bank. See
#' details.
#' @details
#' Distributing the widening between the left and right banks through the
#' `side` argument is always done by running a `distribute_p_*()` function;
#' for simplicity, a character string can be passed to `side` that replaces
#' `*` and uses the function defaults. Options are:
#'   \itemize{
#'     \item "left" or `distribute_p_left()`: specify how much of the widening
#'           applies on the left bank.
#'     \item "right" or `distribute_p_right()`: specify how much of the widening
#'           applies on the right bank.
#'     \item "both" or `distribute_p_both()`: specify how much of the widening
#'           applies to both banks.
#'   }
#' @note
#' While the ellipsis `...` is currently not used, it forces the `width` and
#' `volume` arguments to be named to ensure deliberate specification.
#' @return A modified cross section object.
#' @export
xt_widen <- function(cross_section, ..., width, volume, side = "both") {
  UseMethod("xt_widen")
}

#' @export
xt_widen.sx_1d <- function(cross_section, ..., width, volume, side = "both") {
  if (!missing(volume)) {
    stop("Cannot widen a 1D cross section by volume. Use `width` instead.")
  }
  if (missing(width)) {
    stop("`width` must be specified for 1D cross sections.")
  }

  prop_left <- parse_side_arg(side, cross_section)

  # Call the internal workhorse function
  xt_widen_width_1d(cross_section, dw = width, prop_left = prop_left)
}

#' @export
xt_widen.sx_2d <- function(cross_section, ..., width, volume, side = "both") {
  if (!xor(missing(width), missing(volume))) {
    stop("Must specify either `width` or `volume`, but not both.")
  }
  prop_left <- parse_side_arg(side, cross_section)
  if (missing(volume)) {
    return(xt_widen_width_2d(cross_section, dw = width, prop_left = prop_left))
  }
  xt_widen_volume_2d(cross_section, volume = volume, prop_left = prop_left)
}

#' @export
xt_widen.sx <- function(cross_section, ..., width, volume, side = "both") {
  prof <- xt_geometry_profile(cross_section)
  plan <- xt_geometry_plan(cross_section)
  prof <- xt_widen(prof, ..., width = width, volume = volume, side = side)
  plan <- xt_widen(plan, ..., width = width, volume = volume, side = side)
  xt_geometry_profile(cross_section) <- prof
  xt_geometry_plan(cross_section) <- plan
  cross_section
}
