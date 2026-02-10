#' Side Schemes
#'
#' Side functions determine how much of the widening applied to a channel
#' is allocated to the left and right banks. They are functions of cross
#' sections, and return a numeric vector of proportions indicating how much
#' of the widening should be applied to the left bank (left, by convention).
#'
#' @param prop Numeric vector of values between 0 and 1 indicating how much
#' of the widening to apply to the specified bank. Length 1 for constant
#' proportion for all cross sections; length equal to the number of
#' cross sections for varying proportions.
#' @param prop_left,prop_right Numeric vectors of values between 0 and 1
#' indicating how much of the widening to apply to the left and right banks,
#' respectively. Length 1 for constant proportion for all cross sections;
#' length equal to the number of cross sections for varying proportions.
#' @details
#' While these functions are different ways of specifying the same thing,
#' they are included for completeness. An advantage of using
#' the `side_both()` function is that it conducts an internal
#' check that the proportions for the left and right banks sum to 1.
#' @returns A side object that can be used in widening functions.
#' @rdname sides
#' @export
side_left <- function(prop = 1) {
  checkmate::assert_numeric(prop, lower = 0, upper = 1, any.missing = FALSE)
  f <- function(cross_sections) {
    vctrs::vec_recycle(prop, size = length(cross_sections))
  }
  structure(f, name = "left", params = list(prop = prop), class = "sxchan_side")
}

#' @rdname sides
#' @export
side_right <- function(prop = 1) {
  checkmate::assert_numeric(prop, lower = 0, upper = 1, any.missing = FALSE)
  f <- function(cross_sections) {
    vctrs::vec_recycle(1 - prop, size = length(cross_sections))
  }
  structure(f, name = "right", params = list(prop = prop), class = "sxchan_side")
}

#' @rdname sides
#' @export
side_both <- function(prop_left = 0.5, prop_right = 0.5) {
  checkmate::assert_numeric(
    prop_left, lower = 0, upper = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    prop_right, lower = 0, upper = 1, any.missing = FALSE
  )

  if (!isTRUE(all.equal(prop_left + prop_right, rep(1, length(prop_left))))) {
    stop("Proportions for `prop_left` and `prop_right` must sum to 1.")
  }

  f <- function(cross_sections) {
    vctrs::vec_recycle(prop_left, size = length(cross_sections))
  }
  structure(f, name = "both", params = list(prop_left = prop_left, prop_right = prop_right), class = "sxchan_side")
}
