#' Distribute channel widening proportions
#'
#' A family of helper functions to define how widening proportions (`_p`) are
#' distributed between left and right banks.
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
#' the `distribute_p_both()` function is that is conducts an internal check
#' that the proportions for the left and right banks sum to 1.
#' @return A list object with a `prop_left` element, which is a numeric vector
#' indicating the proportion of the widening to apply to the left bank.
#' @rdname distribute_p_schemes
#' @export
distribute_p_left <- function(prop = 1) {
  checkmate::assert_numeric(prop, lower = 0, upper = 1, any.missing = FALSE)
  list(prop_left = prop)
}

#' @rdname distribute_p_schemes
#' @export
distribute_p_right <- function(prop = 1) {
  checkmate::assert_numeric(prop, lower = 0, upper = 1, any.missing = FALSE)
  # Invert the proportions for the left-side scheme.
  distribute_p_left(prop = 1 - prop)
}

#' @rdname distribute_p_schemes
#' @export
distribute_p_both <- function(prop_left = 0.5, prop_right = 0.5) {
  checkmate::assert_numeric(
    prop_left, lower = 0, upper = 1, any.missing = FALSE
  )
  checkmate::assert_numeric(
    prop_right, lower = 0, upper = 1, any.missing = FALSE
  )

  if (!isTRUE(all.equal(prop_left + prop_right, rep(1, length(prop_left))))) {
    stop("Proportions for `prop_left` and `prop_right` must sum to 1.")
  }

  list(prop_left = prop_left)
}
