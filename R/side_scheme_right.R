#' Specify Left or Right Widening Proportions
#'
#' Creates
#'
#' @param proportions A numeric vector of proportions (0 to 1).
#' @return An object of class `xt_side_scheme_spec`.
#' @export
side_scheme_left <- function(proportions = 1) {
  checkmate::assert_numeric(
    proportions, lower = 0, upper = 1, any.missing = FALSE
  )
  if (!is.numeric(proportions) || any(proportions < 0) || any(proportions > 1)) {
    stop("Proportions must be a numeric vector with values between 0 and 1.")
  }

  structure(
    list(prop_left = proportions, side = "left"),
    class = "xt_side_scheme_spec"
  )
}

#' @title Specify a right-side widening scheme
#' @description Creates an object that defines right-side widening proportions
#'   by inverting a left-side scheme.
#' @param proportions A numeric vector of proportions (0 to 1) for the right side.
#' @return An object of class `xt_side_scheme_spec`.
#' @export
side_scheme_right <- function(proportions = 1) {
  # Invert the proportions for the left-side function
  left_proportions <- 1 - proportions

  # Call the left-side function, which has the core validation logic
  side_scheme_left(proportions = left_proportions)
}
