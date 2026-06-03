#' Side Schemes
#'
#' Side functions determine how much of the widening applied to a channel is
#' allocated to the left and right banks. They return side objects as lists with
#' `left` and `right` proportions.
#'
#' @param prop A single numeric value between 0 and 1 indicating how much
#' of the widening to apply to the specified bank.
#' @param prop_left,prop_right Single numeric values between 0 and 1
#' indicating how much of the widening to apply to the left and right banks,
#' respectively.
#' @details
#' While these functions are different ways of specifying the same thing, they
#' are included for completeness. An advantage of using the `side_both()`
#' function is that it conducts an internal check that the proportions for the
#' left and right banks sum to 1.
#' @returns A side object that can be used in widening functions. It is a list
#'   with numeric entries `left` and `right`.
#' @rdname sides
#' @export
side_left <- function(prop = 1) {
  checkmate::assert_number(prop, lower = 0, upper = 1)
  structure(
    list(left = prop, right = 1 - prop),
    name = "left",
    params = list(prop = prop),
    class = "xchan_side"
  )
}

#' @rdname sides
#' @export
side_right <- function(prop = 1) {
  checkmate::assert_number(prop, lower = 0, upper = 1)
  structure(
    list(left = 1 - prop, right = prop),
    name = "right",
    params = list(prop = prop),
    class = "xchan_side"
  )
}

#' @rdname sides
#' @export
side_both <- function(prop_left = 0.5, prop_right = 0.5) {
  checkmate::assert_number(
    prop_left,
    lower = 0,
    upper = 1
  )
  checkmate::assert_number(
    prop_right,
    lower = 0,
    upper = 1
  )

  if (!isTRUE(all.equal(prop_left + prop_right, 1))) {
    stop("Proportions for `prop_left` and `prop_right` must sum to 1.")
  }

  structure(
    list(left = prop_left, right = prop_right),
    name = "both",
    params = list(prop_left = prop_left, prop_right = prop_right),
    class = "xchan_side"
  )
}
