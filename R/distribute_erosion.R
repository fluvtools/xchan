#' Split channel erosion between left and right banks
#'
#' A family of helper functions to define how widening proportions are
#' allocated between left and right banks.
#'
#' @param fun A function that takes cross_sections and returns left bank proportions
#' @param name Optional name for the splitter
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
#' the `splitter_both()` function is that it conducts an internal
#' check that the proportions for the left and right banks sum to 1.
#' @return A splitter object that can be used to specify how erosion is
#' distributed between left and right banks.
#' @rdname splitter_schemes
#' @export
new_splitter <- function(fun, name = NULL) {
  checkmate::assert_function(fun)
  checkmate::assert_character(name, len = 1, null.ok = TRUE)

  structure(
    fun,
    name = name,
    class = append("sxchan_splitter", class(fun))
  )
}

