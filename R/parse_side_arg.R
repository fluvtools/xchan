#' Parse the `side` argument in `xt_widen()`
#'
#' An internal helper function to parse and standardize the `side` argument
#' from the `xt_widen()` function.
#'
#' @param side The `side` argument from `xt_widen()`.
#' @param cross_sections The cross section object passed to `xt_widen()`.
#' @return A vector of proportions corresponding to the left bank; one
#' proportion for each cross section.
parse_side_arg <- function(side, cross_sections) {
  if (is.character(side)) {
    fn <- paste0("side_", side)
    side <- rlang::exec(fn)
  }
  checkmate::assert_class(side, "sxchan_side")
  prop_left <- side(cross_sections)
  prop_left <- vctrs::vec_recycle(prop_left, size = length(cross_sections))
  checkmate::assert_numeric(
    prop_left,
    lower = 0,
    upper = 1,
    any.missing = FALSE
  )
  prop_left
}
