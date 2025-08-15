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
    fn <- paste0("distribute_erosion_", side)
    side <- rlang::exec(fn)
  }
  checkmate::assert_list(side, types = "numeric", len = 1)
  prop_left <- side[[1]]
  checkmate::assert_numeric(
    prop_left, lower = 0, upper = 1, any.missing = FALSE
  )
  vctrs::vec_recycle(prop_left, size = xt_n_sections(cross_sections))
}
