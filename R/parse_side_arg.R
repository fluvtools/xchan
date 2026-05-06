#' Parse the `side` argument (internal)
#'
#' @noRd
#' @param side Same as `side` in [xt_widen()].
#' @param cross_sections Channel (`xchan`) or other object passed to the side
#'   function (must work with [xt_n_sections()]).
#' @return A vector of proportions for the left bank; one per cross section.
parse_side_arg <- function(side, cross_sections) {
  if (is.character(side)) {
    fn <- paste0("side_", side)
    side <- rlang::exec(fn)
  }
  checkmate::assert_class(side, "xchan_side")
  prop_left <- side(cross_sections)
  n_sections <- xt_n_sections(cross_sections)
  prop_left <- vctrs::vec_recycle(prop_left, size = n_sections)
  checkmate::assert_numeric(
    prop_left,
    lower = 0,
    upper = 1,
    any.missing = FALSE
  )
  prop_left
}
