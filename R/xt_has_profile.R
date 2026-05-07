#' Check if a channel has profile cross sections or chainage
#'
#' @param x A channel object.
#' @returns For `xt_has_profile()`, `TRUE` if the channel has a profile column.
#'   Every `xchan` object has planimetric cross sections by definition.
#'   For `xt_has_chainage()`, `TRUE` if `x` has a numeric **`chainage`** column
#'   with one non-missing value per row (see [xt_generate_plan()]).
#' @rdname xt_has
#' @export
xt_has_profile <- function(x) {
  col <- xt_column_profile(x)
  !is.null(col)
}

#' @rdname xt_has
#' @export
xt_has_chainage <- function(x) {
  checkmate::assert_class(x, "xchan")
  has_chainage_column(x)
}
