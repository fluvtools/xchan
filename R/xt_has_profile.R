#' Check if a channel has profile cross sections
#'
#' @param x A channel object.
#' @returns For `xt_has_profile()`, `TRUE` if the channel has a profile column.
#'   Every `xchan` object has planimetric cross sections by definition.
#' @rdname xt_has
#' @export
xt_has_profile <- function(x) {
  col <- xt_column_profile(x)
  !is.null(col)
}
