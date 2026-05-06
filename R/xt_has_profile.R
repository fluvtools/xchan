#' Check if a channel has profile or plan-view cross sections
#'
#' @param x A channel object.
#' @returns Single logical. For `xt_has_profile()` / `xt_has_plan()`, `TRUE` if
#' the channel has that column tracked. For `xt_has_chainage()`, `TRUE` if `x`
#' has a numeric **`chainage`** column with one non-missing value per row (see
#' [xt_generate_plan()]).
#' @rdname xt_has
#' @export
xt_has_profile <- function(x) {
  col <- xt_column_profile(x)
  !is.null(col)
}

#' @rdname xt_has
#' @export
xt_has_plan <- function(x) {
  col <- xt_column_plan(x)
  !is.null(col)
}

#' @rdname xt_has
#' @export
xt_has_chainage <- function(x) {
  checkmate::assert_class(x, "xchan")
  has_chainage_column(x)
}
