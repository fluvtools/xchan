#' Check if a channel has profile or plan-view cross sections
#'
#' @param x A channel object.
#' @returns Single logical; `TRUE` if the channel has the requested
#' cross section type (i.e., the column exists in the channel data frame
#' and is being tracked), `FALSE` otherwise.
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
