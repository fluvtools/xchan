#' Get one cross section by row index
#'
#' Extract a single cross section from a channel's profile view using
#' row order.
#'
#' @param channel A channel object.
#' @param i Single positive integer row index.
#' @returns One cross section, `xchannel` object.
#' @examples
#' coords <- matrix(c(-2, 10, 0, 8, 2, 10), ncol = 2, byrow = TRUE)
#' xs <- xchan:::new_profile(coords, bankpoints = c(-2, 2))
#' ch <- xt_as_channel(c(4, 4), profile = list(xs, xs), crs = 3005)
#' xt_profile_at(ch, 1)
#' @export
xt_xsection_at <- function(channel, i) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_int(i, lower = 1)
  channel[[i]]
}
