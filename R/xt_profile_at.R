#' Get one cross section or its profile by row index
#'
#' `xt_xsection_at()` extracts a single [`xsection`] by row order.
#' `xt_profile_at()` returns the embedded `xs_profile` for that section (and
#' requires profile geometry on the channel).
#'
#' @param channel A channel object ([`xchan`]).
#' @param i Single positive integer row index.
#' @returns `xt_xsection_at()`: one [`xsection`]. `xt_profile_at()`: one
#'   `xs_profile`.
#'
#' @name xt_xsection_at
#' @examples
#' coords <- matrix(c(-2, 10, 0, 8, 2, 10), ncol = 2, byrow = TRUE)
#' xs <- xchan:::new_profile(coords, bankpoints = c(-2, 2))
#' ch <- xchan:::set_channel_profile(xt_as_channel(c(4, 4), crs = 3005), list(xs, xs))
#' xt_xsection_at(ch, 1)
#' xt_profile_at(ch, 1)
#' @export
xt_xsection_at <- function(channel, i) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_int(i, lower = 1L, .var.name = "i")
  if (i > length(channel)) {
    stop("`i` is out of bounds for this channel.", call. = FALSE)
  }
  channel[[i]]
}

#' @rdname xt_xsection_at
#' @export
xt_profile_at <- function(channel, i) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_int(i, lower = 1L, .var.name = "i")
  if (i > length(channel)) {
    stop("`i` is out of bounds for this channel.", call. = FALSE)
  }
  if (!xt_has_profile(channel)) {
    stop("Channel object must have profile cross sections", call. = FALSE)
  }
  channel[[i]]$profile
}
