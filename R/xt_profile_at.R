#' Get one profile cross section by row index
#'
#' Extract a single `xs_profile` object from a channel's profile column using
#' row order.
#'
#' @param channel A channel object.
#' @param i Single positive integer row index.
#' @returns One `xs_profile` object.
#' @examples
#' coords <- matrix(c(-2, 10, 0, 8, 2, 10), ncol = 2, byrow = TRUE)
#' xs <- xt_profile(coords, bankpoints = c(-2, 2))
#' ch <- xt_as_channel(c(4, 4), profile = list(xs, xs), crs = 3005)
#' xt_profile_at(ch, 1)
#' @export
xt_profile_at <- function(channel, i) {
  checkmate::assert_class(channel, "xchan")
  checkmate::assert_int(i, lower = 1)

  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop(
      "Channel object must have profile cross sections.",
      call. = FALSE
    )
  }

  n <- length(profile)
  if (i > n) {
    stop(
      "`i` is out of bounds for profile column of length ",
      n,
      ".",
      call. = FALSE
    )
  }

  xs <- profile[[i]]
  if (!inherits(xs, "xs_profile")) {
    stop("Selected profile is not an `xs_profile` object.", call. = FALSE)
  }

  xs
}
