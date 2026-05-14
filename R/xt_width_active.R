#' Active Cross Section Widths
#'
#' Calculate the "active" width of channel cross sections, where an "active" width
#' is the width of the cross section occupied by water, not land. This is relevant when
#' there are islands / bars within the river.
#'
#' @param channel Channel object
#' @returns A numeric vector of length equal to the number of cross-sections in the channel
#' containing the active widths of each cross section.
#' @export
xt_width_active <- function(channel) {
  checkmate::assert_class(channel, "xchan")

  profile <- channel_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }

  widths <- numeric(length(profile))

  for (i in seq_along(profile)) {
    xs <- profile[[i]]

    bank_d <- get_bank_distances(xs)
    if (length(bank_d) %% 2L != 0L) {
      stop("Profile ", i, ": expected an even number of bank points.")
    }

    # Sum widths of water segments (pairs alternate land/water; see `new_profile()`).
    w_water <- 0
    for (j in seq_len(length(bank_d) / 2L)) {
      w_water <- w_water + (bank_d[2L * j] - bank_d[2L * j - 1L])
    }
    widths[i] <- w_water
  }

  widths
}
