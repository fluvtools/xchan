#' Get Channel Width
#'
#' Calculate the width of a channel object.
#'
#' @param channel Channel object.
#' @returns Numeric vector of widths of the channel's cross sections.
#' @examples
#' xt_width(demo_channel)
#' @export
xt_width <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  if (xt_has_plan(channel)) {
    plan <- xt_column_plan(channel)
    return(vapply(plan, sf::st_length, FUN.VALUE = numeric(1)))
  }
  prof <- xt_column_profile(channel)
  vapply(prof, xt_width_profile, FUN.VALUE = numeric(1))
}

