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
  checkmate::assert_class(channel, "sxchan")
  if (xt_has_plan(object)) {
    return(vapply(object, sf::st_length, FUNVALUE = numeric(1)))
  }
  vapply(object, xt_width_profile, FUNVALUE = numeric(1))
}

