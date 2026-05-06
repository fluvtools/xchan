#' Get the number of cross sections
#'
#' A generic function to determine the number of cross sections in an object.
#'
#' @param channel An object of class `xchan` representing a channel.
#' @return A single integer representing the number of cross sections.
#' @export
xt_n_sections <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  nrow(channel)
}
