#' Demote the class of a channel object
#'
#' This function strips away a channel's "xchan" class so that
#' only a tibble or data frame remains. All classes above "xchan"
#' are preserved.
#' @param channel A channel object of class `xchan`.
#' @returns The same `channel` object, with "xchan" class and its
#' subclasses removed.
demote_channel_class <- function(channel) {
  cl <- class(channel)
  i <- which(cl == "xchan")
  class(channel) <- cl[-(1:i)]
  channel
}
