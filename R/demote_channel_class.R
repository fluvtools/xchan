#' Demote the class of a channel object
#'
#' This function strips away a channel's "sxchan" class so that
#' only a tibble or data frame remains. All classes above "sxchan"
#' are preserved.
#' @param channel A channel object of class `sxchan`.
#' @returns The same `channel` object, with "sxchan" class and its
#' subclasses removed.
demote_channel_class <- function(channel) {
  cl <- class(channel)
  i <- which(cl == "sxchan")
  class(channel) <- cl[-(1:i)]
  channel
}
