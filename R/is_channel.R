#' Test if an object is a channel object.
#'
#' Checks whether an object inherits the "xchan" class.
#'
#' @param x An object.
#' @returns Logical; TRUE if the test passes.
#' @export
is_channel <- function(x) inherits(x, "xchan")
