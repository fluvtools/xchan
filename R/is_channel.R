#' Test if an object is a cross section set.
#'
#' Checks whether an object inherits the "sxc" class.
#'
#' @param x An object.
#' @returns Logical; TRUE if the test passes.
#' @export
is_channel <- function(x) inherits(x, "sxc")
