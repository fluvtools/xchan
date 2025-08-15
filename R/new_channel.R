#' Constructor functions for cross section objects
#'
#' @param l Object to turn into a cross section.
#' @param ... Attributes to add to the object.
#' @param class If making a subclass, specify its name here.
#' @returns An object of class `"sxc"`, although not necessarily with
#' valid properties. The naming "sxc" is inspired by the sf package,
#' which names geometries that have features "sfc".
#' @rdname new_sx
new_channel <- function(l, ..., class = character()) {
  original_class <- class(l)
  structure(l, ..., class = c(class, "sxc", original_class))
}
