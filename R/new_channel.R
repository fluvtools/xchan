#' Constructor function for channel objects
#'
#' @param l Object to turn into a channel.
#' @param plan_col Name of the plan column (or NULL if not present)
#' @param profile_col Name of the profile column (or NULL if not present)
#' @param ... Additional attributes to add to the object.
#' @param class If making a subclass, specify its name here.
#' @returns An object of class `"sxchan"`, which is a data frame with plan
#' and/or profile columns.
#' @noRd
new_channel <- function(l,
                        plan_col = NULL,
                        profile_col = NULL,
                        ...,
                        class = character()) {
  original_class <- class(l)

  # if (!is.null(plan_col)) attrs$plan_col <- plan_col
  # if (!is.null(profile_col)) attrs$profile_col <- profile_col

  structure(
    l,
    plan_col = plan_col,
    profile_col = profile_col,
    ...,
    class = c(class, "sxchan", original_class)
  )
}
