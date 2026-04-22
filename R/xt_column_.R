#' Extract or replace the profile or plan column in a channel object
#'
#' This function allows you to extract or replace the profile or plan column
#' in a channel object of class `xchan`. The profile column contains
#' xs_profile objects, while the plan column contains sfc_LINESTRING objects.
#'
#' @param channel An object of class `xchan` containing cross-section data.
#' @param value For profile, a list of "xs_profile" objects; for plan, a list of
#' "sfc_LINESTRING" objects. If `NULL`, the column is removed.
#' @returns
#' For `xt_column_*()`, extracts the requested column from the
#' channel data frame. For plan view, this is an "sfc" object, which is a list
#' of sf_LINESTRINGs from the sf package. For profile view, this is a list of
#' of "xs_profile" objects.
#'
#' For `xt_column_*<-`, the original `channel` objects with the specified
#' column updated (or removed if `NULL`). If no plan or profile columns
#' remain, the "xchan" class and any subclasses are removed, leaving a data
#' frame (and its subclasses).
#' @rdname xt_column
xt_column_profile <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  profile_col <- attributes(channel)$profile_col
  if (is.null(profile_col)) {
    return(NULL)
  }
  channel[[profile_col]]
}

#' @rdname xt_column
xt_column_plan <- function(channel) {
  checkmate::assert_class(channel, "xchan")
  plan_col <- attributes(channel)$plan_col
  if (is.null(plan_col)) {
    return(NULL)
  }
  channel[[plan_col]]
}

#' @rdname xt_column
`xt_column_profile<-` <- function(channel, value) {
  checkmate::assert_class(channel, "xchan")
  profile_colname <- attributes(channel)$profile_col
  if (is.null(value)) {
    channel[[profile_colname]] <- NULL
    attributes(channel)$profile_colname <- NULL
    if (!xt_has_plan(channel)) {
      return(demote_channel_class(channel))
    }
    return(channel)
  }
  if (!is.list(value)) {
    stop("Profile value must be a list")
  }
  profile_classes <- vapply(value, inherits, logical(1), "xs_profile")
  if (!all(profile_classes)) {
    invalid_indices <- which(!profile_classes)
    stop(
      "All profile entries must be xs_profile objects. Invalid entries at indices: ",
      paste(invalid_indices, collapse = ", ")
    )
  }

  if (is.null(profile_colname)) {
    profile_colname <- "profile"
    attributes(channel)$profile_col <- profile_colname
  }

  channel[[profile_colname]] <- value
  channel
}

#' @rdname xt_column
`xt_column_plan<-` <- function(channel, value) {
  checkmate::assert_class(channel, "xchan")
  plan_colname <- attributes(channel)$plan_col
  if (is.null(value)) {
    channel[[plan_colname]] <- NULL
    attributes(channel)$plan_col <- NULL
    if (!xt_has_profile(channel)) {
      return(demote_channel_class(channel))
    }
    return(channel)
  }

  validation_result <- xt_validate_plan(value)
  if (!validation_result$valid) {
    stop("Invalid plan view cross sections: ", paste(validation_result$issues, collapse = "; "))
  }

  if (is.null(plan_colname)) {
    plan_colname <- "plan"
    attributes(channel)$plan_col <- plan_colname
  }
  channel[[plan_colname]] <- value
  channel
}
