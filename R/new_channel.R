#' Constructor for channel tables (`xchan_tbl`)
#'
#' \itemize{
#'   \item If \code{l} is \code{sfc_LINESTRING}, builds a single-column (plus
#'     \code{...}) table with an \code{xsection} column from plan (\code{l}) and
#'     optional \code{profile}. Names in \code{...} become extra data-frame
#'     columns.
#'   \item Otherwise \code{l} must be a data frame (or coercible) that already
#'     contains the cross-section column; \code{...} passes extra attributes
#'     onto the channel object via \code{\link{structure}} (existing behaviour).
#' }
#'
#' @param l An \code{sfc_LINESTRING}, or a data frame with a cross-section column.
#' @param xsection_col Name of the cross-section geometry column (wrap path only).
#' @param axis Optional channel axis as \code{sfc} LINESTRING (length 1); see \code{\link{xt_axis}}.
#' @param ... Wrap path: extra attributes on the channel object. Assembly path
#'   (\code{l} is \code{sfc_LINESTRING}): extra columns for the channel table.
#' @param class Subclass names prepended to \code{xchan_tbl}.
#' @param profile Optional list of \code{xs_profile} objects (assembly path only);
#'   same length as \code{l}.
#' @returns An object of class \code{"xchan_tbl"}.
#' @noRd
new_channel <- function(
  l,
  xsection_col = "xsection",
  axis = NULL,
  ...,
  class = character(),
  profile = NULL
) {
  if (inherits(l, "sfc") && inherits(l, "sfc_LINESTRING")) {
    vp <- validate_plan(l)
    if (!vp$valid) {
      stop(paste(vp$issues, collapse = "; "), call. = FALSE)
    }

    dots <- rlang::list2(...)
    dup <- intersect(names(dots), c("plan", "profile", "xsection"))
    if (length(dup)) {
      stop(
        "Extra columns must not be named `plan`, `profile`, or `xsection`.",
        call. = FALSE
      )
    }

    xsec <- xchan_from_plan_profile(l, profile)
    xsec <- `xchan_crs<-`(xsec, sf::st_crs(l))
    cols <- c(list(xsection = xsec), dots)
    df <- rlang::exec(create_data_frame, !!!cols)
    axis_obj <- if (!is.null(axis)) validate_axis_sf(axis, sf::st_crs(l)) else NULL
    out <- wrap_xchan_tbl(df, xsection_col = "xsection", axis = axis_obj, class = class)
    validate_plan_profile_widths(out)
    return(out)
  }

  if (!is.null(profile)) {
    stop(
      "`profile` is only used when `l` is an `sfc_LINESTRING` object.",
      call. = FALSE
    )
  }

  wrap_xchan_tbl(l, xsection_col = xsection_col, axis = axis, ..., class = class)
}

#' @noRd
wrap_xchan_tbl <- function(
  l,
  xsection_col = "xsection",
  axis = NULL,
  ...,
  class = character()
) {
  original_class <- class(l)
  if (!xsection_col %in% names(l)) {
    stop("Cross-section column `", xsection_col, "` not found.", call. = FALSE)
  }

  structure(
    l,
    xsection_col = xsection_col,
    axis = axis,
    ...,
    class = c(class, "xchan_tbl", original_class)
  )
}
