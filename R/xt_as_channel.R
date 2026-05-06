#' Coerce to a channel object (`xchan`)
#'
#' Convert widths, line geometries, or a data frame into a channel object.
#' This is the coercion analogue of [xt_channel()], which **constructs** a
#' channel from planimetric (and optional profile) columns. Coercion methods
#' always attach a plan column; profile remains optional.
#'
#' **Extra columns (`...`):** When the method **builds** the channel table from
#' widths or geometries (`numeric`, `sfc`, `sfg`), named arguments in `...`
#' become extra columns (same recycling as [xt_channel()]). When you **already
#' have a data frame**, add variables as columns on that table first, then call
#' `xt_as_channel()` --- `...` is not used there (see [xt_channel()] if you need
#' to assemble plan/profile from vectors in one call).
#'
#' @param x Object to coerce (numeric vector of widths, `sfc`, `data.frame`, or
#'   existing `xchan`).
#' @param ... Used only by `numeric`, `sfc`, and `sfg` methods: named arguments
#'   become extra columns in the channel table (recycled like [xt_channel()]).
#'   Not used when coercing a `data.frame` (must be empty). Ignored when coercing
#'   an existing `xchan`, with a warning if non-empty.
#'
#' @returns An object of class `"xchan"`.
#'
#' @seealso [xt_channel()]
#'
#' @examples
#' # Synthetic widths (integer positions along the channel)
#' xt_as_channel(c(10, 15, 12, 8))
#'
#' # Extra columns are recycled as in [base::data.frame()]
#' xt_as_channel(c(10, 15, 12, 8), section_id = c("A", "B", "C", "D"))
#'
#' library(sf)
#' seg <- st_sfc(
#'   st_linestring(matrix(c(-0.2, 0.3, 0.2, 1), nrow = 2, byrow = TRUE)),
#'   st_linestring(matrix(c(0.1, 0.1, 1, 1), nrow = 2, byrow = TRUE)),
#'   crs = 3005
#' )
#' xt_as_channel(seg)
#'
#' df <- data.frame(section = c("u", "v"), roughness = c(0.02, 0.03))
#' df$plan <- seg
#' # Extra variables live on `df` before coercing (not via `...`)
#' xt_as_channel(df, plan_col = "plan")
#'
#' @export
xt_as_channel <- function(x, ...) {
  UseMethod("xt_as_channel")
}

#' @rdname xt_as_channel
#' @param profile Optional list of `xs_profile` objects (same length as plan).
#' @param axis Optional channel axis (`sfc`/`sfg` LINESTRING, length 1); see [xt_axis()].
#'   Used when coercing from `numeric`, `sfc`, `sfg`, or `data.frame`.
#' @param crs For `numeric`, `sfc`, and `data.frame` methods:
#'   CRS applied to plan geometries via [sf::st_set_crs()]. `NULL` leaves
#'   existing CRS unchanged.
#' @export
xt_as_channel.numeric <- function(x, profile = NULL, ..., crs = NULL, axis = NULL) {
  checkmate::assert_numeric(x, lower = 0, any.missing = FALSE)

  plan <- Map(
    function(w, i) {
      sf::st_linestring(matrix(c(-w / 2, w / 2, i, i), ncol = 2))
    },
    x,
    seq_along(x)
  )
  plan <- sf::st_sfc(plan)
  if (!is.null(crs)) {
    plan <- sf::st_set_crs(plan, crs)
  }

  df <- create_data_frame(plan = plan, ...)
  prof_col <- NULL
  if (!is.null(profile)) {
    prof_col <- "profile"
    df$profile <- profile
  }
  crs_hint <- if (!is.null(crs)) crs else sf::st_crs(plan)
  axis_obj <- if (!is.null(axis)) validate_axis_sf(axis, crs_hint) else NULL
  out <- new_channel(df, plan_col = "plan", profile_col = prof_col, axis = axis_obj)
  xt_validate_plan_profile_widths(out)
  out
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.sfg <- function(x, profile = NULL, ..., crs = NULL, axis = NULL) {
  xt_as_channel(sf::st_sfc(x), profile = profile, ..., crs = crs, axis = axis)
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.sfc <- function(x, profile = NULL, ..., crs = NULL, axis = NULL) {
  if (!is.null(crs)) {
    x <- sf::st_set_crs(x, crs)
  }
  if (!inherits(x, "sfc_LINESTRING")) {
    x <- sf::st_cast(x, "LINESTRING")
  }

  df <- create_data_frame(plan = x, ...)
  prof_col <- NULL
  if (!is.null(profile)) {
    prof_col <- "profile"
    df$profile <- profile
  }
  axis_obj <- if (!is.null(axis)) validate_axis_sf(axis, sf::st_crs(x)) else NULL
  out <- new_channel(df, plan_col = "plan", profile_col = prof_col, axis = axis_obj)
  xt_validate_plan_profile_widths(out)
  out
}

#' @rdname xt_as_channel
#' @param plan_col Name of the column holding planimetric `sfc_LINESTRING`
#'   geometries (required).
#' @param profile_col Name of the profile list-column, if any.
#' @export
xt_as_channel.data.frame <- function(
  x,
  plan_col,
  profile_col = NULL,
  crs = NULL,
  axis = NULL,
  ...
) {
  if (...length() > 0) {
    stop("`...` is not used when coercing a data frame.")
  }
  if (missing(plan_col) || is.null(plan_col)) {
    stop(
      "`plan_col` must name the column containing planimetric cross sections."
    )
  }
  if (!plan_col %in% names(x)) {
    stop("Plan column '", plan_col, "' not found in data frame")
  }
  if (!is.null(profile_col) && !profile_col %in% names(x)) {
    stop("Profile column '", profile_col, "' not found in data frame")
  }

  if (!is.null(crs)) {
    x[[plan_col]] <- sf::st_set_crs(x[[plan_col]], crs)
  }

  axis_obj <- if (!is.null(axis)) {
    validate_axis_sf(axis, sf::st_crs(x[[plan_col]]))
  } else {
    NULL
  }

  out <- new_channel(x, plan_col = plan_col, profile_col = profile_col, axis = axis_obj)
  xt_validate_plan_profile_widths(out)
  out
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.xchan <- function(x, crs = NULL, ...) {
  if (...length() > 0) {
    warning(
      "extra arguments are ignored when coercing an existing `xchan` object",
      call. = FALSE
    )
  }
  if (is.null(crs)) {
    return(x)
  }
  pc <- attr(x, "plan_col", exact = TRUE)
  if (is.null(pc)) {
    return(x)
  }
  x[[pc]] <- sf::st_set_crs(x[[pc]], crs)
  x
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.default <- function(x, ...) {
  stop(
    "Can't coerce an object of class ",
    paste(class(x), collapse = "/"),
    " to a channel; use `xt_channel()` or define an S3 method.",
    call. = FALSE
  )
}
