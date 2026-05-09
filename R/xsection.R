#' Predicate tests for \code{xchan}, \code{xchan_tbl}, and \code{xsection}
#'
#' These functions test inheritance from a single class each. For tables or list
#' containers holding cross sections, see [xt_is_channel()] and [xt_is_cross_section()].
#'
#' @name is.xchan
#' @param x Any object.
#' @returns `TRUE` or `FALSE`.
#'
#' @seealso [xt_is_channel], [xt_is_cross_section]
#'
#' @examples
#' \donttest{
#' tbl <- xt_as_channel(c(10, 12, 11))
#' is.xchan_tbl(tbl)
#' xc <- tbl[[attr(tbl, "xsection_col", exact = TRUE)]]
#' is.xchan(xc)
#' xs <- xc[[1]]
#' is.xsection(xs)
#' }
#'
#' @export
is.xchan <- function(x) inherits(x, "xchan")

#' @rdname is.xchan
#' @export
is.xchan_tbl <- function(x) inherits(x, "xchan_tbl")

#' @rdname is.xchan
#' @export
is.xsection <- function(x) {
  inherits(x, "xsection")
}

#' @rdname is.xchan
#' @export
is_xsection <- function(x) {
  is.xsection(x)
}

#' Construct a single cross section (`xsection`)
#'
#' `xsection` stores one planimetric transect (required) and an optional profile
#' cross section. Plan geometry is stored as a numeric matrix of `(x, y)` pairs
#' with rows ordered from left bank to right bank.
#'
#' @param plan Matrix with 2 numeric columns (`x`, `y`) and at least 2 rows.
#' @param profile Optional `xs_profile` object.
#' @returns An object of class `"xsection"`.
#' @export
xsection <- function(plan, profile = NULL) {
  checkmate::assert_matrix(
    plan,
    mode = "numeric",
    ncols = 2L,
    min.rows = 2L,
    any.missing = FALSE
  )
  if (!is.null(profile)) {
    checkmate::assert_class(profile, "xs_profile")
  }
  structure(
    list(
      plan = plan,
      profile = profile
    ),
    class = "xsection"
  )
}

#' @noRd
assert_section_profiles_homogeneous <- function(sections) {
  if (!length(sections)) {
    return(invisible(sections))
  }
  has_prof <- vapply(
    sections,
    function(s) !is.null(s$profile),
    logical(1),
    USE.NAMES = FALSE
  )
  if (length(unique(has_prof)) <= 1L) {
    return(invisible(sections))
  }
  with_prof <- which(has_prof)
  without_prof <- which(!has_prof)
  stop(
    "Cross sections must either all include profile geometry or none; mixing ",
    "is not allowed. With profile at positions ",
    paste(with_prof, collapse = ", "),
    "; without profile at positions ",
    paste(without_prof, collapse = ", "),
    ".",
    call. = FALSE
  )
}

#' @noRd
assert_xchan_profile_homogeneity <- function(x) {
  checkmate::assert_class(x, "xchan")
  assert_section_profiles_homogeneous(x)
  invisible(x)
}

#' Construct a vector of cross sections (`xchan`)
#'
#' `xchan` is a list-like geometry container, analogous to an `sfc` object in
#' `{sf}`. CRS is stored once at the container level.
#'
#' @param sections A list of `xsection` objects.
#' @param crs Optional CRS accepted by [sf::st_crs()].
#' @returns An object of class `"xchan"` / `"xchan_geom"`.
#' @export
xchan <- function(sections, crs = NULL) {
  if (!is.list(sections)) {
    stop("`sections` must be a list of `xsection` objects.", call. = FALSE)
  }
  if (length(sections) > 0L) {
    bad <- !vapply(sections, is_xsection, logical(1))
    if (any(bad)) {
      stop(
        "All `sections` entries must be `xsection` objects. Bad indices: ",
        paste(which(bad), collapse = ", "),
        call. = FALSE
      )
    }
    assert_section_profiles_homogeneous(sections)
  }
  structure(
    sections,
    crs = if (is.null(crs)) NA else sf::st_crs(crs),
    class = c("xchan", "xchan_geom", "list")
  )
}

#' @exportS3Method base::print
print.xsection <- function(x, ...) {
  n_plan <- nrow(x$plan)
  has_prof <- !is.null(x$profile)
  cat("xsection\n")
  cat("  Plan vertices:", n_plan, "\n")
  if (has_prof) {
    cat("  Profile vertices:", nrow(x$profile$coordinates), "\n")
    cat("  Bank points:", length(x$profile$banks), "\n")
    cat("  Thalweg points:", length(x$profile$thalwegs), "\n")
  } else {
    cat("  Profile: none\n")
  }
  invisible(x)
}

#' @noRd
xsection_to_linestring <- function(xs) {
  checkmate::assert_class(xs, "xsection")
  sf::st_linestring(xs$plan)
}

#' @noRd
xsection_from_linestring <- function(plan_line, profile = NULL) {
  if (!inherits(plan_line, "sfg")) {
    stop("`plan_line` must be an sf geometry (`sfg`).", call. = FALSE)
  }
  xy <- sf::st_coordinates(plan_line)[, 1:2, drop = FALSE]
  xsection(xy, profile = profile)
}

#' @noRd
xchan_from_plan_profile <- function(plan, profile = NULL) {
  checkmate::assert_class(plan, "sfc_LINESTRING")
  if (!is.null(profile)) {
    if (!is.list(profile) || length(profile) != length(plan)) {
      stop("`profile` must be a list of same length as `plan`.", call. = FALSE)
    }
    bad <- !vapply(profile, function(p) inherits(p, "xs_profile"), logical(1))
    if (any(bad)) {
      stop(
        "All profile entries must be `xs_profile`. Bad indices: ",
        paste(which(bad), collapse = ", "),
        call. = FALSE
      )
    }
  }
  secs <- vector("list", length(plan))
  for (i in seq_along(plan)) {
    p <- if (is.null(profile)) NULL else profile[[i]]
    secs[[i]] <- xsection_from_linestring(plan[[i]], profile = p)
  }
  xchan(secs, crs = sf::st_crs(plan))
}

#' @noRd
xchan_crs <- function(x) {
  if (inherits(x, "xchan")) {
    return(attr(x, "crs", exact = TRUE))
  }
  if (inherits(x, "xchan_tbl")) {
    col <- attr(x, "xsection_col", exact = TRUE)
    if (is.null(col) || !col %in% names(x)) {
      return(NA)
    }
    return(attr(x[[col]], "crs", exact = TRUE))
  }
  NA
}

#' @noRd
`xchan_crs<-` <- function(x, value) {
  crs <- if (is.null(value)) NA else sf::st_crs(value)
  if (inherits(x, "xchan")) {
    attr(x, "crs") <- crs
    return(x)
  }
  if (inherits(x, "xchan_tbl")) {
    col <- attr(x, "xsection_col", exact = TRUE)
    if (!is.null(col) && col %in% names(x)) {
      attr(x[[col]], "crs") <- crs
    }
    return(x)
  }
  stop("Unsupported object for CRS replacement.", call. = FALSE)
}

#' @noRd
xchan_to_plan <- function(x) {
  checkmate::assert_class(x, "xchan")
  geoms <- lapply(x, xsection_to_linestring)
  cr <- xchan_crs(x)
  crs_use <- if (inherits(cr, "crs")) {
    cr
  } else {
    suppressWarnings(sf::st_crs(cr))
  }
  if (inherits(crs_use, "crs") && is.na(crs_use)) {
    crs_use <- sf::NA_crs_
  }
  sf::st_sfc(geoms, crs = crs_use)
}

#' @noRd
xchan_to_profile <- function(x) {
  checkmate::assert_class(x, "xchan")
  assert_xchan_profile_homogeneity(x)
  prof <- lapply(x, function(xs) xs$profile)
  if (all(vapply(prof, is.null, logical(1)))) {
    return(NULL)
  }
  prof
}

#' @noRd
xchan_with_plan <- function(x, plan) {
  checkmate::assert_class(x, "xchan")
  checkmate::assert_class(plan, "sfc_LINESTRING")
  if (length(plan) != length(x)) {
    stop("Plan length must match number of sections.", call. = FALSE)
  }
  out <- x
  for (i in seq_along(out)) {
    out[[i]]$plan <- sf::st_coordinates(plan[[i]])[, 1:2, drop = FALSE]
  }
  attr(out, "crs") <- sf::st_crs(plan)
  out
}

#' @noRd
xchan_with_profile <- function(x, profile) {
  checkmate::assert_class(x, "xchan")
  if (is.null(profile)) {
    out <- x
    for (i in seq_along(out)) {
      out[[i]]$profile <- NULL
    }
    return(out)
  }
  if (!is.list(profile) || length(profile) != length(x)) {
    stop("`profile` must be a list matching section count.", call. = FALSE)
  }
  bad <- !vapply(profile, function(p) inherits(p, "xs_profile"), logical(1))
  if (any(bad)) {
    stop(
      "All profile entries must be `xs_profile`. Bad indices: ",
      paste(which(bad), collapse = ", "),
      call. = FALSE
    )
  }
  out <- x
  for (i in seq_along(out)) {
    out[[i]]$profile <- profile[[i]]
  }
  assert_xchan_profile_homogeneity(out)
  out
}
