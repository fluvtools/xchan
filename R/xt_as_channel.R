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
#' @param spacing Numeric scalar used by the `numeric` method only when `axis` is
#'   `NULL`: spacing between consecutive synthetic cross sections. If both
#'   `axis` and `spacing` are supplied, an error is raised. When `axis` is
#'   `NULL` and `spacing` is omitted, `spacing = 1` is used.
#' @param crs For `numeric`, `sfc`, and `data.frame` methods:
#'   CRS applied to plan geometries via [sf::st_set_crs()]. `NULL` leaves
#'   existing CRS unchanged.
#' @export
xt_as_channel.numeric <- function(
  x,
  ...,
  profile = NULL,
  crs = NULL,
  axis = NULL,
  spacing = NULL
) {
  checkmate::assert_numeric(x, lower = 0, any.missing = FALSE)
  spacing_supplied <- !missing(spacing) && !is.null(spacing)
  if (!is.null(axis) && spacing_supplied) {
    stop("`spacing` cannot be supplied when `axis` is provided.", call. = FALSE)
  }
  if (is.null(axis)) {
    if (is.null(spacing)) {
      spacing <- 1
    }
    checkmate::assert_number(spacing, lower = .Machine$double.eps, finite = TRUE)
  }
  n <- length(x)
  station <- if (is.null(axis)) (seq_len(n) - 1) * spacing else NULL

  if (is.null(axis)) {
    plan <- sf::st_sfc(Map(
      function(w, y) {
        sf::st_linestring(matrix(c(-w / 2, w / 2, y, y), ncol = 2))
      },
      x,
      station
    ))
    if (!is.null(crs)) {
      plan <- sf::st_set_crs(plan, crs)
    }
    if (length(station) == 1L) {
      axis_y <- c(station[1] - spacing / 2, station[1] + spacing / 2)
    } else {
      axis_y <- station
    }
    axis_obj <- sf::st_sfc(
      sf::st_linestring(cbind(rep(0, length(axis_y)), axis_y)),
      crs = sf::st_crs(plan)
    )
  } else {
    axis_obj <- validate_axis_sf(axis, if (!is.null(crs)) crs else NULL)
    if (!is.null(crs)) {
      axis_obj <- sf::st_set_crs(axis_obj, crs)
    }
    plan <- build_plan_from_widths_axis(x, axis_obj)
  }

  args <- c(list(plan = plan), rlang::dots_list(...))
  df <- rlang::exec(create_data_frame, !!!args)
  prof_col <- NULL
  if (!is.null(profile)) {
    prof_col <- "profile"
    df$profile <- profile
  }
  out <- new_channel(df, plan_col = "plan", profile_col = prof_col, axis = axis_obj)
  xt_validate_plan_profile_widths(out)
  out
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.sfg <- function(x, ..., profile = NULL, crs = NULL, axis = NULL) {
  xt_as_channel(sf::st_sfc(x), ..., profile = profile, crs = crs, axis = axis)
}

#' @rdname xt_as_channel
#' @export
xt_as_channel.sfc <- function(x, ..., profile = NULL, crs = NULL, axis = NULL) {
  if (!is.null(crs)) {
    x <- sf::st_set_crs(x, crs)
  }
  if (!inherits(x, "sfc_LINESTRING")) {
    x <- sf::st_cast(x, "LINESTRING")
  }

  args <- c(list(plan = x), rlang::dots_list(...))
  df <- rlang::exec(create_data_frame, !!!args)
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
  ...,
  profile_col = NULL,
  crs = NULL,
  axis = NULL
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
xt_as_channel.xchan <- function(x, ..., crs = NULL) {
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

#' @noRd
build_plan_from_widths_axis <- function(widths, axis) {
  n <- length(widths)
  axis_len <- as.numeric(sf::st_length(axis))
  stations <- if (n == 1L) axis_len / 2 else seq(0, axis_len, length.out = n)
  eps <- max(axis_len * 1e-8, 1e-6)

  segs <- vector("list", n)
  for (i in seq_len(n)) {
    s <- stations[i]
    p <- sf::st_line_interpolate(axis, s)
    s0 <- max(0, s - eps)
    s1 <- min(axis_len, s + eps)
    if (s1 <= s0) {
      s0 <- max(0, s - axis_len * 1e-8)
      s1 <- min(axis_len, s + axis_len * 1e-8)
    }
    p0 <- sf::st_line_interpolate(axis, s0)
    p1 <- sf::st_line_interpolate(axis, s1)
    c0 <- sf::st_coordinates(p0)[1L, 1:2, drop = TRUE]
    c1 <- sf::st_coordinates(p1)[1L, 1:2, drop = TRUE]
    t <- c1 - c0
    nrm <- sqrt(sum(t^2))
    if (nrm < 1e-15) {
      t <- c(0, 1)
    } else {
      t <- t / nrm
    }
    left_n <- c(-t[2], t[1])
    ctr <- sf::st_coordinates(p)[1L, 1:2, drop = TRUE]
    half_w <- widths[i] / 2
    left <- ctr + half_w * left_n
    right <- ctr - half_w * left_n
    segs[[i]] <- sf::st_linestring(rbind(left, right))
  }
  sf::st_sfc(segs, crs = sf::st_crs(axis))
}
