#' Construct a channel object (`xchan`)
#'
#' Assembles a channel table from **required** planimetric cross sections
#' (`sfc_LINESTRING`), optional profile cross sections (`xs_profile` list), and
#' optional extra columns via `...`, in the same spirit as [sf::st_sf()]. To
#' **coerce** widths, geometries, or an existing table instead, see
#' [xt_as_channel()] (extra columns via `...` apply when building from
#' widths/`sfc`; add columns on the data frame first when coercing a table).
#'
#' Every channel has a plan column; profile is optional. Set CRS on `.plan`
#' with [sf::st_crs()] / [sf::st_set_crs()] before calling if needed.
#'
#' When `.profile` is supplied, `length(.profile)` must equal `length(.plan)`.
#' Additional columns are recycled with [vctrs::vec_recycle_common()].
#'
#' @param .plan Planimetric cross sections as `sfc_LINESTRING`.
#' @param .profile Optional list of `xs_profile` objects, same length as
#'    `.plan`.
#' @param .axis Optional channel axis as a single `LINESTRING` (`sfc`/`sfg`);
#'   same CRS as `.plan`. Used by [xt_trace_centerline()] and related functions.
#' @param .chainage Optional numeric vector (length `length(.plan)`): distance
#'   along the channel from upstream to each section’s station (CRS units); see
#'   [xt_generate_plan()].
#' @param ... Additional columns for the channel table (vectors or
#'   list-columns).
#'
#' @returns An object of class `"xchan"`.
#'
#' @seealso [xt_as_channel()] to coerce from widths, existing tables, etc.
#'
#' @export
#' @examples
#' library(sf)
#' seg <- st_sfc(
#'   st_linestring(matrix(c(-1, 1, 0, 1), ncol = 2)),
#'   st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)),
#'   crs = 3005
#' )
#' xt_channel(.plan = seg)
xt_channel <- function(.plan, .profile = NULL, .axis = NULL, .chainage = NULL, ...) {
  if (!inherits(.plan, "sfc") || !inherits(.plan, "sfc_LINESTRING")) {
    stop("`.plan` must be an object of class `sfc_LINESTRING`.", call. = FALSE)
  }
  vp <- xt_validate_plan(.plan)
  if (!vp$valid) {
    stop(paste(vp$issues, collapse = "; "), call. = FALSE)
  }

  dots <- rlang::list2(...)
  dup <- intersect(names(dots), c("plan", "profile", "chainage"))
  if (length(dup)) {
    stop(
      "Arguments in `...` must not be named `plan` or `profile`; ",
      "use `.plan` and `.profile`.",
      call. = FALSE
    )
  }

  profile_col <- NULL
  if (!is.null(.profile)) {
    if (length(.plan) != length(.profile)) {
      stop("`length(.plan)` must equal `length(.profile)`.", call. = FALSE)
    }
    if (!is.list(.profile)) {
      stop("`.profile` must be a list.", call. = FALSE)
    }
    bad <- vapply(.profile, function(p) !inherits(p, "xs_profile"), logical(1))
    if (any(bad)) {
      stop(
        "All elements of `.profile` must inherit from `xs_profile`.",
        call. = FALSE
      )
    }
    profile_col <- "profile"
  }

  cols <- c(list(plan = .plan), dots)
  if (!is.null(.profile)) {
    cols$profile <- .profile
  }
  if (!is.null(.chainage)) {
    if (length(.chainage) != length(.plan)) {
      stop("`.chainage` must have length `length(.plan)`.", call. = FALSE)
    }
    cols$chainage <- .chainage
  }

  df <- rlang::exec(create_data_frame, !!!cols)
  axis_obj <- NULL
  if (!is.null(.axis)) {
    axis_obj <- validate_axis_sf(.axis, sf::st_crs(.plan))
  }
  out <- new_channel(df, plan_col = "plan", profile_col = profile_col, axis = axis_obj)
  xt_validate_plan_profile_widths(out)
  out
}
