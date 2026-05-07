#' Linear unit of a CRS, as a string accepted by `units::set_units()`
#'
#' Returns the length unit that [sf::st_length()] would use for geometries with
#' the same CRS as `x`. Used to convert user-supplied lengths/volumes that
#' carry [units::units()] into bare numerics in the channel's own unit, and to
#' attach units to numeric lengths/volumes returned by package functions.
#'
#' @param x A `xchan`, `sf`/`sfc` object, CRS object, or anything else
#'   `[sf::st_crs()]` accepts. `xchan` objects use the plan column's CRS.
#' @returns A unit symbol (for example `"m"` or `"US_survey_foot"`) suitable
#'   for `units::set_units(..., mode = "standard")`, or `NULL` when no CRS is
#'   set, the CRS has no defined linear unit, or the `units` package is not
#'   available.
#' @noRd
crs_length_unit <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!requireNamespace("units", quietly = TRUE)) {
    return(NULL)
  }
  if (inherits(x, "xchan")) {
    pc <- attr(x, "plan_col", exact = TRUE)
    if (is.null(pc)) {
      return(NULL)
    }
    x <- x[[pc]]
  }
  crs <- tryCatch(sf::st_crs(x), error = function(e) NA)
  if (length(crs) == 0L || is.na(crs)) {
    return(NULL)
  }
  # Probe sf for the unit it would use; this guarantees the resulting symbol
  # round-trips through units::set_units() (it sometimes differs from
  # crs$units_gdal, e.g. "US_survey_foot" vs. "US survey foot").
  tryCatch(
    {
      dummy <- sf::st_sfc(
        sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2)),
        crs = crs
      )
      len <- suppressWarnings(sf::st_length(dummy))
      if (inherits(len, "units")) units::deparse_unit(len) else NULL
    },
    error = function(e) NULL
  )
}

#' Coerce a length argument to plain numeric in `target_unit`
#'
#' If `x` is a `units` object, convert it to `target_unit` and strip the
#' units. If `x` is plain numeric, return it untouched (assumed to already be
#' in `target_unit`). If `target_unit` is `NULL`, units are dropped without
#' conversion (we have no reference unit to convert into; the user is
#' responsible for consistency). `NULL` input passes through.
#'
#' Errors clearly when the input has units that are not convertible to
#' `target_unit` (for example, a `dw` of `"kg"` against a metric channel).
#'
#' @noRd
to_numeric_length <- function(x, target_unit = NULL, arg = "value") {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "units")) {
    if (!is.null(target_unit)) {
      x <- tryCatch(
        units::set_units(x, target_unit, mode = "standard"),
        error = function(e) {
          stop(
            "`",
            arg,
            "` has units incompatible with the channel's length unit (`",
            target_unit,
            "`): ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
    }
    return(units::drop_units(x))
  }
  as.numeric(x)
}

#' Coerce a volume argument to plain numeric in `target_unit^3`
#'
#' Like `to_numeric_length()`, but the implied target unit is
#' `paste0(target_unit, "^3")`. The numeric volume returned is therefore in
#' cubic length units, so it composes correctly with widths and depths
#' measured in `target_unit`.
#'
#' @noRd
to_numeric_volume <- function(x, target_unit = NULL, arg = "value") {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "units")) {
    if (!is.null(target_unit)) {
      vol_unit <- paste0(target_unit, "^3")
      x <- tryCatch(
        units::set_units(x, vol_unit, mode = "standard"),
        error = function(e) {
          stop(
            "`",
            arg,
            "` has units incompatible with the channel's volume unit (`",
            vol_unit,
            "`): ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
    }
    return(units::drop_units(x))
  }
  as.numeric(x)
}

#' Attach a length unit to a numeric vector, when one is available
#'
#' Wraps `units::set_units()` with `NULL`-tolerant behaviour: if `unit` is
#' `NULL` (no CRS unit known) or the `units` package is unavailable, returns
#' `x` unchanged. Used to give length-bearing return values (for example,
#' from `xt_width()` or `xt_distance_ds()`) the same unit as the channel's
#' coordinate system, so downstream arithmetic stays unit-checked.
#'
#' @noRd
with_length_units <- function(x, unit = NULL) {
  if (is.null(unit)) {
    return(x)
  }
  if (!requireNamespace("units", quietly = TRUE)) {
    return(x)
  }
  units::set_units(x, unit, mode = "standard")
}

#' Attach a volume unit (`unit^3`) to a numeric vector, when available
#'
#' Volume convention follows the channel's length unit cubed, matching how
#' areas/volumes accumulate from cross sectional integrals across plan
#' geometry.
#'
#' @noRd
with_volume_units <- function(x, unit = NULL) {
  if (is.null(unit)) {
    return(x)
  }
  if (!requireNamespace("units", quietly = TRUE)) {
    return(x)
  }
  units::set_units(x, paste0(unit, "^3"), mode = "standard")
}
