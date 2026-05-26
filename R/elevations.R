#' Elevation specifications for channel profiles
#'
#' These functions construct **elevation specification** objects (class `"xchan_elevation"`).
#' An elevation specification is a small callable wrapper: passing it as `reference` to
#' [xt_elevation()] runs it against the channel and returns one elevation per cross section
#' (aligned with `[[i]]` storage order of the [`xchan`]).
#'
#' [elevation_thalweg()] takes no arguments: each section contributes its stored
#' `thalweg_elev` value — the minimum elevation among wetted profile vertices —
#' documented as a return component of each `xs_profile`.
#'
#' @section Profile geometry and naming:
#'
#' Objects of class `xs_profile` hold a matrix
#' `coordinates` with column 1 = **distance along the cross section** (left bank toward
#' right bank, increasing) and column 2 = **elevation**. **Bank** points are vertices on
#' that polyline flagged in `banks` (row indices); **thalweg** identifies the deepest
#' part of the traced section.
#'
#' In this vertical slice, **left** and **right** mean smaller vs larger distance along
#' the profile (typically left and right banks in map view when the section is oriented
#' consistently). `elevation_bed()` summarizes elevations at **stored profile vertices**
#' whose distance lies on the **wetted bed**: inside each **water** interval between
#' consecutive banks (alternating water / land / water along the section; see
#' [xt_add_profile()] and `xs_profile` structure). Dry islands and floodplain tails
#' outside the outer banks are excluded. The summary is applied to those vertex
#' elevations only — there is no interpolation along the bed, no distance weighting,
#' and no integration over width; default [base::mean] is an unweighted arithmetic mean
#' of the encoded `z` values (sampling density along the profile therefore affects it).
#'
#' @param .f Numeric summary function applied per cross section. Each call uses a single
#'   numeric vector `x` as the first argument to `.f` (not separate left/right arguments);
#'   further arguments are forwarded from `...`:
#'   * `elevation_bank()` — `x` has length 2: outer **left** then **right** bank elevations
#'     (`c(z_left, z_right)`). Default [base::min] returns the lower bank elevation;
#'     [base::mean] averages the two banks.
#'   * `elevation_bed()` — `x` is the vector of elevation (`z`) values at **encoded**
#'     profile vertices on the **wetted bed** (within each water interval between banks;
#'     islands excluded). Default [base::mean] is an unweighted mean of those values.
#' @param ... Further arguments forwarded to `.f` (for example `probs` for [stats::quantile]).
#'
#' @returns An object inheriting `"xchan_elevation"`: a function `(channel)` that returns
#'   a numeric vector of elevations, one per cross section in storage order (`[[i]]` of
#'   `channel` when it is an [`xchan`]). Also used as `reference`
#'   in other package functions expecting an elevation specification.
#'
#' @rdname elevations
#' @aliases elevations
#' @seealso [xt_elevation()], [xt_gradient()], [xt_add_profile()]
#' @export
elevation_thalweg <- function() {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "thalweg height."
      )
    }
    profile <- channel_profile(channel)
    vapply(
      profile,
      function(xs) xs$thalweg_elev,
      numeric(1)
    )
  }
  structure(fun, name = "thalweg", class = "xchan_elevation")
}

#' @describeIn elevations Banks: apply `.f(x, ...)` where `x` is `c(z_left, z_right)` at the
#'   **outer left** and **outer right** bank vertices (same points as [elevation_bank_left()]
#'   / [elevation_bank_right()]). Default `.f = min` returns the lower bank elevation.
#' @export
elevation_bank <- function(.f = min, ...) {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "bank height."
      )
    }
    profile <- channel_profile(channel)
    vapply(
      profile,
      function(xs) {
        z_left <- get_left_bank_coords(xs)[2]
        z_right <- get_right_bank_coords(xs)[2]
        .f(c(z_left, z_right), ...)
      },
      numeric(1)
    )
  }
  structure(
    fun,
    name = "bank",
    params = list(.f = .f),
    class = "xchan_elevation"
  )
}

#' @describeIn elevations Elevation (`z`) at the **outer left bank** profile vertex only.
#' @export
elevation_bank_left <- function() {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "bank height."
      )
    }
    profile <- channel_profile(channel)
    vapply(
      profile,
      function(xs) get_left_bank_coords(xs)[2],
      numeric(1L)
    )
  }
  structure(fun, name = "bank_left", class = "xchan_elevation")
}

#' @describeIn elevations Elevation (`z`) at the **outer right bank** profile vertex only.
#' @export
elevation_bank_right <- function() {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "bank height."
      )
    }
    profile <- channel_profile(channel)
    vapply(
      profile,
      function(xs) get_right_bank_coords(xs)[2],
      numeric(1L)
    )
  }
  structure(fun, name = "bank_right", class = "xchan_elevation")
}

#' @describeIn elevations Apply `.f(x, ...)` to encoded vertex elevations `x` on the
#'   **wetted bed** (within each water interval between banks; dry islands excluded).
#'   Vertices are included or excluded by distance along the profile only; `.f` does not
#'   interpolate or integrate along the bed. Default `.f = mean` is an unweighted mean of
#'   those `z` values.
#' @export
elevation_bed <- function(.f = mean, ...) {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "bed elevations.",
        call. = FALSE
      )
    }
    profile <- channel_profile(channel)
    vapply(
      profile,
      function(xs) aggregate_elev_on_wetted_bed(xs, .f, ...),
      numeric(1)
    )
  }
  structure(
    fun,
    name = "bed",
    params = list(.f = .f),
    class = "xchan_elevation"
  )
}

#' @noRd
aggregate_elev_on_wetted_bed <- function(xs, .f, ...) {
  checkmate::assert_class(xs, "xs_profile")
  bd <- get_bank_distances(xs)
  if (length(bd) %% 2L != 0L) {
    stop(
      "Expected an even number of bank points for wetted-bed aggregation (got ",
      length(bd),
      ").",
      call. = FALSE
    )
  }
  n_pairs <- length(bd) %/% 2L
  d <- xs$coordinates[, 1]
  on_bed <- rep_len(FALSE, length(d))
  for (k in seq_len(n_pairs)) {
    lo <- bd[2L * k - 1L]
    hi <- bd[2L * k]
    on_bed <- on_bed | (d >= lo & d <= hi)
  }
  elev <- xs$coordinates[on_bed, 2]
  if (!length(elev)) {
    stop(
      "No profile vertices on the wetted bed for this cross section.",
      call. = FALSE
    )
  }
  .f(elev, ...)
}
