#' Elevation specifications for channel profiles
#'
#' These functions construct **elevation specification** objects (class `"xchan_elevation"`).
#' An elevation specification is a small callable wrapper: passing it as `reference` to
#' [xt_elevation()] runs it against the channel and returns one elevation per cross section
#' (aligned with rows of the `"xchan_tbl"`).
#'
#' [elevation_thalweg()] takes no arguments: each section contributes its stored
#' `thalweg_elev` value — the minimum elevation among sampled profile vertices —
#' documented as a return component of [new_profile()].
#'
#' @section Profile geometry and naming:
#'
#' Objects of class `xs_profile` (from [new_profile()]) hold a matrix
#' `coordinates` with column 1 = **distance along the cross section** (left bank toward
#' right bank, increasing) and column 2 = **elevation**. **Bank** points are vertices on
#' that polyline flagged in `banks` (row indices); **thalweg** identifies the deepest
#' part of the traced section.
#'
#' In this vertical slice, **left** and **right** mean smaller vs larger distance along
#' the profile (typically left and right banks in map view when the section is oriented
#' consistently). [elevation_topo()] uses **every** sampled vertex on the polyline
#' (including distances outside the outer bank pair if the profile extends there).
#' [elevation_channel()] and [elevation_bottom()] use only vertices whose distance lies
#' **between** the **outer** left and right bank distances (the wetted / in-channel span).
#'
#' @section Parameters on “zero-arg” constructors:
#'
#' Specifications with no arguments ([elevation_thalweg()], [elevation_bank_left()], …)
#' are fully determined by convention; there is nothing to configure. Combine them with
#' [xt_elevation()] / [xt_gradient()], etc.
#'
#' @param .f Numeric summary function applied per cross section:
#'   * [elevation_bank()] — passes **two** values (left and right **outer bank** elevations)
#'     through `.f`; default [base::min] is the lower of the two outer bank elevations.
#'   * [elevation_channel()] — passes elevations at **all** profile vertices between the
#'     outer left and right **bank distances** through `.f`; default [base::mean].
#'   * [elevation_topo()] — passes **all** vertex elevations `coordinates[, 2]` through `.f`;
#'     default [base::mean] is the mean along the full traced polyline (not restricted to
#'     the bank-to-bank span).
#'   * [elevation_bottom()] — same in-channel vertex set as [elevation_channel()]; kept as a
#'     separate name for readability in long-profile / “bed” contexts.
#' @param ... Further arguments forwarded to `.f` (for example `probs` for [stats::quantile]).
#'
#' @returns An object inheriting `"xchan_elevation"`: a function `(channel)` that returns
#'   a numeric vector of elevations, one per row of `channel`. Also used as `reference`
#'   in other package functions expecting an elevation specification.
#'
#' @rdname elevations
#' @aliases elevations
#' @seealso [xt_elevation()], [xt_gradient()], [new_profile()]
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

#' @describeIn elevations Banks: apply `.f` to the elevations at the **left and right outer
#'   bank** vertices (same points as [elevation_bank_left()] / [elevation_bank_right()]).
#'   Default `.f = min` chooses the lower of the two outer bank elevations.
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

#' @describeIn elevations Aggregate vertex elevations **between** the outer left and right
#'   bank distances (in-channel span) with `.f`; default `.f = mean`. Unlike
#'   [elevation_topo()], excludes profile tails outside both outer banks.
#' @export
elevation_channel <- function(.f = mean, ...) {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "channel (in-bank) elevations."
      )
    }
    profile <- channel_profile(channel)
    vapply(
      profile,
      function(xs) aggregate_elev_between_outer_banks(xs, .f, ...),
      numeric(1)
    )
  }
  structure(
    fun,
    name = "channel",
    params = list(.f = .f),
    class = "xchan_elevation"
  )
}

#' @describeIn elevations Aggregate **every** sampled vertex elevation on the profile
#'   (`coordinates[, 2]`) with `.f`; default `.f = mean`. Use another summary (for example
#'   [stats::median] or [base::min]) when a single statistic of the traced polyline is
#'   desired. Differs from [elevation_bank()], which only uses the **two outer bank**
#'   elevations, and from [elevation_channel()], which restricts to the bank-to-bank span.
#' @export
elevation_topo <- function(.f = mean, ...) {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "topography height."
      )
    }
    profile <- channel_profile(channel)
    vapply(
      profile,
      function(xs) .f(xs$coordinates[, 2], ...),
      numeric(1)
    )
  }
  structure(
    fun,
    name = "topo",
    params = list(.f = .f),
    class = "xchan_elevation"
  )
}

#' Create a bottom elevation specification
#'
#' Applies `.f` to elevations of vertices **between** the outer left and right bank
#' distances along the profile (same logic as [elevation_channel()]).
#'
#' @param .f Summary function ([base::min], [base::max], [base::mean], [stats::quantile], ...).
#' @param ... Passed to `.f` (for example `probs` with [stats::quantile]).
#'
#' @returns An elevation specification; see unified topic `?elevations` alongside
#'   [elevation_thalweg()] and neighbours.
#' @seealso [elevation_channel()] on this same help page.
#' @export
elevation_bottom <- function(.f = mean, ...) {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "bottom elevations."
      )
    }
    profile <- channel_profile(channel)
    vapply(
      profile,
      function(xs) aggregate_elev_between_outer_banks(xs, .f, ...),
      numeric(1)
    )
  }
  structure(
    fun,
    name = "Bottom Elevation",
    params = c(list(.f = .f), list(...)),
    class = "xchan_elevation"
  )
}

#' @noRd
aggregate_elev_between_outer_banks <- function(xs, .f, ...) {
  checkmate::assert_class(xs, "xs_profile")
  left_bank_coords <- get_left_bank_coords(xs)
  right_bank_coords <- get_right_bank_coords(xs)
  left_bank_dist <- left_bank_coords[1]
  right_bank_dist <- right_bank_coords[1]
  d <- xs$coordinates[, 1]
  in_channel <- d >= left_bank_dist & d <= right_bank_dist
  elev <- xs$coordinates[in_channel, 2]
  .f(elev, ...)
}
