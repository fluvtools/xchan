#' Calculate channel gradient
#'
#' Compares elevations between cross sections along the channel axis (see
#' **Details**). A gradient is undefined for a single cross section (there is no
#' along-channel segment), so there is no method for [`xsection`][xsection()] —
#' use at least two stations in an [`xchan`][xchan()].
#'
#' @param channel An [`xchan`][xchan()] with one or more cross sections (length
#'   `n`).
#'   With `n == 1`, every gradient is `NA` because no segment exists.
#' @param ... Must be empty.
#' @param before Number of cross sections **before** the current index to
#'   include as the
#'   **start** of the segment (inclusive). The gradient uses elevations and axis
#'   distances at indices `i - before` and `i + after` when the window is valid.
#' @param after Number of cross sections **after** the current index to include
#'   as the
#'   **end** of the segment (inclusive).
#' @param complete If `TRUE`, allow **truncated** windows at the upstream and
#'   downstream
#'   ends of the channel: for station `i`, the window is clipped to `[1, n]` so
#'   the segment runs from `max(1, i - before)` to `min(n, i + after)`. Every
#'   station gets a value whenever that clipped window spans at least two
#'   distinct stations (otherwise `NA`).
#'
#'   If `FALSE`, require a **full** window: station `i` is only computed when `i
#'   - before >= 1` and `i + after <= n`. Otherwise the value is `NA`. So with
#'   `before = 1` and `after = 1`, only interior stations `2, ..., n-1` are
#'   filled; you get exactly **one** `NA` at the front (first station) and
#'   **one** at the end (last station), not two at either boundary.
#' @param elevation Elevation specification ([elevation_thalweg()],
#'   [elevation_bank()], …).
#' @param axis Optional channel axis (same interpretation as
#'   [xt_distance_downstream()]; distances
#'   are computed without attaching units so the gradient ratio stays
#'   dimensionless).
#' @returns Numeric vector of gradients (length matches number of cross
#'   sections), unitless
#'   even when axis distances carry units.
#'
#' @details
#' At each station `i`, the gradient is `(z_end - z_start) / (s_end - s_start)`
#' where `z` comes from [xt_elevation()] and `s` from downstream distance along
#' the axis (same convention as [xt_distance_downstream()], stored as plain
#' numeric for the ratio).
#'
#' @examples
#' channel <- xt_as_channel(rep(1, 6))
#' channel <- xt_add_profile(
#'   channel,
#'   distance = distance,
#'   elevation = elevation,
#'   section = id,
#'   banks = is_bank,
#'   data = profile_survey
#' )
#' gradient <- xt_gradient(channel, elevation = elevation_thalweg())
#'
#' # Interior stations only (one NA first and last when before = after = 1)
#' gradient <- xt_gradient(channel, before = 1L, after = 1L, complete = FALSE)
#'
#' # Smoothed using a wider full window
#' gradient <- xt_gradient(channel, before = 2L, after = 2L, elevation = elevation_bank(.f = mean))
#' @export
xt_gradient <- function(
  channel,
  ...,
  before = 1L,
  after = 1L,
  complete = FALSE,
  elevation = elevation_bank(),
  axis = NULL
) {
  rlang::check_dots_empty()
  UseMethod("xt_gradient")
}

#' @rdname xt_gradient
#' @usage NULL
#' @export
xt_gradient.xchan <- function(
  channel,
  ...,
  before = 1L,
  after = 1L,
  complete = FALSE,
  elevation = elevation_bank(),
  axis = NULL
) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan")

  elevations <- xt_elevation(channel, reference = elevation)
  distances <- axis_distances_numeric(channel, axis)

  gradient_sliding_window(elevations, distances, before, after, complete)
}

#' @rdname xt_gradient
#' @usage NULL
#' @exportS3Method xt_gradient default
xt_gradient.default <- function(
  channel,
  ...,
  before = 1L,
  after = 1L,
  complete = FALSE,
  elevation = elevation_bank(),
  axis = NULL
) {
  stop(
    "No `xt_gradient()` method for class ",
    paste(class(channel), collapse = "/"),
    ". Use an `xchan` object (gradient needs multiple cross sections).",
    call. = FALSE
  )
}

#' @noRd
gradient_sliding_window <- function(
  elevations,
  distances,
  before,
  after,
  complete
) {
  n <- length(elevations)
  gradient <- rep(NA_real_, n)
  checkmate::assert_numeric(elevations, len = n, any.missing = TRUE)
  checkmate::assert_numeric(distances, len = n, any.missing = TRUE)

  before <- as.integer(before)[1L]
  after <- as.integer(after)[1L]
  checkmate::assert_int(before, lower = 0L)
  checkmate::assert_int(after, lower = 0L)

  for (i in seq_len(n)) {
    if (complete) {
      start_idx <- max(1L, i - before)
      end_idx <- min(n, i + after)
    } else {
      if (i < before + 1L || i > n - after) {
        next
      }
      start_idx <- i - before
      end_idx <- i + after
    }

    if (end_idx <= start_idx) {
      next
    }

    delta_elevation <- elevations[end_idx] - elevations[start_idx]
    delta_distance <- distances[end_idx] - distances[start_idx]
    if (!is.finite(delta_distance) || delta_distance == 0) {
      gradient[i] <- NA_real_
    } else {
      gradient[i] <- delta_elevation / delta_distance
    }
  }

  gradient
}
