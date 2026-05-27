#' Calculate Erosion Width from Volume Change
#'
#' This function calculates the erosion width for each cross-section in a
#' channel given a specified volume change, distributing the change according
#' to a given scheme.
#'
#' @inheritParams xt_widen
#' @param dv Volume of erosion; single positive value or vector matching the
#'   number of cross sections. Plain numeric is interpreted in the channel's
#'   CRS length unit cubed; a [units::units()] volume object is converted
#'   automatically (for example
#'   `units::set_units(c(20, 30), "L")` against a metric channel).
#' @returns A numeric vector of erosion widths for each cross-section in the
#'   channel, carrying [units::units()] when the channel has a CRS with a
#'   defined linear unit.
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
#' xt_erosion_width(channel, dv = 0.5, side = "left")
#' xt_erosion_width(channel, dv = 0.5, side = side_left(0.75))
#' @export
xt_erosion_width <- function(
  channel,
  dv,
  side = "both"
) {
  checkmate::assert_class(channel, "xchan")
  unit <- channel_length_unit(channel)
  dv <- to_numeric_volume(dv, unit, arg = "dv")
  raw <- erosion_width_numeric(channel, dv, side)
  with_length_units(raw, unit)
}

#' @noRd
erosion_width_numeric <- function(channel, dv, side = "both") {
  profile <- channel_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }

  # Parse side argument to get proportions
  prop_left <- parse_side_arg(side, channel)

  # Recycle volume to match number of cross-sections
  dv <- vctrs::vec_recycle(dv, length(profile))

  # Calculate erosion width for each cross-section
  widths <- numeric(length(profile))
  failures <- list()

  for (i in seq_along(profile)) {
    result <- tryCatch(
      {
        xs <- profile[[i]]
        dv_left <- dv[i] * prop_left[i]
        dv_right <- dv[i] - dv_left

        dw1 <- erosion_width_left(xs, dv_left)
        xs_flipped <- flip_profile(xs)
        dw2 <- erosion_width_left(xs_flipped, dv_right)

        list(ok = TRUE, value = dw1 + dw2)
      },
      error = function(e) {
        list(ok = FALSE, error = e)
      }
    )
    if (result$ok) {
      widths[i] <- result$value
    } else {
      failures[[length(failures) + 1L]] <- list(
        label = section_label_at(channel, i),
        message = conditionMessage(result$error)
      )
      widths[i] <- NA_real_
    }
  }

  stop_erosion_section_errors(failures)
  widths
}

erosion_width_left <- function(xs, dv) {
  checkmate::assert_numeric(dv, 0, len = 1, any.missing = FALSE)
  if (dv == 0) {
    return(0)
  }
  # Get left bank information
  left_bank_coords <- get_left_bank_coords(xs)
  x_old <- left_bank_coords[1]

  # Get left side coordinates (negative distances)
  left_nodes <- xs$coordinates[
    xs$coordinates[, 1] <= x_old,
    ,
    drop = FALSE
  ]
  x_extent <- min(left_nodes[, 1])
  # Maximum width available for leftward erosion (positive number).
  dw_max_available <- x_old - x_extent
  v_max <- erosion_volume_left(xs, dw_max_available)
  if (dv > v_max) {
    stop(
      "Cannot calculate erosion width for given change in volume, as ",
      "the cross section extent is surpassed."
    )
  }
  # Erosion volume is monotone in width, but widened sections can introduce
  # vertical cliffs and flat-bottom plateaus where a closed-form inverse based
  # on the left-bank topo is no longer reliable. Use bisection against the
  # actual volume helper so `xt_erosion_width()` stays consistent with the
  # geometry produced by `widen_profile_left()`.
  lo <- 0
  hi <- dw_max_available
  for (i in seq_len(60L)) {
    mid <- (lo + hi) / 2
    if (erosion_volume_left(xs, mid) >= dv) {
      hi <- mid
    } else {
      lo <- mid
    }
  }
  hi
}
