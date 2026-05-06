#' Calculate Erosion Width from Volume Change
#'
#' This function calculates the erosion width for each cross-section in a
#' channel given a specified volume change, distributing the change according
#' to a given scheme.
#'
#' @inheritParams xt_widen
#' @param dv Volume of erosion; single positive numeric or vector matching
#'   number of cross-sections.
#' @param error_on_overflow Logical; should an error be thrown if asked
#'   to calculate erosion width beyond cross section extent? `TRUE` if so
#'   (the default). If `FALSE`, returns the maximum width up to the extent.
#' @returns A numeric vector of erosion widths for each cross-section in the
#'   channel.
#' @examples
#' xt_erosion_width(channel, dv = 50, side = "left")
#' xt_erosion_width(channel, dv = 50, side = side_left(0.75))
#' @export
xt_erosion_width <- function(
  channel,
  dv,
  side = "both",
  error_on_overflow = TRUE
) {
  checkmate::assert_class(channel, "xchan")

  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }

  # Parse side argument to get proportions
  prop_left <- parse_side_arg(side, channel)

  # Recycle volume to match number of cross-sections
  dv <- vctrs::vec_recycle(dv, length(profile))

  # Calculate erosion width for each cross-section
  widths <- numeric()
  censored <- logical()

  for (i in seq_along(profile)) {
    xs <- profile[[i]]
    dv_left <- dv[i] * prop_left[i]
    dv_right <- dv[i] - dv_left

    dw1 <- xt_erosion_width_left(
      xs,
      dv_left,
      error_on_overflow = error_on_overflow
    )
    xs_flipped <- flip_profile(xs)
    dw2 <- xt_erosion_width_left(
      xs_flipped,
      dv_right,
      error_on_overflow = error_on_overflow
    )

    widths[i] <- dw1 + dw2
    censored[i] <- attr(dw1, "censored") || attr(dw2, "censored")
  }

  attr(widths, "censored") <- censored
  widths
}

xt_erosion_width_left <- function(xs, dv, error_on_overflow = TRUE) {
  checkmate::assert_numeric(dv, 0, len = 1, any.missing = FALSE)
  if (dv == 0) {
    dw <- 0
    attr(dw, "censored") <- FALSE
    return(dw)
  }
  # Get left bank information
  left_bank_coords <- get_left_bank_coords(xs)
  x_old <- left_bank_coords[1]
  y_bank <- left_bank_coords[2]

  # Get left side coordinates (negative distances)
  left_nodes <- xs$coordinates[
    xs$coordinates[, 1] <= x_old,
    ,
    drop = FALSE
  ]
  x_extent <- min(left_nodes[, 1])
  # Maximum width available for leftward erosion (positive number).
  dw_max_available <- x_old - x_extent

  # find_dx_for_volume_right() only searches in the +x direction, so mirror
  # the left-side topo across x = 0 (including x_old) to turn leftward
  # erosion into a rightward search. The function returns a delta in the
  # flipped frame, which is the erosion width directly.
  left_nodes_flipped <- left_nodes
  left_nodes_flipped[, 1] <- -left_nodes_flipped[, 1]
  dw <- tryCatch(
    find_dx_for_volume_right(
      v = dv,
      x0 = -x_old,
      topo = left_nodes_flipped,
      thalweg_height = y_bank,
      valley = "left"
    ),
    error = function(e) {
      if (
        grepl("Requested volume exceeds", conditionMessage(e), fixed = TRUE)
      ) {
        NA_real_
      } else {
        stop(e)
      }
    }
  )

  censored <- FALSE
  if (is.na(dw) || dw > dw_max_available) {
    if (error_on_overflow) {
      stop(
        "Cannot calculate erosion width for given change in volume, as ",
        "the cross section extent is surpassed."
      )
    } else {
      dw <- dw_max_available
      censored <- TRUE
    }
  }
  attr(dw, "censored") <- censored
  dw
}
