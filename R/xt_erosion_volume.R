#' Calculate Erosion Volume from Width Change
#'
#' This function calculates the erosion volume for each cross-section in
#' a channel given a specified width change, distributing the change
#' according to a given scheme.
#'
#' @inheritParams xt_widen
#' @param width Change in width; single positive value or vector matching the
#'   number of cross sections. Plain numeric is interpreted in the channel's
#'   CRS length unit; a [units::units()] length object is converted
#'   automatically (for example `units::set_units(10, "ft")` against a metric
#'   channel).
#' @returns A numeric vector of erosion volumes for each cross-section in the
#'   channel, carrying [units::units()] of (CRS length unit)^3 when the
#'   channel has a CRS with a defined linear unit.
#' @examples
#' xt_erosion_volume(channel, width = 10, side = "left")
#' xt_erosion_volume(channel, width = 10, side = side_left(0.75))
#' @export
xt_erosion_volume <- function(
  channel,
  width,
  side = "both"
) {
  checkmate::assert_class(channel, "xchan")
  unit <- crs_length_unit(channel)
  width <- to_numeric_length(width, unit, arg = "width")
  raw <- erosion_volume_numeric(channel, width, side)
  with_volume_units(raw, unit)
}

#' @noRd
erosion_volume_numeric <- function(channel, width, side = "both") {
  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Channel object must have profile cross sections")
  }

  # Parse side argument to get proportions
  prop_left <- parse_side_arg(side, channel)

  # Recycle width to match number of cross-sections
  width <- vctrs::vec_recycle(width, length(profile))

  # Calculate erosion volume for each cross-section
  volumes <- numeric(length(profile))
  for (i in seq_along(profile)) {
    xs <- profile[[i]]
    dw_left <- width[i] * prop_left[i]
    dw_right <- width[i] - dw_left

    v1 <- xt_erosion_volume_left(xs, dw_left)
    xs_flipped <- flip_profile(xs)
    v2 <- xt_erosion_volume_left(xs_flipped, dw_right)

    volumes[i] <- v1 + v2
  }

  volumes
}

xt_erosion_volume_left <- function(xs, width) {
  checkmate::assert_numeric(width, 0, len = 1, any.missing = FALSE)

  if (width == 0) {
    return(0)
  }

  # xs$banks and xs$thalwegs store indices into xs$coordinates; convert to
  # actual x-coordinates (distances along the profile) before using them
  # for geometric comparisons.
  x_old <- get_left_bank_coords(xs)[1]
  x_new <- x_old - width
  nodes <- xs$coordinates
  x_extent <- min(nodes[, 1])
  if (x_new < x_extent) {
    stop(
      "Cannot calculate erosion volume for given change in width, as ",
      "the cross section extent is surpassed."
    )
  }

  y_thalweg <- get_min_thalweg_coords(xs)[2]
  nodes <- inject_coords(nodes, x_new)
  x_in_between <- nodes[, 1] >= x_new & nodes[, 1] <= x_old
  between_nodes <- nodes[x_in_between, , drop = FALSE]
  n <- nrow(between_nodes)
  if (n < 2) {
    return(0)
  }
  nm1 <- n - 1
  delta_x <- between_nodes[2:n, 1] - between_nodes[1:nm1, 1]
  delta_y <- abs(between_nodes[2:n, 2] - between_nodes[1:nm1, 2])
  avg_y <- (between_nodes[2:n, 2] + between_nodes[1:nm1, 2]) / 2
  y_upper <- pmax(between_nodes[2:n, 2], between_nodes[1:nm1, 2])
  below_thalweg <- between_nodes[, 2] < y_thalweg
  zero_area <- below_thalweg[1:nm1] + below_thalweg[2:n] == 2
  partially_above <- below_thalweg[1:nm1] + below_thalweg[2:n] == 1
  area <- ifelse(
    partially_above,
    delta_x * (y_upper - y_thalweg)^2 / delta_y / 2,
    delta_x * (avg_y - y_thalweg)
  )
  area[zero_area] <- 0
  sum(area)
}
