#' Get Eroded Volume (Area)
#'
#' This function takes the original x-sec object and the eroded one resulting
#' from whether (erode_left_xs) or (erode_right_xs)
#'
#' @param xs 2D cross section.
#' @param dw Change in width; single positive numeric.
#' @param prop_left Proportion of *width* change to apply to the left side
#' channel; the right side channel gets `1 - prop_left` of the change in width.
#' @param error_on_overflow Logical; should an error be thrown if asked
#' to calculate erosion volume beyond cross section extent? `TRUE` if so
#' (the default). If `FALSE`, returns the maximum volume up to the extent.
#' See return value for distinguishing between the two values.
#' @returns A numeric vector of erosion volumes associated with the specified
#' width change. A `censored` attribute contains a logical vector with length
#' equal to that of `xs` indicating whether the calculated width is right-censored.
#' If `TRUE`, then `dw` extends beyond the cross section extent, and the actual
#' erosion volume is at least as big as the indicated value.
#' @rdname xt_erosion_volume
#' @export
xt_erosion_volume <- function(xs, dw, prop_left = 0.5, error_on_overflow = TRUE) {
  dw_left <- dw * prop_left
  dw_right <- dw - dw_left
  v1 <- xt_erosion_volume_left(xs, dw_left, error_on_overflow = error_on_overflow)
  v2 <- xt_erosion_volume_right(xs, dw_right, error_on_overflow = error_on_overflow)
  v <- v1 + v2
  attr(v, "censored") <- attr(v1, "censored") | attr(v2, "censored")
  v
}

#' @rdname xt_erosion_volume
#' @export
xt_erosion_volume_right <- function(xs, dw, error_on_overflow = TRUE) {
  xs <- flip_xs2d(xs)
  xt_erosion_volume_left(xs, dw, error_on_overflow = error_on_overflow)
}

#' @rdname xt_erosion_volume
#' @export
xt_erosion_volume_left <- function(xs, dw, error_on_overflow = TRUE) {
  checkmate::assert_numeric(dw, 0, len = 1, any.missing = FALSE)
  if (dw == 0) {
    a <- 0
    attr(a, "censored") <- FALSE
    return(a)
  }
  x_old <- xs$left$bank[1]
  x_new <- x_old - dw
  left_nodes <- xs$left$multiline
  x_extent <- min(left_nodes[, 1])
  censored <- FALSE
  if (x_new < x_extent) {
    if (error_on_overflow) {
      stop(
        "Cannot calculate erosion volume for given change in width, as ",
        "the cross section extent is surpassed."
      )
    } else {
      dw <- x_old - x_extent
      x_new <- x_extent
      censored <- TRUE
    }
  }
  y_thalweg <- xs$left$thalweg[2]
  left_nodes <- inject_2d_points(left_nodes, x_new)
  x_in_between <- left_nodes[, 1] >= x_new & left_nodes[, 1] <= x_old
  between_nodes <- left_nodes[x_in_between, , drop = FALSE]
  n <- nrow(between_nodes)
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
  a <- sum(area)
  attr(a, "censored") <- censored
  a
}
