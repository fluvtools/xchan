#' Get Eroded Volume (Area)
#'
#' This function takes the original x-sec object and the eroded one resulting
#' from whether (erode_left_xs) or (erode_right_xs)
#'
#' @param xs 2D cross section.
#' @param dw Change in width; single positive numeric.
#' @param prop_left Proportion of *width* change to apply to the left side
#' channel; the right side channel gets `1 - prop_left` of the change in width.
#' @rdname xt_erosion_volume
#' @export
xt_erosion_volume <- function(xs, dw, prop_left = 0.5) {
  dw_left <- dw * prop_left
  dw_right <- dw - dw_left
  v1 <- xt_erosion_volume_left(xs, dw_left)
  v2 <- xt_erosion_volume_right(xs, dw_right)
  v1 + v2
}

#' @rdname xt_erosion_volume
#' @export
xt_erosion_volume_right <- function(xs, dw) {
  xs <- flip_xs2d(xs)
  xt_erosion_volume_left(xs, dw)
}

#' @rdname xt_erosion_volume
#' @export
xt_erosion_volume_left <- function(xs, dw) {
  checkmate::assert_numeric(dw, 0, len = 1, any.missing = FALSE)
  if (dw == 0) return(0)
  x_old <- xs$left$bank[1]
  x_new <- x_old - dw
  left_nodes <- xs$left$multiline
  if (x_new < min(left_nodes[, 1])) {
    stop(
      "Cannot calculate erosion volume for given change in width, as ",
      "the cross section extent is surpassed."
    )
  }
  y_thalweg <- xs$left$thalweg[2]
  left_nodes <- inject_bankpoint(left_nodes, x_new)
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
  sum(area)
}
