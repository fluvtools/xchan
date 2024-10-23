

#' Get thalwegs
#' @export
get_thalwegs <- function(mat) {
  mat[mat[, 2] == min(mat[, 2]), , drop = FALSE]
}

#' get point on cross section
#' @export
get_points <- function(mat, x) {
  y <- approx(mat[, 1], mat[, 2], x)$y
  cbind(x, y)
}

#' Inject point
#'
#' Inject bankpoint, potentially splitting a linesegment into two
#' if x doesn't already land on a node.
#' @export
inject_bankpoint <- function(mat, x) {
  x <- x[!(x %in% mat[, 1])]
  if (length(x) == 0) return(mat)
  new_points <- get_points(mat, x)
  new_mat <- rbind(mat, new_points)
  new_mat[order(new_mat[, 1]), ]
}

#' Make cross section
#' @examples
#' my_xs <- xt_cross_section(coords, 1, 12)
#' plot_(my_xs)
#'
#' xs_width(my_xs)
#' rb_height(my_xs)
#'
#' bigger_xs <- widen_right(my_xs, 2)
#' plot_(bigger_xs, add = TRUE)
#'
#' xs_width(bigger_xs)
#' rb_height(bigger_xs)
#'
#' plot_(widen_right(bigger_xs, 3), add = TRUE)
#' plot_(widen_left(bigger_xs, 3), add = TRUE)
#' @export
xt_cross_section <- function(mat, x_lb, x_rb) {
  mat <- inject_bankpoint(mat, c(x_lb, x_rb))
  thalwegs <- get_thalwegs(mat)
  x_thalwegs <- thalwegs[, 1]
  coords_left <- mat[mat[, 1] <= min(x_thalwegs), ]
  coords_right <- mat[mat[, 1] >= max(x_thalwegs), ]
  lb_thalwegs <- get_thalwegs(coords_left)
  rb_thalwegs <- get_thalwegs(coords_right)
  list(
    left = list(
      multiline = coords_left,
      bank = get_points(coords_left, x_lb),
      thalweg = get_thalwegs(coords_left)
    ),
    right = list(
      multiline = coords_right,
      bank = get_points(coords_right, x_rb),
      thalweg = get_thalwegs(coords_right)
    )
  )
}

#' @export
plot_ <- function(xs, add = FALSE) {
  if (!add) {
    plot(sf::st_sfc(
      sf::st_multilinestring(list(xs$left$multiline)),
      sf::st_multilinestring(list(xs$right$multiline))
    ))
  }
  plot(sf::st_sfc(sf::st_multilinestring(list(xs$left$multiline))), add = TRUE, col = "blue")
  plot(sf::st_sfc(sf::st_multilinestring(list(xs$right$multiline))), add = TRUE, col = "red")
  plot(sf::st_linestring(rbind(xs$left$thalweg, xs$right$thalweg)), add = TRUE)
  plot(sf::st_point(xs$left$bank), add = TRUE, col = "blue")
  plot(sf::st_point(xs$right$bank), add = TRUE, col = "red")
}

#' @export
widen_left <- function(xs, dw) {
  if (dw == 0) return(xs)
  original_x <- xs$left$bank[1]
  new_x <- original_x - dw
  # Inject the new x value into the banks.
  bank <- inject_bankpoint(xs$left$multiline, new_x)
  # Get the bankpoint
  bankpoint <- get_point(bank, new_x)
  new_y <- bankpoint[2]
  # Move the lower points of the bank over, but not past the new bankpoint.
  bank[bank[, 2] < new_y, 1] <- pmax(bank[bank[, 2] < new_y, 1] - dw, new_x)
  xs$left$multiline <- bank
  xs$left$bank <- bankpoint
  xs$left$thalweg[1] <- xs$left$thalweg[1] - dw
  xs
}

#' @export
widen_right <- function(xs, dw) {
  if (dw == 0) return(xs)
  original_x <- xs$right$bank[1]
  new_x <- original_x + dw
  # Inject the new x value into the banks.
  bank <- inject_bankpoint(xs$right$multiline, new_x)
  # Get the bankpoint
  bankpoint <- get_point(bank, new_x)
  new_y <- bankpoint[2]
  # Move the lower points of the bank over, but not past the new bankpoint.
  bank[bank[, 2] < new_y, 1] <- pmin(bank[bank[, 2] < new_y, 1] + dw, new_x)
  xs$right$multiline <- bank
  xs$right$bank <- bankpoint
  xs$right$thalweg[1] <- xs$right$thalweg[1] + dw
  xs
}

#' @export
xs_width <- function(xs) {
  xs$right$bank[1] - xs$left$bank[1]
}

#' @export
lb_height <- function(xs) {
  xs$left$bank[2] - xs$left$thalweg[2]
}

#' @export
rb_height <- function(xs) {
  xs$right$bank[2] - xs$right$thalweg[2]
}


