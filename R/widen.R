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
widen <- function(xs, dw, side = c("both", "left", "right")) {
  side <- match.arg(side)
  if (side == "both") {
    dw_left <- dw / 2
    dw_right <- dw / 2
  } else if (side == "left") {
    dw_left <- dw
    dw_right <- 0
  } else {
    dw_left <- 0
    dw_right <- dw
  }
  xs |>
    widen_right(xs, dw_right) |>
    widen_left(xs, dw_left)
}
