coords_between <- function(profile, d_left, d_right, inclusive = TRUE) {
  checkmate::assert_class(profile, "xs_profile")
  checkmate::assert_numeric(d_left, len = 1)
  checkmate::assert_numeric(d_right, len = 1)
  checkmate::assert_logical(inclusive, len = 1)
  nodes <- coords_all(profile)
  d <- nodes[, 1]
  if (inclusive) {
    nodes[d >= d_left & d <= d_right, , drop = FALSE]
  } else {
    nodes[d > d_left & d < d_right, , drop = FALSE]
  }
}


coords_channel <- function(profile, include_banks = TRUE) {
  checkmate::check_class(profile, "xs_profile")
  banks <- range(profile$banks)
  nodes <- coords_between(profile, banks[1], banks[2], inclusive = TRUE)
  if (!include_banks) {
    id <- 1 + seq_len(nrow(nodes) - 2)
    nodes <- nodes[id, , drop = FALSE]
  }
  nodes
}

coords_channel_active <- function(profile, include_banks = TRUE) {
  checkmate::check_class(profile, "xs_profile")
  banks <- range(profile$banks)
  nbanks <- length(banks)

  # Loop over each water section.
  nodes <- list()
  for (i in seq_len(nbanks / 2)) {
    nodes[[i]] <- coords_between(
      profile, banks[2 * i - 1], banks[2 * i], inclusive = TRUE
    )
    if (!include_banks) {
      id <- 1 + seq_len(nrow(nodes[i]) - 2)
      nodes[i] <- nodes[i][id, , drop = FALSE]
    }
  }
  do.call("rbind", nodes)
}

coords_all <- function(profile) {
  checkmate::check_class(profile, "xs_profile")
  profile$coordinates
}


#' Returns a matrix with as many rows as banks, with elevations
#' given by the maximum elevation at the bank location.
coords_banks <- function(profile) {
  checkmate::check_class(profile, "xs_profile")
  nodes <- coords_all(profile)
  banks <- profile$banks
  distances <- nodes[, 1]
  heights <- nodes[, 2]
  lgl_list <- lapply(banks, function(b) b == distances)
  heights <- vapply(
    lgl_list,
    function(lgl) max(banks[lgl, 2]),
    FUN.VALUE = numeric(1L)
  )
  matrix(append(banks, heights), ncol = 2)
}
