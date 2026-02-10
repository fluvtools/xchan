#' Elevation Specifications
#'
#' Elevation specifications define how to calculate elevation values from
#' channel cross-sections. These functions return elevation specification
#' objects that can be used in various channel operations.
#'
#' @param .f Function to apply to elevation values (e.g., mean, min, max)
#' @param ... Additional arguments passed to `.f`
#' @returns An elevation specification object
#' @rdname elevations
#' @export
elevation_thalweg <- function() {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "thalweg height."
      )
    }
    profile <- xt_column_profile(channel)
    vapply(
      profile,
      function(xs) xs$left$thalweg[2],
      numeric(1)
    )
  }
  structure(fun, name = "thalweg", class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_bank <- function(.f = min, ...) {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "bank height."
      )
    }
    profile <- xt_column_profile(channel)
    vapply(
      profile,
      function(xs) .f(c(xs$left$bank[2], xs$right$bank[2]), ...),
      numeric(1)
    )
  }
  structure(fun, name = "bank", params = list(.f = .f), class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_bank_left <- function() {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "bank height."
      )
    }
    profile <- xt_column_profile(channel)
    vapply(
      profile,
      function(xs) get_left_bank_coords(xs)[2],
      numeric(1L)
    )
  }
  structure(fun, name = "bank_left", class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_bank_right <- function() {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "bank height."
      )
    }
    profile <- xt_column_profile(channel)
    vapply(
      profile,
      function(xs) get_right_bank_coords(xs)[2],
      numeric(1L)
    )
  }
  structure(fun, name = "bank_right", class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_topo_left <- function() {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "topography height."
      )
    }
    profile <- xt_column_profile(channel)
    vapply(
      profile,
      function(xs) {
        get_left_bank_coords(xs)[2]
      },
      numeric(1)
    )
  }
  structure(fun, name = "topo_left", class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_topo_right <- function() {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "topography height."
      )
    }
    profile <- xt_column_profile(channel)
    vapply(
      profile,
      function(xs) {
        get_right_bank_coords(xs)[2]
      },
      numeric(1)
    )
  }
  structure(fun, name = "topo_right", class = "sxchan_elevation")
}

#' @rdname elevations
#' @export
elevation_topo <- function(.f = mean, ...) {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "topography height."
      )
    }
    profile <- xt_column_profile(channel)
    vapply(
      profile,
      function(xs) .f(xs$coordinates[, 2], ...),
      numeric(1)
    )
  }
  structure(fun, name = "topo", params = list(.f = .f, ...), class = "sxchan_elevation")
}

#' Create a bottom elevation specification
#'
#' @param .f Function to apply to channel bottom elevations (e.g., min, max, mean, quantile)
#' @param ... Additional arguments for .f (e.g., probs for quantile)
#' @returns An elevation specification that returns aggregated bottom elevations
#' @export
elevation_bottom <- function(.f = mean, ...) {
  fun <- function(channel) {
    if (!xt_has_profile(channel)) {
      stop(
        "Channel object must have profile cross sections to obtain ",
        "topography height."
      )
    }
    profile <- xt_column_profile(channel)

    vapply(
      profile,
      function(xs) {
        left_bank_coords <- get_left_bank_coords(xs)
        right_bank_coords <- get_right_bank_coords(xs)
        left_bank_dist <- left_bank_coords[1]
        right_bank_dist <- right_bank_coords[1]
        d <- xs$coordinates[, 1]
        in_channel <- d >= left_bank_dist & d <= right_bank_dist
        elev <- xs$coordinates[in_channel, 2]
        .f(elev, ...)
      },
      numeric(1)
    )
  }
  structure(
    fun,
    name = "Bottom Elevation",
    params = c(list(.f = .f), list(...)),
    class = "sxchan_elevation"
  )
}


