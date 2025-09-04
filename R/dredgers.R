#' Create a rectangular dredger
#'
#' @param depth Depth to dredge below the water surface
#' @param wse Water surface elevation specification
#' @returns A dredger object that takes a channel and returns a modified channel
#' @export
dredger_rectangle <- function(depth, wse = elevation_bank()) {
  f <- function(channel) {
    if (!is_channel(channel)) {
      stop("Input must be a channel object.")
    }
    if (xt_has_profile(channel)) {
      profile <- xt_column_profile(channel)
      x_left <- profile$banks[1]
      x_right <- profile$banks[length(profile$banks)]
      elev <- wse(channel)
      z_bottom <- elev - depth
      new_profile <- Map(
        f = function(prof, z) {
          xt_nodes_channel(prof) <- matrix(
            c(x_left, z, x_right, z),
            ncol = 2, byrow = TRUE
          )
        },
        profile,
        z_bottom
      )
    } else {
      widths <- xt_width(channel)
      new_profile <- lapply(widths, function(w) {
        xt_profile(
          coords = matrix(
            c(
              -w / 2, depth,
              -w / 2, 0,
              w / 2, 0,
              w / 2, depth
            ),
            ncol = 2, byrow = TRUE
          ),
          bankpoints = c(-w / 2, w / 2)
        )
      })
    }
    xt_column_profile(channel) <- new_profile
    channel
  }
  structure(
    f,
    name = "Rectangle Dredger",
    params = list(depth = depth, wse = wse),
    class = "sxchan_dredger"
  )
}

