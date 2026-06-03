#' Squamish demo profiles from the package DEM with rectangular bathymetry
#'
#' The packaged Squamish DEM is LiDAR-derived and has no submerged bathymetry.
#' This helper samples it with [xt_generate_profile()] and inserts a 3 m deep
#' rectangular channel with [xt_dredge_to()].
#'
#' @param channel An [`xchan`] with planimetric cross sections.
#' @param ... Arguments passed to [xt_generate_profile()] after `channel` and
#'   `dem` (for example `sample_freq`).
#' @returns An [`xchan`] with dredged profile cross sections.
#' @noRd
squamish_with_profiles <- function(channel, ...) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package \"terra\" is required.", call. = FALSE)
  }
  channel <- xt_generate_profile(
    channel,
    terra::unwrap(squamish_dem),
    ...
  )
  xt_dredge_to(channel, bathy = bathy_rectangle(depth = 3))
}
