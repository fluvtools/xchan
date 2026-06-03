#' Convert channel geometry to `sfc`
#'
#' Returns simple feature geometry columns for plan, profile, or 3D views of an
#' [`xchan`]. Combine with [sf::st_sf()] yourself if you need a data frame of
#' attributes alongside geometry.
#'
#' @param channel An [`xchan`] object.
#' @param ... Must be empty.
#' @param what Type of spatial object to return: `"plan"` for map-view cross
#'   sections (default); `"profile"` for each profile as a `LINESTRING` in
#'   distance–elevation space (no CRS); `"3d"` for 3D multilinestrings built by
#'   mapping profile elevations onto plan coordinates (same CRS as the plan).
#' @param extent `"banks"` restricts geometry to the bank-to-bank span in each
#'   representation; `"full"` uses the full sampled profile span (and for `what
#'   = "plan"`, map segments spanning each profile's horizontal range);
#'   `"wetted"` keeps only **water** intervals between consecutive bank contacts
#'   (dry islands excluded), so each cross section may be a `MULTILINESTRING`
#'   when an island splits the channel. For `what = "plan"` with `"full"`, if
#'   there is no profile view a warning is issued and bank-to-bank geometries
#'   are returned instead. For `"wetted"` without profiles, water intervals are
#'   taken from plan vertices (even count, alternating water / land / water).
#' @returns An `"sfc"` object.
#' @examples
#' ch <- xt_as_channel(c(2, 2), crs = 3005)
#' plan_view <- xt_as_sfc(ch, what = "plan")
#' plan_view
#' plot(xt_as_sfc(ch, what = "plan"))
#' @export
xt_as_sfc <- function(
  channel,
  ...,
  what = c("plan", "profile", "3d"),
  extent = c("banks", "full", "wetted")
) {
  rlang::check_dots_empty()
  checkmate::assert_class(channel, "xchan")
  what <- match.arg(what)
  extent <- match.arg(extent)

  plan <- channel_plan(channel)
  checkmate::assert_class(plan, "sfc")

  if (what == "plan") {
    if (extent == "banks") {
      return(plan)
    }
    if (extent == "full") {
      if (!xt_has_profile(channel)) {
        warning(
          'extent = "full" requires profile cross sections; returning bank-to-bank geometry instead.',
          call. = FALSE
        )
        return(plan)
      }
      profiles <- channel_profile(channel)
      geoms <- vector("list", length(plan))
      for (i in seq_along(plan)) {
        rng <- range(profiles[[i]]$coordinates[, 1])
        geoms[[i]] <- transect_segment_from_relative(plan[[i]], rng[1], rng[2])
      }
      return(sf::st_sfc(geoms, crs = sf::st_crs(plan)))
    }
    profiles <- channel_profile(channel)
    geoms <- vector("list", length(plan))
    for (i in seq_along(plan)) {
      if (!is.null(profiles)) {
        intervals <- water_interval_ranges(get_bank_distances(profiles[[i]]))
        segs <- lapply(seq_len(nrow(intervals)), function(k) {
          transect_segment_from_relative(
            plan[[i]],
            intervals[k, 1],
            intervals[k, 2]
          )
        })
      } else {
        segs <- plan_water_linestrings(plan[[i]])
      }
      geoms[[i]] <- as_line_or_multilinestring(segs)
    }
    return(sf::st_sfc(geoms, crs = sf::st_crs(plan)))
  }

  profile <- channel_profile(channel)
  if (is.null(profile)) {
    stop(
      "Profile geometry is not available for this channel object.",
      call. = FALSE
    )
  }

  if (what == "profile") {
    profile_geoms <- lapply(
      profile,
      function(pr) profile_linestring_for_extent(pr, extent)
    )
    return(sf::st_sfc(profile_geoms))
  }

  ext <- extent
  coords_3d <- vector("list", length(plan))
  for (i in seq_along(plan)) {
    coords_3d[[i]] <- do.call(
      create_3d_coords,
      list(
        plan = plan[[i]],
        profile = profile[[i]],
        extent = ext
      )
    )
  }
  sf::st_sfc(coords_3d, crs = sf::st_crs(plan))
}
