#' Convert channel to spatial object
#'
#' Converts a channel object to an `sfc` or [`sf::sf`] object.
#' For `xt_as_sf()`, columns other than the active cross-section geometry column
#' are preserved in the attribute table; the selected spatial representation is
#' written to a single geometry column.
#'
#' @param channel Channel object.
#' @param what Type of spatial object to return: `"plan"` for map-view cross
#'   sections (default); `"profile"` for each profile as a `LINESTRING` in
#'   distance–elevation space (no CRS); `"3d"` for 3D multilinestrings built by
#'   mapping profile elevations onto plan coordinates (same CRS as the plan).
#' @param extent `"banks"` restricts geometry to the bank-to-bank span in each
#'   representation; `"full"` uses the full sampled profile span (and for
#'   `what = "plan"`, map segments spanning each profile's horizontal range).
#'   For `what = "plan"` with `"full"`, if there is no profile view a warning
#'   is issued and bank-to-bank geometries are returned instead.
#' @param geom_col Name of the geometry column in the resulting `sf` object.
#' @returns For `xt_as_sfc()`, an `"sfc"` object. For `xt_as_sf()`, an `"sf"`
#'   object whose geometry column holds the requested representation.
#' @examples
#' xt_as_sf(demo_channel, what = "profile")
#' plan_view <- xt_as_sfc(demo_channel, what = "plan")
#' plan_view
#' plot(plan_view)
#' plot(xt_as_sfc(demo_channel, what = "3d"))
#' @rdname xt_as_sf
#' @export
xt_as_sfc <- function(
  channel,
  what = c("plan", "profile", "3d"),
  extent = c("banks", "full")
) {
  checkmate::assert_class(channel, "xchan_tbl")
  what <- match.arg(what)
  extent <- match.arg(extent)

  plan <- channel_plan(channel)
  checkmate::assert_class(plan, "sfc")

  if (what == "plan") {
    if (extent == "banks") {
      return(plan)
    }
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

  profile <- channel_profile(channel)
  if (is.null(profile)) {
    stop("Profile geometry is not available for this channel object.", call. = FALSE)
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

#' @rdname xt_as_sf
#' @export
xt_as_sf <- function(
  channel,
  what = c("plan", "profile", "3d"),
  extent = c("banks", "full"),
  geom_col = "geometry"
) {
  checkmate::assert_class(channel, "xchan_tbl")
  what <- match.arg(what)
  extent <- match.arg(extent)

  geometry <- xt_as_sfc(channel, what = what, extent = extent)

  xsec_nm <- attr(channel, "xsection_col", exact = TRUE)
  drop <- unique(stats::na.omit(c(xsec_nm)))
  keep <- setdiff(names(channel), drop)
  if (geom_col %in% keep) {
    stop(
      "`geom_col` must not duplicate a retained attribute column name.",
      call. = FALSE
    )
  }

  df <- channel[, keep, drop = FALSE]
  df[[geom_col]] <- geometry
  sf::st_sf(df, crs = sf::st_crs(geometry), sf_column_name = geom_col)
}
