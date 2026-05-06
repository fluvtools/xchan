#' Convert channel to spatial object
#'
#' Converts a channel object to an sfc of sf object from the sf package.
#' For `xt_as_sf()`, original columns are preserved
#' except for the plan and profile geometries, which are combined into a single
#' geometry column.
#'
#' @param channel Channel object.
#' @param what Type of spatial object to return. Options are "plan" for
#' a plan view (line segments; the default); "profile" for a profile view
#' (multiline segments); or "3d" for a 3D arrangement.
#' @param geom_col Name of the geometry column in the resulting sf object.
#' @returns For `xt_as_sfc()`, an "sfc" object containing the cross section
#' geometries. For `xt_as_sf()`, an "sf" object with the original columns of
#' the channel preserved
#' except for the columns containing the plan and profile geometries, which
#' are combined into a single "geometry" column.
#' @examples
#' xt_as_sf(demo_channel, what = "profile")
#' plan_view <- xt_as_sfc(demo_channel, what = "plan")
#' plan_view
#' plot(plan_view)
#' plot(xt_as_sfc(demo_channel, what = "3d"))
#' @rdname xt_as_sf
#' @export
xt_as_sfc <- function(channel, what = c("plan", "profile", "3d")) {
  checkmate::assert_class(channel, "xchan")
  what <- rlang::arg_match(what)

  plan <- xt_column_plan(channel)
  if (what == "plan") {
    # Plan view cross sections are already sfc.
    checkmate::assert_class(plan, "sfc")
    return(plan)
  }

  profile <- xt_column_profile(channel)
  if (is.null(profile)) {
    stop("Profile geometry is not available for this channel object.")
  }

  if (what == "profile") {
    profile_geoms <- list()
    for (i in seq_along(profile)) {
      prof <- profile[[i]]

      # Extract coordinates from left and right sides
      left_coords <- prof$left$coordinates
      right_coords <- prof$right$coordinates

      # Create multiline objects for left and right sides
      left_geom <- sf::st_multilinestring(list(left_coords))
      right_geom <- sf::st_multilinestring(list(right_coords))

      # Combine into a single multiline geometry
      combined_geom <- sf::st_multilinestring(list(
        left_coords,
        right_coords
      ))

      profile_geoms[[i]] <- combined_geom
    }
    return(sf::st_sfc(profile_geoms))
  }

  # Convert each cross section to a multiline
  coords_3d <- Map(create_3d_coords, plan, profile)
  sf::st_sfc(coords_3d)
}

#' @rdname xt_as_sf
#' @export
xt_as_sf <- function(channel, what = c("plan", "profile", "3d"), geom_col = "geometry") {
  checkmate::assert_class(channel, "xchan")
  what <- rlang::arg_match(what)
  geometry <- xt_as_sfc(channel, what = what)
  xt_column_plan(channel) <- NULL
  xt_column_profile(channel) <- NULL
  channel[[geom_col]] <- geometry
  sf::st_as_sf(channel, sf_column_name = geom_col)
}


