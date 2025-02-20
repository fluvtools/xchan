#' @export
get_islandxs <- function(polygon, line){
  polygon_sf <- sf::st_sf(geometry = polygon)
  # Assign CRS (EPSG:3157) to each element in the list of Cross section
  line <- lapply(line, function(geom) {
    sf::st_sfc(geom, crs = 3157)  # Convert to sfc with CRS
  })
  # Combine all geometries into an sf object
  line_sf <- sf::st_sf(geometry = do.call(c, line))
  # Compute intersection points between each line and the polygon
  intersection_points <- sf::st_intersection(
    sf::st_geometry(line_sf), sf::st_geometry(polygon_sf)
  )
  # Extract intersection coordinates and count the number of points per line
  num_intersections <- sapply(intersection_points, function(pts) {
    if (inherits(pts, "sfg") || inherits(pts, "sfc")) {
      # Check if it's a valid geometry
      coords <- sf::st_coordinates(pts)
      return(nrow(coords))  # Count intersection points
    } else {
      return(0)  # No intersections
    }
  })
  indices <- which(num_intersections > 2)
  selected_intersections <- intersection_points[indices]
  # Extract coordinates from the selected intersections
  coords_list <- lapply(selected_intersections, function(geom) {
    if (inherits(geom, "sfg") || inherits(geom, "sfc")) {
      # Ensure it's a valid geometry
      return(sf::st_coordinates(geom))
    } else {
      return(NULL)  # Skip if no valid geometry
    }
  })
  return(list(indices = indices, coords_list = coords_list))
}
