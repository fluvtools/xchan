#' Get Islands from Cross Sections
#'
#' @param polygon Planimetric bankline polygon
#' @param line Planimetric (1D) cross sections; object of class `sxc`
#' @returns A list with two entries:
#' - `indices`: ID's of the cross sections that intersect an island.
#' - `coords_list`: A list for each cross section;
#'   provides (x, y) points of all banks, including island banks.
#' @details Uses CRS of 3157. Should change when formalizing the package.
#' @author Heba A
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
  num_intersections <- vapply(
    intersection_points,
    function(pts) {
      if (inherits(pts, "sfg") || inherits(pts, "sfc")) {  # Check if it's a valid geometry
        coords <- sf::st_coordinates(pts)
        return(nrow(coords))  # Count intersection points
      } else {
        return(0L)  # No intersections
      }
    },
    FUN.VALUE = integer(1L)
  )

  # Get indices where the number of intersections is greater than 2
  indices <- which(num_intersections > 2)

  # Extract intersection geometries for these indices
  selected_intersections <- intersection_points[indices]

  # Extract coordinates from the selected intersections
  coords_list <- lapply(selected_intersections, function(geom) {
    if (inherits(geom, "sfg") || inherits(geom, "sfc")) {  # Ensure it's a valid geometry
      return(sf::st_coordinates(geom))
    } else {
      return(NULL)  # Skip if no valid geometry
    }
  })

  list(indices = indices, coords_list = coords_list)
}
