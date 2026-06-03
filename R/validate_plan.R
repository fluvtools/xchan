#' Validate plan view cross sections
#'
#' Check if plan view cross sections (line segments) are proper for cross
#' section analysis.
#'
#' @param plan Plan view cross sections as sfc_LINESTRING object
#' @returns A list with validation results:
#'   - `valid`: Logical indicating if all checks passed - `issues`: Character
#'   vector describing any issues found - `details`: List with detailed results
#'   for each check
#' @details This function performs basic validation on plan view cross sections:
#'
#' - **Basic geometry**: Ensures all geometries are valid LINESTRING objects
#'
#' @keywords internal
validate_plan <- function(plan) {
  if (!inherits(plan, "sfc_LINESTRING")) {
    return(list(
      valid = FALSE,
      issues = "Plan must be an sfc_LINESTRING object",
      details = list()
    ))
  }

  issues <- character()
  details <- list()

  # Check basic geometry validity
  valid_geoms <- sf::st_is_valid(plan)
  if (!all(valid_geoms)) {
    invalid_indices <- which(!valid_geoms)
    issues <- c(
      issues,
      paste(
        "Invalid geometries at indices:",
        paste(invalid_indices, collapse = ", ")
      )
    )
    details$valid_geometries <- list(
      valid = FALSE,
      invalid_indices = invalid_indices
    )
  } else {
    details$valid_geometries <- list(valid = TRUE)
  }

  list(
    valid = length(issues) == 0,
    issues = issues,
    details = details
  )
}
