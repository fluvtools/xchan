#' Create channel object
#'
#' Creates a channel object from various inputs. A channel object is a
#' data frame with planimetric cross sections and/or profile cross sections.
#'
#' @param x Object to create channel from
#' @param ... Additional columns for the channel data frame
#' @returns A channel object with class "sxchan"
#' @examples
#' # Create channel from widths
#' channel <- xt_channel(c(10, 15, 12, 8))
#'
#' # Create channel from widths with additional metadata
#' channel <- xt_channel(c(10, 15, 12, 8),
#'                      section_id = c("A", "B", "C", "D"),
#'                      roughness = 0.1)
#'
#' # Create channel from sf geometries
#' library(sf)
#' seg <- st_linestring(matrix(c(0, 1, 0, 1), ncol = 2))
#' channel <- xt_channel(seg)
#'
#' # Create channel from sf geometries with profiles and metadata
#' channel <- xt_channel(plan_geometries,
#'                      profile = profile_list)
#'
#' # Create channel from data frame
#' df <- data.frame(
#'   plan = list(seg1, seg2, seg3),
#'   profile = list(prof1, prof2, prof3),
#'   section_id = c("A", "B", "C")
#' )
#' channel <- xt_channel(df, plan_col = "plan", profile_col = "profile")
#' @export
xt_channel <- function(x, ...) {
  UseMethod("xt_channel")
}

#' @export
xt_channel.numeric <- function(x, profile = NULL, ...) {
  checkmate::assert_numeric(x, lower = 0, any.missing = FALSE)

  # Create simple planimetric cross sections from widths
  plan <- lapply(x, function(w) {
    sf::st_linestring(matrix(c(-w / 2, w / 2, 0, 0), ncol = 2))
  })
  plan <- sf::st_sfc(plan)

  # Create data frame/tibble with additional columns from ellipsis
  df <- create_data_frame(...)
  df$plan <- plan
  prof_col <- NULL
  if (!is.null(profile)) {
    prof_col <- "profile"
    df$profile <- profile
  }
  new_channel(df, plan_col = "plan", profile_col = prof_col)
}

#' @export
xt_channel.sfc <- function(x, profile = NULL, ...) {
  # Create data frame/tibble with additional columns from ellipsis
  df <- create_data_frame(...)
  df$plan <- x
  prof_col <- NULL
  if (!is.null(profile)) {
    prof_col <- "profile"
    df$profile <- profile
  }
  new_channel(df, plan_col = "plan", profile_col = prof_col)
}

#' @export
xt_channel.data.frame <- function(x, plan_col = NULL, profile_col = NULL, ...) {
  if (is.null(plan_col) && is.null(profile_col)) {
    stop("At least one of plan_col or profile_col must be specified")
  }
  
  # Validate that specified columns exist
  if (!is.null(plan_col) && !plan_col %in% names(x)) {
    stop("Plan column '", plan_col, "' not found in data frame")
  }
  if (!is.null(profile_col) && !profile_col %in% names(x)) {
    stop("Profile column '", profile_col, "' not found in data frame")
  }

  # Create new channel object
  new_channel(x, plan_col = plan_col, profile_col = profile_col)
}
