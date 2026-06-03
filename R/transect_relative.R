# Plan transects from profile relative distances (chord from first to last
#  vertex,
# matching extend_cross_section). Relative distance 0 is frame centre; banks at
# +/- half plan length.

#' @noRd
transect_xy_from_relative <- function(plan_seg, r) {
  co <- sf::st_coordinates(plan_seg)
  start <- co[1L, 1:2, drop = TRUE]
  end <- co[nrow(co), 1:2, drop = TRUE]
  chord <- end - start
  chord_len <- sqrt(sum(chord^2))
  if (chord_len < .Machine$double.eps) {
    return(start)
  }
  u <- chord / chord_len
  w <- as.numeric(sf::st_length(plan_seg))
  start + u * (r + w / 2)
}

#' @noRd
transect_segment_from_relative <- function(plan_seg, r1, r2) {
  a <- transect_xy_from_relative(plan_seg, r1)
  b <- transect_xy_from_relative(plan_seg, r2)
  sf::st_linestring(rbind(a, b))
}

#' Plan-view linestrings for each water interval along a transect with islands
#'
#' Vertices alternate water / land / water; odd-indexed vertex pairs are water.
#'
#' @noRd
plan_water_linestrings <- function(plan_seg) {
  co <- sf::st_coordinates(plan_seg)[, 1:2, drop = FALSE]
  n <- nrow(co)
  if (n < 2L) {
    stop("Planimetric transect must have at least two vertices.", call. = FALSE)
  }
  if (n == 2L) {
    return(list(sf::st_linestring(co)))
  }
  if (n %% 2L != 0L) {
    stop(
      "Planimetric transect must have an even number of vertices so bank ",
      "contacts alternate water / land / water along the line (got ",
      n,
      ").",
      call. = FALSE
    )
  }
  lapply(seq(1L, n - 1L, by = 2L), function(i) {
    sf::st_linestring(co[i:(i + 1L), , drop = FALSE])
  })
}
