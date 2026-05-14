# Plan transects from profile relative distances (chord from first to last vertex,
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
