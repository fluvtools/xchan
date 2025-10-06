xt_widen_width_plan <- function(plan, by, prop_left) {
  xt_validate_plan(plan)
  checkmate::assert_numeric(by)
  checkmate::assert_numeric(prop_left, 0, 1, len = 1)
  by_left <- by * prop_left
  by_right <- by - by_left
  object <- xt_widen_width_plan_right(plan, by = by_right)
  object <- xt_widen_width_plan_right(flip_plan(object), by = by_left)
  flip_plan(object)
}

xt_widen_width_plan_right <- function(plan, by) {
  xt_validate_plan(plan)
  checkmate::assert_numeric(by)
  by <- vctrs::vec_recycle(by, length(plan))
  if (all(by == 0)) {
    return(plan)
  }

  # Extract coordinates and modify them
  coords_list <- lapply(seq_along(plan), function(i) {
    mat <- plan[[i]]
    base_pt <- mat[1, 1:2]        # left
    end_pt <- mat[2, 1:2]         # right
    vec <- end_pt - base_pt
    mag <- sqrt(sum(vec^2))
    unit_vec <- vec / mag
    translation_vec <- unit_vec * by[i]
    mat[2, 1:2] <- mat[2, 1:2] + translation_vec
    mat
  })

  # Reconstruct the sfc object to update bbox
  sf::st_sfc(coords_list, crs = sf::st_crs(plan))
}
