xt_widen_width_plan <- function(plan, dw, prop_left) {
  xt_validate_plan(plan)
  checkmate::assert_numeric(dw)
  checkmate::assert_numeric(prop_left, 0, 1, len = 1)
  dw_left <- dw * prop_left
  dw_right <- dw - dw_left
  object <- xt_widen_width_plan_right(plan, dw = dw_right)
  object <- xt_widen_width_plan_right(flip_plan(object), dw = dw_left)
  flip_plan(object)
}

xt_widen_width_plan_right <- function(plan, dw) {
  xt_validate_plan(plan)
  checkmate::assert_numeric(dw)
  dw <- vctrs::vec_recycle(dw, length(plan))
  if (all(dw == 0)) {
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
    translation_vec <- unit_vec * dw[i]
    mat[2, 1:2] <- mat[2, 1:2] + translation_vec
    mat
  })

  # Reconstruct the sfc object to update bbox
  sf::st_sfc(coords_list, crs = sf::st_crs(plan))
}
