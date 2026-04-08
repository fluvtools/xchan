widen_plan <- function(plan, dw, prop_left) {
  validation <- xt_validate_plan(plan)
  if (!validation$valid) {
    stop("Invalid plan view cross sections: ", paste(validation$issues, collapse = "; "))
  }
  checkmate::assert_numeric(dw)
  checkmate::assert_numeric(prop_left, lower = 0, upper = 1, any.missing = FALSE)
  dw <- vctrs::vec_recycle(dw, length(plan))
  prop_left <- vctrs::vec_recycle(prop_left, length(plan))
  dw_left <- dw * prop_left
  dw_right <- dw - dw_left
  object <- widen_plan_right(plan, dw = dw_right)
  object <- widen_plan_right(flip_plan(object), dw = dw_left)
  flip_plan(object)
}

widen_plan_right <- function(plan, dw) {
  validation <- xt_validate_plan(plan)
  if (!validation$valid) {
    stop("Invalid plan view cross sections: ", paste(validation$issues, collapse = "; "))
  }
  checkmate::assert_numeric(dw)
  dw <- vctrs::vec_recycle(dw, length(plan))
  if (all(dw == 0)) {
    return(plan)
  }

  # Extract coordinates and modify them
  coords_list <- lapply(seq_along(plan), function(i) {
    mat <- sf::st_coordinates(plan[i])[, 1:2, drop = FALSE]
    if (nrow(mat) < 2) {
      stop("Each plan cross section must have at least two vertices.")
    }
    base_pt <- mat[1, 1:2]        # left
    end_pt <- mat[nrow(mat), 1:2] # right
    vec <- end_pt - base_pt
    mag <- sqrt(sum(vec^2))
    if (mag == 0) {
      stop("Encountered zero-length plan cross section; cannot widen.")
    }
    unit_vec <- vec / mag
    translation_vec <- unit_vec * dw[i]
    mat[nrow(mat), 1:2] <- mat[nrow(mat), 1:2] + translation_vec
    sf::st_linestring(mat)
  })

  # Reconstruct the sfc object to update bbox
  sf::st_sfc(coords_list, crs = sf::st_crs(plan))
}
