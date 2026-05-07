test_that("xt_distance_ds uses axis projection and respects explicit axis", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 7)
  ds <- xt_distance_ds(ch)
  expect_length(ds, nrow(ch))
  ax <- xt_axis(ch)
  mid <- xchan:::plan_midpoints_sfc(ch$plan)
  # `ds` carries CRS units; compare bare numeric values.
  expect_equal(as.numeric(ds), as.numeric(sf::st_line_project(ax, mid)))
  sh <- ch[sample.int(nrow(ch)), ]
  expect_equal(
    as.numeric(xt_distance_ds(sh, axis = ax)),
    as.numeric(sf::st_line_project(ax, xchan:::plan_midpoints_sfc(sh$plan)))
  )
})

test_that("explicit axis matches stored axis for xt_distance_ds (also used by xt_gradient)", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 5)
  ax <- xt_axis(ch)
  expect_identical(xt_distance_ds(ch), xt_distance_ds(ch, axis = ax))
})
