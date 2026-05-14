test_that("xt_distance_downstream uses axis projection and respects explicit axis", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 7)
  ds <- xt_distance_downstream(ch)
  expect_length(ds, length(ch))
  ax <- xt_axis(ch)
  mid <- xchan:::plan_midpoints_sfc(channel_plan(ch))
  # `ds` carries CRS units; compare bare numeric values.
  expect_equal(as.numeric(ds), as.numeric(sf::st_line_project(ax, mid)))
  sh <- ch[sample.int(length(ch)), ]
  expect_equal(
    as.numeric(xt_distance_downstream(sh, axis = ax)),
    as.numeric(sf::st_line_project(ax, xchan:::plan_midpoints_sfc(channel_plan(sh))))
  )
})

test_that("explicit axis matches stored axis for xt_distance_downstream (also used by xt_gradient)", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 5)
  ax <- xt_axis(ch)
  expect_identical(xt_distance_downstream(ch), xt_distance_downstream(ch, axis = ax))
})

test_that("xt_distance_upstream complements xt_distance_downstream along axis length", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 8)
  ax <- xt_axis(ch)
  L <- as.numeric(sf::st_length(ax))
  d_down <- as.numeric(xt_distance_downstream(ch))
  d_up <- as.numeric(xt_distance_upstream(ch))
  expect_equal(d_down + d_up, rep(L, length(ch)))
})
