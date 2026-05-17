test_that("transect_axis_station uses extended chord ∩ axis, not bank-mid projection", {
  skip_if_not_installed("sf")
  crs <- 3005
  ax <- sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(100, 100))), crs = crs)
  seg_sfc <- sf::st_sfc(sf::st_linestring(rbind(c(30, 40), c(70, 40))), crs = crs)
  stn <- xchan:::transect_axis_station_sfc(seg_sfc, ax)
  s_hit <- as.numeric(sf::st_line_project(ax, stn))
  mid <- xchan:::transect_bank_midpoint_sfc(seg_sfc)
  s_mid <- as.numeric(sf::st_line_project(ax, mid))
  expect_equal(s_hit, 40 * sqrt(2), tolerance = 1e-6)
  expect_equal(s_mid, 45 * sqrt(2), tolerance = 1e-6)
  expect_true(abs(s_hit - s_mid) > 1e-3)
})

test_that("xt_distance_downstream uses chord–axis intersection chainage", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 7)
  ds <- xt_distance_downstream(ch)
  expect_length(ds, length(ch))
  ax <- xt_axis(ch)
  plan <- channel_plan(ch)
  exp <- xchan:::plan_chainage_on_axis(plan, ax)
  expect_equal(as.numeric(ds), exp)
  sh <- ch[sample.int(length(ch)), ]
  expect_equal(
    as.numeric(xt_distance_downstream(sh, axis = ax)),
    xchan:::plan_chainage_on_axis(channel_plan(sh), ax)
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
