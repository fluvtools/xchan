test_that("xt_trace_centerline joins midpoints in axis projection order", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(Squamish_bankline, n = 8)
  plan <- channel_plan(ch)
  axis_line <- xt_axis(ch)
  mid_pts <- xchan:::plan_midpoints_sfc(plan)
  d <- as.numeric(sf::st_line_project(axis_line, mid_pts))
  ord <- order(d)
  xy <- sf::st_coordinates(mid_pts)[ord, 1:2, drop = FALSE]
  axis <- xt_trace_centerline(ch)
  expect_equal(
    unname(as.matrix(sf::st_coordinates(axis)[, 1:2, drop = FALSE])),
    unname(xy)
  )
})
