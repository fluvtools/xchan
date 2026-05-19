test_that("xt_xsection_at uses downstream flow order not list index", {
  xs_up <- xsection(matrix(c(0, -1, 0, 1), ncol = 2, byrow = TRUE))
  xs_mid <- xsection(matrix(c(5, -1, 5, 1), ncol = 2, byrow = TRUE))
  xs_down <- xsection(matrix(c(10, -1, 10, 1), ncol = 2, byrow = TRUE))
  ch <- xt_as_channel(list(xs_down, xs_up, xs_mid), crs = 3005)
  ax <- sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(10, 0))), crs = 3005)

  expect_identical(xt_xsection_at(ch, 1, axis = ax), xs_up)
  expect_identical(xt_xsection_at(ch, 3, axis = ax), xs_down)
})
