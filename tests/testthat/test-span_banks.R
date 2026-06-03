test_that("Spanning banks works on a square", {
  library(sf)
  corners <- matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )
  square <- st_polygon(list(corners))
  square_sfc <- st_sfc(square)
  # ---- Vertical line ------
  middle <- st_point(c(0.5, 0.5))
  # plot(square); plot(middle, add = TRUE)
  span_vertical1 <- xchan:::span_banks(middle, pi / 2, square)
  # plot(span_vertical1, add = TRUE)
  span_vertical2 <- xchan:::span_banks(middle, pi / 2, square_sfc)
  # plot(span_vertical2, add = TRUE)
  vertical <- st_linestring(matrix(
    c(0.5, 0, 0.5, 1),
    ncol = 2,
    byrow = TRUE
  ))
  expect_true(
    st_equals(
      st_snap(span_vertical1, vertical, tolerance = 1e-10),
      st_sfc(vertical),
      sparse = FALSE
    )[1, 1]
  )
  expect_true(
    st_equals(
      st_snap(span_vertical2, vertical, tolerance = 1e-10),
      st_sfc(vertical),
      sparse = FALSE
    )[1, 1]
  )
  # ---- Angled line ------
  # plot(square); plot(middle, add = TRUE)
  span_diag1 <- xchan:::span_banks(middle, pi / 4, square)
  # plot(span_diag1, add = TRUE)
  span_diag2 <- xchan:::span_banks(middle, pi / 4, square_sfc)
  # plot(span_diag2, add = TRUE)
  diag <- st_linestring(matrix(
    c(0, 0, 1, 1),
    ncol = 2,
    byrow = TRUE
  ))
  expect_true(
    st_equals(
      st_snap(span_diag1, diag, tolerance = 1e-10),
      st_sfc(diag),
      sparse = FALSE
    )[1, 1]
  )
  expect_true(
    st_equals(
      st_snap(span_diag2, diag, tolerance = 1e-10),
      st_sfc(diag),
      sparse = FALSE
    )[1, 1]
  )
})
