test_that("near-duplicate transect breakpoints collapse within tolerance", {
  got <- xchan:::collapse_sorted_tvals(
    c(0, 1e-10, 162.3008, 162.3008 + 1e-9),
    tol = 1e-8
  )

  expect_equal(got, c(5e-11, 162.3008000005), tolerance = 1e-12)
})

test_that("true island crossings add an even number of bank vertices", {
  skip_if_not_installed("sf")

  outer <- matrix(
    c(
      0,
      0,
      10,
      0,
      10,
      6,
      0,
      6,
      0,
      0
    ),
    ncol = 2,
    byrow = TRUE
  )
  hole <- matrix(
    c(
      4,
      1,
      6,
      1,
      6,
      3,
      4,
      3,
      4,
      1
    ),
    ncol = 2,
    byrow = TRUE
  )
  banks <- sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = 3005)
  chord <- sf::st_linestring(rbind(c(0, 2), c(10, 2)))

  got <- xchan:::transect_refine_with_island_boundaries(chord, banks)
  co <- sf::st_coordinates(sf::st_sfc(got, crs = 3005))[, 1:2, drop = FALSE]

  expect_identical(nrow(co), 4L)
  expect_equal(co[, 1L], c(0, 4, 6, 10), tolerance = 1e-8)
  expect_equal(co[, 2L], rep(2, 4), tolerance = 1e-8)
})

test_that("tangent island nicks do not create extra bank vertices", {
  skip_if_not_installed("sf")

  outer <- matrix(
    c(
      0,
      0,
      10,
      0,
      10,
      6,
      0,
      6,
      0,
      0
    ),
    ncol = 2,
    byrow = TRUE
  )
  hole <- matrix(
    c(
      5,
      2,
      6,
      3,
      5,
      4,
      4,
      3,
      5,
      2
    ),
    ncol = 2,
    byrow = TRUE
  )
  banks <- sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = 3005)
  chord <- sf::st_linestring(rbind(c(0, 2), c(10, 2)))

  got <- xchan:::transect_refine_with_island_boundaries(chord, banks)
  co <- sf::st_coordinates(sf::st_sfc(got, crs = 3005))[, 1:2, drop = FALSE]

  expect_identical(nrow(co), 2L)
  expect_equal(co[, 1L], c(0, 10), tolerance = 1e-8)
  expect_equal(co[, 2L], c(2, 2), tolerance = 1e-8)
})
