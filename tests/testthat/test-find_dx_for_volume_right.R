topo <- matrix(
  c(
    0, 12,
    1, 12,
    2, 15,
    3, 7,
    4, 11,
    5, 12
  ),
  ncol = 2,
  byrow = TRUE
)

find_dx_for_volume_right <- xchan:::find_dx_for_volume_right

test_that("dx calculations work in typical situations.", {
  t <- 7
  a1 <- 6.5
  a2 <- a1 + 4
  a_max <- 17
  # Matching exact x values
  dx1 <- find_dx_for_volume_right(a1, x0 = 1, topo = topo, thalweg_height = t)
  dx2 <- find_dx_for_volume_right(a2, x0 = 1, topo = topo, thalweg_height = t)
  expect_equal(dx1, 1)
  expect_equal(dx2, 2)
  # In-between area means in-between x values.
  a1.5 <- mean(c(a1, a2))
  dx1.5 <- find_dx_for_volume_right(a1.5, x0 = 1, topo = topo, thalweg_height = t)
  expect_lt(dx1.5, dx2)
  expect_gt(dx1.5, dx1)
  # Zero volume and max volume work.
  dx0 <- find_dx_for_volume_right(0, x0 = 1, topo = topo, thalweg_height = t)
  expect_equal(dx0, 0)
  dx_max <- find_dx_for_volume_right(
    a_max, x0 = 1, topo = topo, thalweg_height = t
  )
  expect_equal(dx_max, 4)
  # Can start at an x value not in the matrix.
  dx01 <- find_dx_for_volume_right(
    1.25, x0 = 0.5, topo = topo, thalweg_height = t
  )
  expect_equal(dx01, 0.25)
  dx02 <- find_dx_for_volume_right(
    2.5 + a1, x0 = 0.5, topo = topo, thalweg_height = t
  )
  expect_equal(dx02, 1.5)
})

test_that("Exceeding max volume available returns an appropriate value.", {
  expect_error(find_dx_for_volume_right(
    1000, x0 = 1, topo = topo, thalweg_height = 7
  ))
  expect_error(find_dx_for_volume_right(
    Inf, x0 = 1, topo = topo, thalweg_height = 7
  ))
})

test_that("Improper inputs or edge cases.", {
  expect_error(find_dx_for_volume_right(
    -1, x0 = 1, topo = topo, thalweg_height = 7
  ))
  expect_error(find_dx_for_volume_right(
    1, x0 = 1, topo = topo[numeric(), 1:2, drop = FALSE], thalweg_height = 7
  ))
})

test_that("Single-row topography (reached the end of the topo).", {
  topo1 <- topo[6, , drop = FALSE]  # x=5, y=12
  # v=0 always returns 0, by convention.
  # valley-left, v=0, above-thalweg
  l0a <- find_dx_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 0, valley = "left"
  )
  expect_equal(l0a, 0)
  # valley-right, v=0, above-thalweg
  r0a <- find_dx_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 0, valley = "right"
  )
  expect_equal(r0a, 0)
  # valley-left, v=0, below-thalweg
  l0b <- find_dx_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 13, valley = "left"
  )
  expect_equal(l0b, 0)
  # valley-right, v=0, below-thalweg
  r0b <- find_dx_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 13, valley = "right"
  )
  expect_equal(r0b, 0)
  # valley-left, v=0, at-thalweg
  l0c <- find_dx_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 12, valley = "left"
  )
  expect_equal(l0c, 0)
  # valley-right, v=0, at-thalweg
  r0c <- find_dx_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 12, valley = "right"
  )
  expect_equal(r0c, 0)

  # v>0 should always error once the available extent is exhausted.
  # valley-left, v>0, above-thalweg
  expect_error(find_dx_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 0, valley = "left"
  ))
  # valley-right, v>0, above-thalweg
  expect_error(find_dx_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 0, valley = "right"
  ))
  # valley-left, v>0, below-thalweg
  expect_error(find_dx_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 13, valley = "left"
  ))
  # valley-right, v>0, below-thalweg
  expect_error(find_dx_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 13, valley = "right"
  ))
  # valley-left, v>0, at-thalweg
  expect_error(find_dx_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 12, valley = "left"
  ))
  # valley-right, v>0, at-thalweg
  expect_error(find_dx_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 12, valley = "right"
  ))
})

test_that("dx calculations work with a valley", {
  t <- 11
  eps <- 1e-5
  a1 <- 2.5
  a2 <- a1 + 1
  a3 <- a2 + 0.5
  left_valley <- 1.5
  right_valley <- 3
  # Erode to just before, then just after, the valley.
  l <- find_dx_for_volume_right(
    a2 - eps, x0 = 1, topo = topo, thalweg_height = t
  )
  r <- find_dx_for_volume_right(
    a2 + eps, x0 = 1, topo = topo, thalweg_height = t
  )
  expect_lt(l, left_valley)
  expect_gt(l, left_valley - 0.1)
  expect_gt(r, right_valley)
  expect_lt(r, right_valley + 0.1)
  # Erode to the valley; check left and right works.
  l <- find_dx_for_volume_right(
    a2, x0 = 1, topo = topo, thalweg_height = t, valley = "left"
  )
  r <- find_dx_for_volume_right(
    a2, x0 = 1, topo = topo, thalweg_height = t, valley = "right"
  )
  expect_equal(l, left_valley)
  expect_equal(r, right_valley)
  # Beyond the valley
  a2.5 <- mean(c(a2, a3))
  dx2.5 <- find_dx_for_volume_right(a2.5, x0 = 1, topo = topo, thalweg_height = t)
  expect_gt(dx2.5, 3)
  expect_lt(dx2.5, 4)
})

rm("topo")
