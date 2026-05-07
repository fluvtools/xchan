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

test_that("x calculations work in typical situations.", {
  t <- 7
  a1 <- 6.5
  a2 <- a1 + 4
  a_max <- 17
  # Matching exact x values
  x1 <- find_x_for_volume_right(a1, x0 = 1, topo = topo, thalweg_height = t)
  x2 <- find_x_for_volume_right(a2, x0 = 1, topo = topo, thalweg_height = t)
  expect_equal(x1, 2)
  expect_equal(x2, 3)
  # In-between area means in-between x values.
  a1.5 <- mean(c(a1, a2))
  x1.5 <- find_x_for_volume_right(a1.5, x0 = 1, topo = topo, thalweg_height = t)
  expect_lt(x1.5, x2)
  expect_gt(x1.5, x1)
  # Zero volume and max volume work.
  x0 <- find_x_for_volume_right(0, x0 = 1, topo = topo, thalweg_height = t)
  expect_equal(x0, 1)
  x_max <- find_x_for_volume_right(
    a_max, x0 = 1, topo = topo, thalweg_height = t
  )
  expect_equal(x_max, 5)
  # Can start at a x value not in the matrix.
  x01 <- find_x_for_volume_right(
    1.25, x0 = 0.5, topo = topo, thalweg_height = t
  )
  expect_equal(x01, 0.75)
  x02 <- find_x_for_volume_right(
    2.5 + a1, x0 = 0.5, topo = topo, thalweg_height = t
  )
  expect_equal(x02, 2)
})

test_that("Exceeding max volume available returns an appropriate value.", {
  expect_error(find_x_for_volume_right(
    1000, x0 = 1, topo = topo, thalweg_height = 7
  ))
  expect_error(find_x_for_volume_right(
    Inf, x0 = 1, topo = topo, thalweg_height = 7
  ))
})

test_that("Improper inputs or edge cases.", {
  expect_error(find_x_for_volume_right(
    -1, x0 = 1, topo = topo, thalweg_height = 7
  ))
  expect_error(find_x_for_volume_right(
    1, x0 = 1, topo = topo[numeric(), 1:2, drop = FALSE], thalweg_height = 7
  ))

})

test_that("Single-row topography (reached the end of the topo).", {
  topo1 <- topo[6, , drop = FALSE]  # x=5, y=12
  # v=0 always returns x0, by convention.
  # valley-left, v=0, above-thalweg
  l0a <- find_x_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 0, valley = "left"
  )
  expect_equal(l0a, 5)
  # valley-right, v=0, above-thalweg
  r0a <- find_x_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 0, valley = "right"
  )
  expect_equal(r0a, 5)
  # valley-left, v=0, below-thalweg
  l0b <- find_x_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 13, valley = "left"
  )
  expect_equal(l0b, 5)
  # valley-right, v=0, below-thalweg
  r0b <- find_x_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 13, valley = "right"
  )
  expect_equal(r0b, 5)
  # valley-left, v=0, at-thalweg
  l0c <- find_x_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 12, valley = "left"
  )
  expect_equal(l0c, 5)
  # valley-right, v=0, at-thalweg
  r0c <- find_x_for_volume_right(
    0, x0 = 5, topo = topo1, thalweg_height = 12, valley = "right"
  )
  expect_equal(r0c, 5)

  # v>0 should always error once the available extent is exhausted.
  # valley-left, v>0, above-thalweg
  expect_error(find_x_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 0, valley = "left"
  ))
  # valley-right, v>0, above-thalweg
  expect_error(find_x_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 0, valley = "right"
  ))
  # valley-left, v>0, below-thalweg
  expect_error(find_x_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 13, valley = "left"
  ))
  # valley-right, v>0, below-thalweg
  expect_error(find_x_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 13, valley = "right"
  ))
  # valley-left, v>0, at-thalweg
  expect_error(find_x_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 12, valley = "left"
  ))
  # valley-right, v>0, at-thalweg
  expect_error(find_x_for_volume_right(
    1, x0 = 5, topo = topo1, thalweg_height = 12, valley = "right"
  ))
})

test_that("x calculations work with a valley", {
  t <- 11
  eps <- 1e-5
  a1 <- 2.5
  a2 <- a1 + 1
  a3 <- a2 + 0.5
  left_valley <- 2.5
  right_valley <- 4
  # Erode to just before, then just after, the valley.
  l <- find_x_for_volume_right(
    a2 - eps, x0 = 1, topo = topo, thalweg_height = t
  )
  r <- find_x_for_volume_right(
    a2 + eps, x0 = 1, topo = topo, thalweg_height = t
  )
  expect_lt(l, left_valley)
  expect_gt(l, left_valley - 0.1)
  expect_gt(r, right_valley)
  expect_lt(r, right_valley + 0.1)
  # Erode to the valley; check left and right works.
  l <- find_x_for_volume_right(
    a2, x0 = 1, topo = topo, thalweg_height = t, valley = "left"
  )
  r <- find_x_for_volume_right(
    a2, x0 = 1, topo = topo, thalweg_height = t, valley = "right"
  )
  expect_equal(l, left_valley)
  expect_equal(r, right_valley)
  # Beyond the valley
  a2.5 <- mean(c(a2, a3))
  x2.5 <- find_x_for_volume_right(a2.5, x0 = 1, topo = topo, thalweg_height = t)
  expect_gt(x2.5, 4)
  expect_lt(x2.5, 5)
})

rm("topo")
