test_that("Eroded volume is correct when eroding into a gully.", {
  xs <- xt_cross_section(
    matrix(
      c(
        0, 10.5,
        1, 11,
        2, 11,
        3, 9,
        4, 11,
        5, 10,
        6, 11
      ),
      byrow = TRUE, ncol = 2
    ),
    x_lb = 4, x_rb = 6
  )
  # plot(xs)
  ## Try gradually increasing the distance, comparing against known volumes.
  area0.5 <- 0.5 * 1 / 2
  expect_equal(erosion_volume_left(xs, 0.5), area0.5)
  area1 <- area0.5
  expect_equal(erosion_volume_left(xs, 1), area1)
  area1.5 <- area1
  expect_equal(erosion_volume_left(xs, 1.5), area1.5)
  area2 <- area1.5 + 0.5 * 1 / 2
  expect_equal(erosion_volume_left(xs, 2), area2)
  area3 <- area2 + 1 * 1
  expect_equal(erosion_volume_left(xs, 3), area3)
  area4 <- area3 + 0.5 * 1 + 0.5 * 1 / 2
  expect_equal(erosion_volume_left(xs, 4), area4)
  ## Now, erode into the gully; then try different distances.
  dw <- 0.75
  gone_area <- erosion_volume_left(xs, dw)
  expect_lt(xs$left$thalweg[2], xs$left$bank[2])
  xs <- suppressWarnings(xt_widen_left_2d(xs, dw))
  # plot(xs)
  expect_gt(xs$left$thalweg[2], xs$left$bank[2])
  expect_equal(erosion_volume_left(xs, 0.25), 0)
  expect_equal(erosion_volume_left(xs, 0.5), 0)
  expect_equal(erosion_volume_left(xs, 1.5 - dw), area1.5 - gone_area)
  expect_equal(erosion_volume_left(xs, 2 - dw), area2 - gone_area)
  expect_equal(erosion_volume_left(xs, 3 - dw), area3 - gone_area)
  expect_equal(erosion_volume_left(xs, 4 - dw), area4 - gone_area)
})


test_that("Right erosion works, too.", {
  # Use the same cross section as before, but manually flipped.
  xs <- xt_cross_section(
    matrix(
      c(
        0, 10.5,
        -1, 11,
        -2, 11,
        -3, 9,
        -4, 11,
        -5, 10,
        -6, 11
      ),
      byrow = TRUE, ncol = 2
    ),
    x_lb = -6, x_rb = -4
  )
  # plot(xs)
  ## Try gradually increasing the distance, comparing against known volumes.
  area0.5 <- 0.5 * 1 / 2
  expect_equal(xt_erosion_volume_right(xs, 0.5), area0.5)
  area1 <- area0.5
  expect_equal(xt_erosion_volume_right(xs, 1), area1)
  area1.5 <- area1
  expect_equal(xt_erosion_volume_right(xs, 1.5), area1.5)
  area2 <- area1.5 + 0.5 * 1 / 2
  expect_equal(xt_erosion_volume_right(xs, 2), area2)
  area3 <- area2 + 1 * 1
  expect_equal(xt_erosion_volume_right(xs, 3), area3)
  area4 <- area3 + 0.5 * 1 + 0.5 * 1 / 2
  expect_equal(xt_erosion_volume_right(xs, 4), area4)
  ## Now, erode into the gully; then try different distances.
  dw <- 0.75
  gone_area <- xt_erosion_volume_right(xs, dw)
  expect_lt(xs$right$thalweg[2], xs$right$bank[2])
  xs <- suppressWarnings(xt_widen_right_2d(xs, dw))
  # plot(xs)
  expect_gt(xs$right$thalweg[2], xs$right$bank[2])
  expect_equal(xt_erosion_volume_right(xs, 0.25), 0)
  expect_equal(xt_erosion_volume_right(xs, 0.5), 0)
  expect_equal(xt_erosion_volume_right(xs, 1.5 - dw), area1.5 - gone_area)
  expect_equal(xt_erosion_volume_right(xs, 2 - dw), area2 - gone_area)
  expect_equal(xt_erosion_volume_right(xs, 3 - dw), area3 - gone_area)
  expect_equal(xt_erosion_volume_right(xs, 4 - dw), area4 - gone_area)
})
