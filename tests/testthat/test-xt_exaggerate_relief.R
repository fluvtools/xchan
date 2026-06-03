test_that("xt_exaggerate_relief(xs_profile) scales height above thalweg", {
  coords <- matrix(c(-2, 10, 0, 8, 2, 12), ncol = 2, byrow = TRUE)
  xs <- xchan:::new_profile(coords, bankpoints = c(-2, 2))

  out <- xt_exaggerate_relief(xs, times = 2)

  expect_s3_class(out, "xs_profile")
  expect_equal(out$coordinates[, 1], xs$coordinates[, 1])
  expect_equal(out$coordinates[, 2], c(12, 8, 16))
})

test_that("xt_exaggerate_relief(xchan) updates all profiles", {
  coords <- matrix(c(-2, 10, 0, 8, 2, 12), ncol = 2, byrow = TRUE)
  xs <- xchan:::new_profile(coords, bankpoints = c(-2, 2))
  ch <- xchan:::set_channel_profile(
    xt_as_channel(c(4, 4), crs = 3005),
    list(xs, xs)
  )

  out <- xt_exaggerate_relief(ch)
  prof <- channel_profile(out)

  expect_s3_class(out, "xchan")
  expect_equal(prof[[1]]$coordinates[, 2], c(12, 8, 16))
  expect_equal(prof[[2]]$coordinates[, 2], c(12, 8, 16))
})

test_that("xt_exaggerate_relief errors when channel has no profiles", {
  ch <- xt_as_channel(c(4, 4), crs = 3005)
  expect_error(
    xt_exaggerate_relief(ch),
    "requires a channel with profile"
  )
})
