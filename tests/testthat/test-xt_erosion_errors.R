profile_for_width <- function(half_width) {
  xchan:::new_profile(
    coords = matrix(
      c(-half_width, 11, 0, 9, half_width, 11),
      ncol = 2,
      byrow = TRUE
    ),
    bankpoints = c(-half_width, half_width)
  )
}

test_that("xt_erosion_width reports all failing cross sections", {
  ch <- xchan:::set_channel_profile(
    xt_as_channel(c(10, 12), crs = 3005),
    list(profile_for_width(5), profile_for_width(6))
  )
  xt_section_id(ch) <- c("A", "B")

  err <- tryCatch(
    xt_erosion_width(ch, dv = c(1e6, 1e6)),
    error = identity
  )
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_match(msg, "cross section 1 \\(id = A\\)")
  expect_match(msg, "cross section 2 \\(id = B\\)")
})

test_that("xt_erosion_volume reports failing cross section", {
  ch <- xchan:::set_channel_profile(
    xt_as_channel(10, crs = 3005),
    list(profile_for_width(5))
  )

  err <- tryCatch(
    xt_erosion_volume(ch, dw = 1e6),
    error = identity
  )
  expect_match(conditionMessage(err), "cross section 1")
  expect_match(conditionMessage(err), "extent is surpassed")
})
