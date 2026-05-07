test_that("side constructors require scalar proportions", {
  expect_error(side_left(c(0.2, 0.8)), "length 1")
  expect_error(side_right(c(0.2, 0.8)), "length 1")
  expect_error(side_both(c(0.2, 0.8), 0.5), "length 1")
  expect_error(side_both(0.5, c(0.2, 0.8)), "length 1")
})

test_that("side constructors return left/right proportion lists", {
  channel <- xt_as_channel(1:4)

  expect_equal(side_left(0.3), list(left = 0.3, right = 0.7), ignore_attr = TRUE)
  expect_equal(side_right(0.3), list(left = 0.7, right = 0.3), ignore_attr = TRUE)
  expect_equal(
    side_both(0.3, 0.7),
    list(left = 0.3, right = 0.7),
    ignore_attr = TRUE
  )
  expect_equal(parse_side_arg(side_left(0.3), channel), rep(0.3, 4))
  expect_equal(parse_side_arg(side_right(0.3), channel), rep(0.7, 4))
  expect_equal(parse_side_arg(side_both(0.3, 0.7), channel), rep(0.3, 4))
})
