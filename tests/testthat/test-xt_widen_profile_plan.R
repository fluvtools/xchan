test_that("xt_widen keeps plan and profile widths aligned on demo channel", {
  skip_if_not_installed("terra")

  squamish <- xt_generate_plan(squamish_bankline, n = 10)
  squamish <- squamish_with_profiles(squamish, sample_freq = 10)

  widened <- suppressWarnings(
    xt_widen(squamish, dw = 20, side = "right")
  )

  plan_w <- as.numeric(xt_width(widened))
  prof_w <- vapply(
    lapply(widened, function(x) x$profile),
    xt_width,
    numeric(1)
  )
  expect_equal(prof_w, plan_w)

  i <- 9L
  left_before <- get_left_bank_coords(squamish[[i]]$profile)[1]
  right_before <- get_right_bank_coords(squamish[[i]]$profile)[1]
  left_after <- get_left_bank_coords(widened[[i]]$profile)[1]
  right_after <- get_right_bank_coords(widened[[i]]$profile)[1]
  expect_equal(mean(c(left_after, right_after)), 0)
  expect_equal(left_after, left_before - 10)
  expect_equal(right_after, right_before + 10)
})
