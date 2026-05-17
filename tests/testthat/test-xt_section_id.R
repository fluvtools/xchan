test_that("xt_section_id getter returns seq_len(n) on a fresh xchan", {
  ch <- xchan(list(
    xsection(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    xsection(matrix(c(0, 1, 1, 1), ncol = 2, byrow = TRUE))
  ))
  expect_identical(xt_section_id(ch), c(1L, 2L))
})

test_that("xt_section_id<- assigns and NULL clears", {
  ch <- xchan(list(
    xsection(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    xsection(matrix(c(0, 1, 1, 1), ncol = 2, byrow = TRUE))
  ))
  xt_section_id(ch) <- c("a", "b")
  expect_identical(xt_section_id(ch), c("a", "b"))
  xt_section_id(ch) <- NULL
  expect_null(xt_section_id(ch))
})

test_that("xt_section_id<- rejects wrong length and duplicates", {
  ch <- xchan(list(
    xsection(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    xsection(matrix(c(0, 1, 1, 1), ncol = 2, byrow = TRUE))
  ))
  expect_error(xt_section_id(ch) <- 1L, "length")
  expect_error(xt_section_id(ch) <- c(1L, 1L), "unique")
})

test_that("xt_section_id default method errors on non-xchan", {
  expect_error(xt_section_id(list()), "xchan")
})
