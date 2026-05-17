test_that("xt_generate_plan stores footprint on xt_bankline()", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 8)
  bl <- xt_bankline(ch)
  expect_s3_class(bl, "sfc")
  expect_true(all(sf::st_is(bl, c("POLYGON", "MULTIPOLYGON"))))
  expect_true(sf::st_equals(bl, sf::st_geometry(fraser_bankline), sparse = FALSE)[1L, 1L])
})

test_that("[.xchan and xt_arrange_downstream preserve bankline", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 9)
  bl0 <- xt_bankline(ch)
  sub <- ch[c(3, 1, 7)]
  expect_true(sf::st_equals(xt_bankline(sub), bl0, sparse = FALSE)[1L, 1L])
  set.seed(2)
  sh <- ch[sample.int(length(ch))]
  back <- xt_arrange_downstream(sh)
  expect_true(sf::st_equals(xt_bankline(back), bl0, sparse = FALSE)[1L, 1L])
})

test_that("xt_bankline<- NULL clears footprint", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 5)
  expect_false(is.null(xt_bankline(ch)))
  xt_bankline(ch) <- NULL
  expect_null(xt_bankline(ch))
})

test_that("plot.xchan runs with stored bankline", {
  skip_if_not_installed("sf")
  ch <- xt_generate_plan(fraser_bankline, n = 6)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(ch, axis = "none"), NA)
})
