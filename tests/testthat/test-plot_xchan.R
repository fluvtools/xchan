test_that("plot.xchan warns when extent is full but channel has no profiles", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(c(2, 2))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_warning(plot(ch, extent = "full"), "profile")
})
