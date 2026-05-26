test_that("plot.xchan warns when extent is full but channel has no profiles", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(c(2, 2))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_warning(plot(ch, extent = "full"), "profile")
})

test_that("plot.xchan overlay uses add = TRUE without opening a new plot", {
  skip_if_not_installed("sf")
  foo <- xt_as_channel(rexp(4))
  bar <- xt_widen(foo, side = "left", dw = 2)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(bar, col = "red")
  usr1 <- graphics::par("usr")
  plot(foo, add = TRUE, col = "blue")
  usr2 <- graphics::par("usr")
  expect_equal(usr2, usr1)
})

test_that("plot.xchan axis argument runs without error", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(c(2, 2))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(ch, axis = "arrows"), NA)
  expect_error(plot(ch, axis = "line"), NA)
  expect_error(plot(ch, axis = "none"), NA)
})

test_that("plot.xchan banks show/hide/auto runs without error", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(c(2, 2, 2), crs = 3005)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(ch, banks = "show"), NA)
  expect_error(plot(ch, banks = "hide"), NA)
  expect_error(plot(ch, banks = "auto"), NA)
  bl <- sf::st_buffer(
    sf::st_union(sf::st_geometry(channel_plan(ch))),
    dist = 0.5
  )
  xt_bankline(ch) <- bl
  expect_error(plot(ch, banks = "auto"), NA)
  expect_error(plot(ch, banks = "show"), NA)
})

test_that("plot_plan limits aspect for wide synthetic numeric channels", {
  skip_if_not_installed("sf")
  ch <- xt_as_channel(c(10, 12, 8, 15, 11, 9))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  xchan:::plot_plan(ch, axis = "none")
  usr <- graphics::par("usr")
  dx <- usr[2L] - usr[1L]
  dy <- usr[4L] - usr[3L]
  expect_true(max(dx, dy) / min(dx, dy) <= 4.25)
})
