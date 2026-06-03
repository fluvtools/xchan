test_that("bathy constructors return xchan_bathymetry objects", {
  rect <- bathy_rectangle(depth = 2)
  vsh <- bathy_vshape(depth = 3, thalweg_frac = 0.4)
  expect_s3_class(rect, "xchan_bathymetry")
  expect_s3_class(vsh, "xchan_bathymetry")
  expect_equal(rect$shape, "rectangle")
  expect_equal(vsh$shape, "vshape")
})

test_that("bathy_vshape accepts thalweg_frac at 0 and 1", {
  expect_s3_class(bathy_vshape(depth = 1, thalweg_frac = 0), "xchan_bathymetry")
  expect_s3_class(bathy_vshape(depth = 1, thalweg_frac = 1), "xchan_bathymetry")
})

test_that("bathy_vshape rejects thalweg_frac outside [0, 1]", {
  expect_error(bathy_vshape(depth = 1, thalweg_frac = -0.1))
  expect_error(bathy_vshape(depth = 1, thalweg_frac = 1.1))
})

test_that("print.xchan_bathymetry formats units depth", {
  skip_if_not_installed("units")
  rect <- bathy_rectangle(units::set_units(2, "m"))
  out <- testthat::capture_output(print(rect))
  expect_match(out, "* depth: 2 m", fixed = TRUE)
})

test_that("print.xchan_bathymetry shows shape and bullet arguments", {
  rect <- bathy_rectangle(depth = 2)
  out <- testthat::capture_output(print(rect))
  expect_match(out, "Bathymetry specification: rectangle", fixed = TRUE)
  expect_match(out, "* depth: 2", fixed = TRUE)
  expect_false(grepl("wse", out, fixed = TRUE))
})
