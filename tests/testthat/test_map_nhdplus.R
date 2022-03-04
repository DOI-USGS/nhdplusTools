source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

test_that("map_nhdplus", {
  skip_on_cran()

  out1 = map_nhdplus("05428500")
  expect_equal(class(out1), "list")
  expect_true(inherits(out1$outlets, "sf"))

  out2 = map_nhdplus(list(13293970, 13293750))
  expect_equal(class(out2), "list")
  expect_true(inherits(out2$flowline, "sf"))

  out3 = map_nhdplus(list(13293970, 13293750), streamorder = 3, nhdplus_data = sample_data)

  expect_equal(class(out3), "list")
  expect_true(inherits(out3$network_wtbd, "sf"))

  #return leaflet object
  out4 = map_nhdplus("05428500", return_map = TRUE)
  expect_true(inherits(out4, "leaflet"))
})
