context("discover nhdplus id")

test_that("nldi basics work", {

  nldi_sources <- discover_nldi_sources()

  expect_equal(class(nldi_sources), "data.frame")

  expect(all(c("comid", "huc12pp", "nwissite") %in% nldi_sources$source))

  expect_equal(names(nldi_sources), c("source", "sourceName", "features"))

})
