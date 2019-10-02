context("align_nhdplus_names")

cida = sf::read_sf(system.file("extdata", "cida_flowlines.gpkg", package = "nhdplusTools"))

test_that("cida names dont work with get_UM", {
  expect_error(get_DM(cida, 8585070))
})

test_that("aligned cida names work", {
  aligned = align_nhdplus_names(cida)
  result <- get_DM(aligned, 8585070)
  expect_equal(length(result), 26)
})
