context("align_nhdplus_names")

cida = sf::st_read('/Users/mikejohnson/Documents/GitHub/nhdplusTools/inst/extdata/cida_flowlines.gpkg')
comid = cida$comid[33]

test_that("cida names dont work with get_UM", {
  expect_error(get_DM(cida, 8585070))
})

test_that("aligned cida names work", {
  aligned = align_nhdplus_names(cida)
  result <- get_DM(aligned, 8585070)
  expect_equal(length(result), 25)
})
