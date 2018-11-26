context("package setup")

test_that("nhdplus_data path sets and gets right", {
  expect_equal(nhdplus_path(), "../NHDPlusV21_National_Seamless.gdb")

  expect_equal(nhdplus_path("test", warn = FALSE), 0)

  expect_equal(nhdplus_path(), "test")

  expect_warning(nhdplus_path("test", warn = TRUE), "Path does not exist.")

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")
})

context("discover nhdplus id")

test_that("discover nhdplus id errors", {
  expect_error(discover_nhdplus_id(),
               "Must provide point or nldi_feature input.")

  point <- sf::st_sfc(sf::st_point(c(-76.89303, 39.57934)), crs = 4269)
  expect_warning(discover_nhdplus_id(point),
                 "point too close to edge of catchment.")
})

test_that("discover nhdplus id works as expected", {

  point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
  expect_equal(discover_nhdplus_id(point), 11689978)
  expect_equal(discover_nhdplus_id(point = point), 11689978)

  nldi_huc12 <- list(featureSource = "huc12pp", featureID = "070700051701")
  expect_equal(discover_nhdplus_id(nldi_feature = nldi_huc12), 13637491)

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
  expect_equal(discover_nhdplus_id(nldi_feature = nldi_nwis), 17864756)


})

test_that("total drainage area works", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  catchment_area <- prepare_nhdplus(walker_flowline, 0, 0,
                               purge_non_dendritic = FALSE, warn = FALSE) %>%
    left_join(select(walker_flowline, COMID, AreaSqKM), by = "COMID") %>%
    select(ID = COMID, toID = toCOMID, area = AreaSqKM)

  new_da <- calculate_total_drainage_area(catchment_area)

  catchment_area$totda <- new_da
  catchment_area$nhdptotda <- walker_flowline$TotDASqKM

  expect(mean(abs(catchment_area$totda - catchment_area$nhdptotda)) < 1e-3, "drainage area not close enough")
  expect(max(abs(catchment_area$totda - catchment_area$nhdptotda)) < 1e-2, "drainage area not close enough")
})
