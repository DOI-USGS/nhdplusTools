context("package setup")

pt_data <- sf::read_sf(system.file("extdata/petapsco_flowlines.gpkg",
                                   package = "nhdplusTools"))

test_that("nhdplus_data path sets and gets right", {
  expect_equal(nhdplus_path(), "../NHDPlusV21_National_Seamless.gdb")

  expect_equal(nhdplus_path("test", warn = FALSE), 0)

  expect_equal(nhdplus_path(), "test")

  expect_warning(nhdplus_path("test", warn = TRUE), "Path does not exist.")

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")
})

context("discover nhdplus id")

test_that("discover nhdplus id errors", {
  skip_on_cran()
  expect_error(discover_nhdplus_id(),
               "Must provide point or nldi_feature input.")

  point <- sf::st_sfc(sf::st_point(c(-76.89303, 39.57934)), crs = 4269)
  expect_warning(discover_nhdplus_id(point),
                 "point too close to edge of catchment.")
})

test_that("discover nhdplus id works as expected", {
  skip_on_cran()
  point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
  expect_equal(discover_nhdplus_id(point), 11689978)
  expect_equal(discover_nhdplus_id(point = point), 11689978)

  nldi_huc12 <- list(featureSource = "huc12pp", featureID = "070700051701")
  expect_equal(discover_nhdplus_id(nldi_feature = nldi_huc12), 13637491)

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
  expect_equal(discover_nhdplus_id(nldi_feature = nldi_nwis), 17864756)


})

context("prepare_nhdplus")

test_that("prep_nhdplus_works and errors as expected", {
  flines_in <- pt_data

  flines <- prepare_nhdplus(flines_in,
                            min_network_size = 10,
                            min_path_length = 1,
                            warn = FALSE)

  expect_error(
    flines <- prepare_nhdplus(
      dplyr::rename(flines_in, LENGTH = LENGTHKM),
      min_network_size = 10,
      min_path_length = 1,
      warn = FALSE),
    paste("Missing some required attributes in call to:",
          "prepare_nhdplus. Expected: LENGTHKM."))
})

test_that("prep_nhdplus leaves non-dendritic", {
  flines_in <- pt_data

  flines <- suppressWarnings(
    prepare_nhdplus(flines_in,
                    min_network_size = 10,
                    min_path_length = 1,
                    purge_non_dendritic = FALSE))

  expect_equal(nrow(flines), 707)

  flines_in$ToNode[150] <-
    flines_in$ToNode[which(!flines_in$ToNode %in% flines_in$FromNode)]

  expect_error(prepare_nhdplus(flines_in,
                               min_network_size = 10,
                               min_path_length = 1,
                               purge_non_dendritic = FALSE,
                               warn = FALSE),
               paste("FromNode - ToNode imply terminal flowlines that are not\n",
                     "flagged terminal. Can't assume NA toCOMIDs go to the ocean."))

})

test_that("prep_nhdplus removes tiny networks", {
  expect_warning(flines <- prepare_nhdplus(
    sf::st_set_geometry(readRDS("data/tiny_network.rds"), NULL),
    min_network_size = 10,
    min_path_length = 1,
    purge_non_dendritic = FALSE),
    paste("Removed 4 flowlines that don't apply.\n",
          "Includes: Coastlines, non-dendritic paths, \nand networks",
          "with drainage area less than 10 sqkm"))
  expect_equal(nrow(flines), 0)
})

test_that("prep_nhdplus works with inland network", {
  flines_in <- pt_data

  flines <- dplyr::filter(flines_in, COMID %in% get_UT(flines_in, 11690564))
  flines <- sf::st_set_geometry(flines, NULL)
  expect_warning(prepare_nhdplus(flines, 0, 0, FALSE, FALSE),
                 "Got NHDPlus data without a Terminal catchment. Attempting to find it.")
})

test_that("prep_nhdplus removes small drainage basins", {
  flines_in <- pt_data
  flines <- prepare_nhdplus(flines_in,
                            min_network_size = 0,
                            min_path_length = 0,
                            min_path_size = 20,
                            purge_non_dendritic = FALSE,
                            warn = FALSE)
  expect_equal(nrow(flines), 303)
})

