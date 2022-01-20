

source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

pt_data <- sample_flines

test_that("nhdplus_data path sets and gets right", {
  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")

  expect_equal(nhdplus_path(), "../NHDPlusV21_National_Seamless.gdb")

  expect_equal(nhdplus_path("test", warn = FALSE), 0)

  expect_equal(nhdplus_path(), "test")

  expect_warning(nhdplus_path("test", warn = TRUE), "Path does not exist.")

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")
})

test_that("nhdplusTools_data_path works", {
  check <- tools::R_user_dir("usgs_r/nhdplusTools")

  orig <- nhdplusTools_data_dir()

  expect_equal(nhdplusTools_data_dir(),
               check)

  expect_equal(nhdplusTools_data_dir("test"),
               "test")

  expect_equal(nhdplusTools_data_dir(check),
               check)

  nhdplusTools_data_dir(orig)
})



test_that("discover nhdplus id errors", {
  skip_on_cran()
  expect_error(discover_nhdplus_id(),
               "Must provide point or nldi_feature input.")

  point <- sf::st_sfc(sf::st_point(c(-76.89303, 39.57934)), crs = 4269)

})

test_that("discover nhdplus id works as expected", {
  skip_on_cran()
  point <- sf::st_sfc(sf::st_point(c(-76.874, 39.482)), crs = 4326)
  expect_equal(discover_nhdplus_id(point), 11689978)

  expect_equal(discover_nhdplus_id(point = point), 11689978)

  # raindrop is broken for now
  # expect_equal(discover_nhdplus_id(point, raindrop = TRUE)$comid[1], 11689978)

  nldi_huc12 <- list(featureSource = "huc12pp", featureID = "070700051701")
  expect_equal(discover_nhdplus_id(nldi_feature = nldi_huc12), 13637491)

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
  expect_equal(discover_nhdplus_id(nldi_feature = nldi_nwis), 17864756)


})



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

  flines <- prepare_nhdplus(flines_in,
                            min_network_size = 10,
                            min_path_length = 1,
                            warn = FALSE,
                            skip_toCOMID = TRUE)

  expect_true(!"toCOMID" %in% names(flines))
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
    sf::st_set_geometry(readRDS(
      list.files(pattern = "tiny_network.rds", full.names = TRUE, recursive = TRUE)), NULL),
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
  expect_warning(prepare_nhdplus(flines, 0, 0, 0, FALSE, FALSE),
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

test_that("get_tocomid", {

  tocomid <- get_tocomid(sample_flines)

  expect_equal(nrow(tocomid), nrow(sample_flines))

  expect_true(!any(is.na(tocomid$tocomid)))

  tocomid <- get_tocomid(sample_flines, missing = NA, return_dendritic = FALSE)

  expect(length(tocomid$tocomid), 714)

  expect(sum(is.na(tocomid$tocomid)), 1)

  expect_error(get_tocomid(dplyr::select(sample_flines, -Divergence)))

  tocomid <- get_tocomid(sample_flines, add = FALSE)

  expect_equal(names(tocomid), c("comid", "tocomid"))
})

test_that("compatibalize", {
  one <- pt_data

  attr(one, "sf_column") <- "geotest"
  names(one)[names(one) == "geom"] <- "geotest"

  two <- sf::st_transform(pt_data, 5070)

  three <- st_compatibalize(one, two)

  expect_equal(sf::st_crs(two), sf::st_crs(three))

  expect_true(all(names(two) == names(three)))

})

test_that("rname geometry", {
  g <- sf::st_sf(a=3, geo = sf::st_sfc(sf::st_point(1:2)))

  g <- rename_geometry(g, "geometry")

  expect_true("geometry" %in% names(g))

  expect_equal(attr(g, "sf_column"), "geometry")

})

