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

context("calculate network attributes")

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

test_that("arbolate sum works", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
  catchment_length <- prepare_nhdplus(walker_flowline, 0, 0,
                                      purge_non_dendritic = FALSE, warn = FALSE) %>%
    left_join(select(walker_flowline, COMID), by = "COMID") %>%
    select(ID = COMID, toID = toCOMID, length = LENGTHKM)

  arb_sum <- calculate_arbolate_sum(catchment_length)

  catchment_length$arb_sum <- arb_sum
  catchment_length$nhd_arb_sum <- walker_flowline$ArbolateSu

  expect(mean(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum)) < 1e-3, "arbolate sum not close enough")
  expect(max(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum)) < 1e-2, "arbolate sum not close enough")
})

test_that("calculate level path", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    ID = test_flowline$COMID,
    toID = test_flowline$toCOMID,
    nameID = walker_flowline$GNIS_ID,
    weight = walker_flowline$ArbolateSu,
    stringsAsFactors = FALSE)

  test_flowline <- left_join(test_flowline,
                             calculate_levelpaths(test_flowline), by = "ID")

  nhdp_lp <- sort(unique(walker_flowline$LevelPathI))
  nhdt_lp <- sort(unique(test_flowline$levelpath))

  expect_true(length(nhdp_lp) == length(nhdt_lp))

  for(lp in seq_along(nhdp_lp)) {
    nhdp <- filter(walker_flowline, LevelPathI == nhdp_lp[lp])
    outlet_comid <- filter(nhdp, Hydroseq == min(Hydroseq))$COMID
    nhdt <- filter(test_flowline, outletID == outlet_comid)
    expect(all(nhdp$COMID %in% nhdt$ID), paste("Mismatch in", nhdp_lp[lp],
                                               "level path from NHDPlus."))
  }
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

