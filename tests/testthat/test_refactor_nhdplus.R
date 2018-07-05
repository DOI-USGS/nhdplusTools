context("refactor wrapper")

test_that("refactor_nhdplus works as expected with three pass mode", {

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring", where = "package:lwgeom", mode = "function")) {

  nhdplus_flines <- sf::st_zm(readRDS("data/north_network.rds"))

  split_flines_meters <- 2000
  split_flines_cores <- 3
  collapse_flines_meters <- collapse_flines_main_meters <- 1000
  out_collapsed <- "nhdplus_collapsed.gpkg"
  out_reconciled <- "nhdplus_reconciled.gpkg"

  flines <- suppressWarnings(sf::st_set_geometry(nhdplus_flines, NULL) %>%
    prepare_nhdplus(0, 0) %>%
    dplyr::inner_join(select(nhdplus_flines, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_transform(5070) %>%
    split_flowlines(split_flines_meters, split_flines_cores))

    collapsed_flines <- collapse_flowlines(sf::st_set_geometry(flines, NULL),
                                           (0.25 * collapse_flines_meters / 1000),
                                           TRUE,
                                           (0.25 * collapse_flines_main_meters / 1000))

    collapsed_flines <- suppressWarnings(collapse_flowlines(collapsed_flines,
                                           (0.5 * collapse_flines_meters / 1000),
                                           TRUE,
                                           (0.5 * collapse_flines_main_meters / 1000)))

    collapsed_flines <- suppressWarnings(collapse_flowlines(collapsed_flines,
                                           (collapse_flines_meters / 1000),
                                           TRUE,
                                           (collapse_flines_main_meters / 1000)))

  collapsed <- reconcile_collapsed_flowlines(collapsed_flines, select(flines, COMID), id = "COMID")

  collapsed$member_COMID <- unlist(lapply(collapsed$member_COMID, function(x) paste(x, collapse = ",")))

  expect(collapsed$toID[which(collapsed$ID == 29)] == 11)

  # Taken care of in clean up! All kinds of wierd around this one in this test.
  expect(collapsed_flines$joined_fromCOMID[collapsed_flines$COMID == 5876435] == 5876083)

  }
})

test_that("The refactor_nhdplus function runs as expected", {
  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring", where = "package:lwgeom", mode = "function")) {

  nhdplus_flowlines <- sf::st_zm(readRDS("data/north_network.rds"))

  suppressWarnings( # Known warnings -- just check for errors.
    refactor_nhdplus(nhdplus_flines = nhdplus_flowlines,
                   split_flines_meters = 2000, split_flines_cores = 3,
                   collapse_flines_meters = 500, collapse_flines_main_meters = 500,
                   out_collapsed = "temp.gpkg",
                   out_reconciled = "temp_rec.gpkg",
                   three_pass = TRUE,
                   warn = FALSE)
    )

  expect(file.exists("temp.gpkg"))
  expect(file.exists("temp_rec.gpkg"))
  unlink("temp.gpkg")
  unlink("temp_rec.gpkg")

  refactor_nhdplus(nhdplus_flines = nhdplus_flowlines,
                   split_flines_meters = 2000, split_flines_cores = 3,
                   collapse_flines_meters = 500, collapse_flines_main_meters = 500,
                   out_collapsed = "temp.gpkg",
                   out_reconciled = "temp_rec.gpkg",
                   three_pass = FALSE,
                   warn = FALSE)

  expect(file.exists("temp.gpkg"))
  expect(file.exists("temp_rec.gpkg"))

  unlink("temp.gpkg")
  unlink("temp_rec.gpkg")

  }
})

context("prepare_nhdplus")

test_that("prep_nhdplus_works", {
  flines <- suppressWarnings(prepare_nhdplus(
    readRDS("data/petapsco_network.rds"),
    min_network_size = 10,
    min_path_length = 1))
  expect_equal(
    flines,
    readRDS("data/petapsco_prepared.rds"))
})

test_that("prep_nhdplus leaves non-dendritic", {
  flines_in <- readRDS("data/petapsco_network.rds")

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
