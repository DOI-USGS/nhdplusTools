context("split_lines and split_catchments")
library(magrittr)

test_that("split lines works", {

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  flines <- suppressWarnings(prepare_nhdplus(walker_flowline, 0, 0))
  flines <- collapse_flowlines(flines, 1, F, 1)
  flines <- reconcile_collapsed_flowlines(flines)

  flines <- sf::st_as_sf(dplyr::inner_join(flines, dplyr::select(walker_flowline, COMID),
                                by = c("member_COMID" = "COMID"))) %>%
    dplyr::select(-member_COMID) %>%
    dplyr::distinct() %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(toID = toID[1], LENGTHKM = LENGTHKM[1],
              TotDASqKM = TotDASqKM[1]) %>%
    sf::st_cast("MULTILINESTRING") %>%
    dplyr::ungroup() %>%
    sf::st_line_merge()

  split <- nhdplusTools:::split_lines(sf::st_transform(dplyr::select(flines, ID),
                                                       5070), 250, id = "ID")

  expect(nrow(split) == 574)

  }

})

test_that("split lines works", {

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {

  flines_in <- readRDS("data/guadalupe_network_geom.rds")

  flines <- suppressWarnings(
    sf::st_set_geometry(flines_in, NULL) %>%
    prepare_nhdplus(0, 0) %>%
    dplyr::inner_join(dplyr::select(flines_in, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(5070) %>%
    split_flowlines(2000, 3))

  expect(length(which(grepl("1623361", as.character(flines$COMID)))) == 10)

  }

})

test_that("split_lines_2 works the same as split_lines", {

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {

  flines_in <- readRDS("data/oswego_network.rds")

  flines_in <- suppressWarnings(
    sf::st_set_geometry(flines_in, NULL) %>%
      prepare_nhdplus(0, 0) %>%
      dplyr::inner_join(dplyr::select(flines_in, COMID), by = "COMID") %>%
      sf::st_as_sf() %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_transform(5070))

  flines <- nhdplusTools:::split_lines(flines_in, 2000,
                                       id = "COMID", para = 3)

  expect_warning(flines_2 <- nhdplusTools:::split_lines_2(flines_in, 2000,
                                                          id = "COMID"),
   "Found 139 geometries without very many vertices. Densifying")

  expect_equal(nrow(flines), nrow(flines_2))

  }

})

test_that("split_catchments works", {
  unlink("data/temp/*")
  dir.create("data/temp", showWarnings = FALSE, recursive = TRUE)

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 2000,
                               collapse_flines_meters = 1,
                               collapse_flines_main_meters = 1,
                               split_flines_cores = 2,
                               out_collapsed = "data/temp/subset_refactor.gpkg",
                               out_reconciled = "data/temp/subset_reconcile.gpkg",
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)

  fline_ref <- sf::read_sf("data/temp/subset_refactor.gpkg") %>%
    dplyr::arrange(COMID)
  fline_rec <- sf::read_sf("data/temp/subset_reconcile.gpkg")

  test_flines <- dplyr::filter(fline_ref, as.integer(COMID) == 5329435)

  test_cat <- dplyr::filter(walker_catchment, FEATUREID == 5329435)

  expect(nrow(test_flines) == 5, "got wrong number of test_flines")

  split_cat <- split_catchment(test_cat, test_flines, walker_fdr, walker_fac)

  expect(length(split_cat) == 5, "Got the wrong number of cathment split polygons")
  expect(all(c("XY", "MULTIPOLYGON", "sfg") %in% class(split_cat[[5]])),
         "Got wrong class for polygon with pixel dribble out the bottom")
  expect(all(c("XY", "POLYGON", "sfg") %in% class(split_cat[[2]])),
         "Got wrong class for polygon with one ring")

  test_fline_ref <- fline_ref[1:9, ] # this sucks, but works.
  test_fline_rec <- dplyr::filter(fline_rec,
                              member_COMID %in% as.character(test_fline_ref$COMID))

  test_cat <- dplyr::filter(walker_catchment,
                  FEATUREID %in% unique(as.integer(test_fline_ref$COMID)))

  reconciled_cats <- reconcile_catchments(test_cat, test_fline_ref, test_fline_rec,
                                          walker_fdr, walker_fac)

  expect(nrow(reconciled_cats) == nrow(test_fline_ref), "got the wrong number of split catchments")
  expect(all(reconciled_cats$member_COMID %in% test_fline_ref$COMID))

  unlink("data/temp/*")
})

test_that("split and combine works", {
  unlink("data/temp/*")
  dir.create("data/temp", showWarnings = FALSE, recursive = TRUE)

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 2000,
                               collapse_flines_meters = 1000,
                               collapse_flines_main_meters = 1000,
                               split_flines_cores = 2,
                               out_collapsed = "data/temp/subset_refactor.gpkg",
                               out_reconciled = "data/temp/subset_reconcile.gpkg",
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)

  fline_ref <- sf::read_sf("data/temp/subset_refactor.gpkg") %>%
    dplyr::arrange(COMID)
  fline_rec <- sf::read_sf("data/temp/subset_reconcile.gpkg")

  test_cat_1 <- fline_rec$member_COMID[which(nchar(fline_rec$member_COMID) ==
                                              max(nchar(fline_rec$member_COMID)))]
  test_cat_2 <- fline_rec$member_COMID[which(fline_rec$ID == 2)]

  test_cat_list <- c(unlist(strsplit(test_cat_1, ",")), unlist(strsplit(test_cat_2, ",")))

  test_cat <- dplyr::filter(walker_catchment, FEATUREID %in% as.integer(test_cat_list))

  test_fline_ref <- dplyr::filter(fline_ref, as.integer(COMID) %in%
                                    as.integer(test_cat_list))

  test_fline_rec <- dplyr::filter(fline_rec, member_COMID %in%
                                     c(test_cat_1, test_cat_2))

  reconciled_cats <- reconcile_catchments(test_cat, test_fline_ref,
                                          test_fline_rec, walker_fdr, walker_fac)

  expect(nrow(reconciled_cats) == nrow(test_fline_rec))
  expect(all(reconciled_cats$member_COMID %in% test_fline_rec$member_COMID))

  unlink("data/temp/*")
})


test_that("split and combine works with combine from split", {

  # "166755072,8866562.2"
  # "8833300.1", "8833300.2"

  fdr <- raster::raster("data/reconcile_test_fdr.tif")
  fac <- raster::raster("data/reconcile_test_fac.tif")
  test_fline_ref <- sf::read_sf("data/reconcile_test.gpkg", "fline_ref")
  test_fline_rec <- sf::read_sf("data/reconcile_test.gpkg", "fline_rec")
  test_cat <- sf::read_sf("data/reconcile_test.gpkg", "catchment")

  reconciled_cats <- reconcile_catchments(test_cat, test_fline_ref,
                                          test_fline_rec, fdr, fac)

  expect(nrow(reconciled_cats) == nrow(test_fline_rec) - 1,
         "Got the wrong number of reconciled catchments")

  expect(all(reconciled_cats$member_COMID %in% test_fline_rec$member_COMID))

  expect(reconciled_cats[which(reconciled_cats$ID == 7912),
                         ]$member_COMID == "8833300.1")
  expect(reconciled_cats[which(reconciled_cats$ID == 3108),
                         ]$member_COMID == "166755072,8866562.2")
})


test_that("doing nothing does nothing", {
  unlink("data/temp/*")
  dir.create("data/temp", showWarnings = FALSE, recursive = TRUE)

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 200000,
                               collapse_flines_meters = 1,
                               collapse_flines_main_meters = 1,
                               split_flines_cores = 2,
                               out_collapsed = "data/temp/subset_refactor.gpkg",
                               out_reconciled = "data/temp/subset_reconcile.gpkg",
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)

  fline_ref <- sf::read_sf("data/temp/subset_refactor.gpkg") %>%
    dplyr::arrange(COMID)
  fline_rec <- sf::read_sf("data/temp/subset_reconcile.gpkg")

  expect(nrow(fline_ref) == nrow(fline_rec))
  expect(nrow(fline_ref) == nrow(walker_catchment))

  unlink("data/temp/*")
})
