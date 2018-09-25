context("split_lines and split_catchments")
library(magrittr)

test_that("split lines works", {

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {

  flines_in <- st_read("data/walker_network.geojson", quiet = TRUE)

  flines <- suppressWarnings(prepare_nhdplus(flines_in, 0, 0))
  flines <- collapse_flowlines(flines, 1, F, 1)
  flines <- reconcile_collapsed_flowlines(flines)

  flines <- st_as_sf(dplyr::inner_join(flines, select(flines_in, COMID),
                                by = c("member_COMID" = "COMID"))) %>%
    dplyr::select(-member_COMID) %>%
    dplyr::distinct() %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(toID = toID[1], LENGTHKM = LENGTHKM[1],
              TotDASqKM = TotDASqKM[1]) %>%
    st_cast("MULTILINESTRING") %>%
    dplyr::ungroup() %>%
    st_line_merge()

  split <- nhdplusTools:::split_lines(st_transform(select(flines, ID), 5070), 250, id = "ID")

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
    dplyr::inner_join(select(flines_in, COMID), by = "COMID") %>%
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
      dplyr::inner_join(select(flines_in, COMID), by = "COMID") %>%
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

  dir.create("data/temp", showWarnings = FALSE, recursive = TRUE)

  flines_in <- sf::read_sf("data/walker.gpkg", "NHDFlowline_Network")
  catchments <- sf::read_sf("data/walker.gpkg", "CatchmentSP")

  # nolint start
  # r <- fasterize::raster("NHDPlusCA/fdr.tif")
  #
  # cropper <- catchments %>%
  #   st_transform(as.character(raster::crs(r))) %>%
  #   st_union() %>%
  #   st_buffer(1000) %>%
  #   as_Spatial()
  #
  # fac <- fasterize::raster("NHDPlusCA/fac.tif")
  # sub_fac <- raster::crop(fac, cropper)
  # sub_r <- raster::crop(r, cropper)
  # raster::writeRaster(sub_fac, "data/walker_fac.tif", overwrite = TRUE)
  # raster::writeRaster(sub_r, "data/walker_fdr.tif", overwrite = TRUE)
  # nolint end

  fdr <- raster::raster("data/walker_fdr.tif")
  fac <- raster::raster("data/walker_fac.tif")

  refactor <- refactor_nhdplus(nhdplus_flines = flines_in,
                               split_flines_meters = 2000,
                               collapse_flines_meters = 1,
                               collapse_flines_main_meters = 1,
                               split_flines_cores = 2,
                               out_collapsed = "data/temp/subset_refactor.gpkg",
                               out_reconciled = "data/temp/subset_reconcile.gpkg",
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)

  flines <- sf::read_sf("data/temp/subset_reconcile.gpkg") %>%
    dplyr::arrange(member_COMID)

  proj <- as.character(raster::crs(fdr))
  test_flines <- dplyr::filter(flines, member_COMID %in% c(5329435.1,
                                                           5329435.2,
                                                           5329435.3,
                                                           5329435.4,
                                                           5329435.5)) %>%
    sf::st_transform(proj)


  test_cat <- dplyr::filter(catchments, FEATUREID == 5329435) %>%
    sf::st_transform(proj)

  split_cat <- split_catchment(test_cat, test_flines, fdr, fac)

  expect(length(split_cat) == 5, "Got the wrong number of cathment split polygons")
  expect(all(c("XY", "MULTIPOLYGON", "sfg") %in% class(split_cat[[5]])),
         "Got wrong class for polygon with pixel dribble out the bottom")
  expect(all(c("XY", "POLYGON", "sfg") %in% class(split_cat[[2]])),
         "Got wrong class for polygon with one ring")
  unlink("data/temp/*")
})
