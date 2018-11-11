context("split_lines")

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
