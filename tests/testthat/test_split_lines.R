context("split_lines")

test_that("split lines works", {

  if(suppressWarnings(require(lwgeom)) & exists("st_linesubstring", where = 'package:lwgeom', mode = "function")) {


  library(sf)
  library(dplyr)

  flines_in <- st_read("data/walker_network.geojson", quiet = TRUE)

  flines <- suppressWarnings(prepare_nhdplus(flines_in, 0, 0))
  flines <- collapse_flowlines(flines, 1, F, 1)
  flines <- reconcile_collapsed_flowlines(flines)

  flines <- st_as_sf(inner_join(flines, select(flines_in, COMID), by = c("member_COMID" = "COMID"))) %>%
    select(-member_COMID) %>%
    distinct() %>%
    group_by(ID) %>%
    summarise(toID = toID[1], LENGTHKM = LENGTHKM[1], TotDASqKM = TotDASqKM[1]) %>%
    st_cast("MULTILINESTRING") %>%
    ungroup() %>%
    st_line_merge()

  split <- split_lines(st_transform(select(flines, ID), 5070), 250, id = "ID")

  expect(nrow(split) == 574)

  }

})

test_that("split lines works", {

  if(suppressWarnings(require(lwgeom)) & exists("st_linesubstring", where = 'package:lwgeom', mode = "function")) {


  library(sf)
  library(dplyr)

  flines_in <- readRDS("data/guadalupe_network_geom.rds")

  flines <- suppressWarnings(
    st_set_geometry(flines_in, NULL) %>%
    prepare_nhdplus(0, 0) %>%
    inner_join(select(flines_in, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(5070) %>%
    split_flowlines(2000, 3))

  expect(length(which(grepl("1623361", as.character(flines$COMID)))) == 10)

  }

})

test_that("split_lines_2 works the same as split_lines", {

  if(suppressWarnings(require(lwgeom)) & exists("st_linesubstring", where = 'package:lwgeom', mode = "function")) {

  flines_in <- readRDS("data/oswego_network.rds")

  flines_in <- suppressWarnings(
    st_set_geometry(flines_in, NULL) %>%
      prepare_nhdplus(0, 0) %>%
      inner_join(select(flines_in, COMID), by = "COMID") %>%
      sf::st_as_sf() %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_transform(5070))

  flines <- nhdplusTools:::split_lines(flines_in, 2000, id = "COMID", para = 3)

  expect_warning(flines_2 <- nhdplusTools:::split_lines_2(flines_in, 2000, id = "COMID"),
                 "Found 139 geometries without very many vertices. Densifying")

  expect_equal(nrow(flines), nrow(flines_2))

  }

})
