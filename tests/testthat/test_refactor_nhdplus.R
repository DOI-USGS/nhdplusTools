context("three pass")

test_that("refactor_nhdplus works as expected with three pass mode",{

  if(suppressWarnings(require(lwgeom)) & exists("st_linesubstring", where = 'package:lwgeom', mode = "function")) {

  library(dplyr)
  library(sf)
  nhdplus_flines <- sf::st_zm(readRDS("data/north_network.rds"))

  # st_write(nhdplus_flines, "north_network.gpkg")

  split_flines_meters <- 2000
  split_flines_cores <- 3
  collapse_flines_meters <- collapse_flines_mainstem_meters <- 1000
  out_collapsed <- "nhdplus_collapsed.gpkg"
  out_reconciled <- "nhdplus_reconciled.gpkg"

  flines <- suppressWarnings(st_set_geometry(nhdplus_flines, NULL) %>%
    prepare_nhdplus(0, 0) %>%
    inner_join(select(nhdplus_flines, COMID), by = "COMID") %>%
    st_as_sf() %>%
    st_cast("LINESTRING") %>%
    st_transform(5070) %>%
    split_flowlines(split_flines_meters, split_flines_cores))

    collapsed_flines <- collapse_flowlines(st_set_geometry(flines, NULL),
                                           (0.25*collapse_flines_meters/1000),
                                           TRUE,
                                           (0.25*collapse_flines_mainstem_meters/1000))

    collapsed_flines <- suppressWarnings(collapse_flowlines(collapsed_flines,
                                           (0.5*collapse_flines_meters/1000),
                                           TRUE,
                                           (0.5*collapse_flines_mainstem_meters/1000)))

    collapsed_flines <- suppressWarnings(collapse_flowlines(collapsed_flines,
                                           (collapse_flines_meters/1000),
                                           TRUE,
                                           (collapse_flines_mainstem_meters/1000)))

  # collapsed_flines %>%
  #   inner_join(select(flines,
  #                     COMID), by = "COMID") %>%
  #   st_as_sf() %>%
  #   st_transform(4326) %>%
  #   st_write(out_collapsed, layer_options = "OVERWRITE=YES", quiet = T)

  collapsed <- reconcile_collapsed_flowlines(collapsed_flines, select(flines, COMID), id = "COMID")

  collapsed$member_COMID <- unlist(lapply(collapsed$member_COMID, function(x) paste(x, collapse = ",")))

  # st_write(st_transform(collapsed, 4326), out_reconciled, layer_options = "OVERWRITE=YES", quiet = T)

  expect(collapsed$toID[which(collapsed$ID == 29)] == 11)

  # Taken care of in clean up! All kinds of wierd around this one in this test.
  expect(collapsed_flines$joined_fromCOMID[collapsed_flines$COMID == 5876435] == 5876083)

  }
})
