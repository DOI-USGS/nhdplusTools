context("reconcile_collapse_flowlines")

test_that("reconcile collapse flowlines works as expected", {
  flines_in <- sf::st_read("data/walker_network.geojson", quiet = TRUE)
  #
  # flines_geo <- select(flines_in, COMID)
  #
  flines <- suppressWarnings(prepare_nhdplus(flines_in, 0, 0))
  flines <- collapse_flowlines(flines, 1, F, 1)
  flines <- reconcile_collapsed_flowlines(flines)

  expect_equal(flines$member_COMID[which(flines$ID == 18)], c(5329323, 5329325, 5329327))

  expect_equal(flines$toID[which(flines$ID == 18)], c(17, 17, 17))

  expect(flines$toID[which(flines$ID == 42)] == 18)
  expect(flines$toID[which(flines$ID == 19)] == 18)

  # flines_rec <- sf::st_as_sf(inner_join(flines, flines_geo, by = c("member_COMID" = "COMID"))) %>%
  #   select(-member_COMID) %>%
  #   distinct() %>%
  #   group_by(ID) %>%
  #   summarise(toID = toID[1], LENGTHKM = LENGTHKM[1], TotDASqKM = TotDASqKM[1]) %>%
  #   st_cast("MULTILINESTRING") %>%
  #   ungroup() %>%
  #   st_line_merge()
  #
  # sf::st_write(flines_rec, "reconciled_output.geojson")
  #
  # plot(flines_rec[c("ID", "geometry")])

})

test_that("collapse works on a double pass", {
  library(sf)
  library(dplyr)

  nhdplus_flines <- readRDS("data/oswego_network.rds")
  split_flines_meters <- 2000
  split_flines_cores <- 3
  collapse_flines_meters <- 500
  collapse_flines_mainstem_meters <- 500

  if(suppressWarnings(require(lwgeom)) & exists("st_linesubstring", where = 'package:lwgeom', mode = "function")) {

  flines <- suppressWarnings(st_set_geometry(nhdplus_flines, NULL) %>%
    prepare_nhdplus(0, 0) %>%
    inner_join(select(nhdplus_flines, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(5070) %>%
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

  # Old Tests:
  expect_equal(collapsed_flines$joined_toCOMID[which(collapsed_flines$COMID == "21975773")], "21975819.1")
  expect_equal(collapsed_flines$joined_toCOMID[which(collapsed_flines$COMID == "21976313")], "21975819.1")

  expect_equal(collapsed_flines$joined_fromCOMID[which(collapsed_flines$COMID == "21976891")],
               collapsed_flines$joined_fromCOMID[which(collapsed_flines$COMID == "21974583")])

  # collapsed_flines %>%
  #   inner_join(select(flines, COMID), by = "COMID") %>%
  #   sf::st_as_sf() %>%
  #   sf::st_transform(4326) %>%
  #   sf::st_write("out_collapsed.gpkg", layer_options = "OVERWRITE=YES")

  collapsed <- reconcile_collapsed_flowlines(collapsed_flines, select(flines, COMID), id = "COMID")

  # These tests are dumb but don't know how else to handle.
  # Checking neighborhood of: c(21976315, 21975773, 21976313, 21975819.1) and 21975817
  expect(collapsed$toID[which(collapsed$ID==58)] == 3019)
  expect(collapsed$toID[which(collapsed$ID==5805)] == 3019)
  expect(collapsed$toID[which(collapsed$ID==59)] == 3022)
  expect(collapsed$toID[which(collapsed$ID==3022)] == 58)

  # sf::st_write(sf::st_transform(collapsed, 4326), "out_reconciled.gpkg", layer_options = "OVERWRITE=YES")

  }
})
