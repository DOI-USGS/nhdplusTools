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

  flines_rec <- sf::st_as_sf(inner_join(flines, flines_geo, by = c("member_COMID" = "COMID"))) %>%
    select(-member_COMID) %>%
    distinct() %>%
    group_by(ID) %>%
    summarise(toID = toID[1], LENGTHKM = LENGTHKM[1], TotDASqKM = TotDASqKM[1]) %>%
    st_cast("MULTILINESTRING") %>%
    ungroup() %>%
    st_line_merge()

  sf::st_write(flines_rec, "reconciled_output.geojson")
  #
  # plot(flines_rec[c("ID", "geometry")])

})
