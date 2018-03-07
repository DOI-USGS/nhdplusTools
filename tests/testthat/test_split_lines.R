context("split_lines")

test_that("split lines works", {
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

})
