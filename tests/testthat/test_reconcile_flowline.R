context("reconcile_collapse_flowlines")

test_that("reconcile collapse flowlines works as expected", {

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  flines <- suppressWarnings(prepare_nhdplus(walker_flowline, 0, 0))
  flines <- collapse_flowlines(flines, 1, F, 1)
  flines <- reconcile_collapsed_flowlines(flines)

  expect_equal(flines$member_COMID[which(flines$ID == 18)],
               c(5329323, 5329325, 5329327))

  expect_equal(flines$toID[which(flines$ID == 18)], c(17, 17, 17))

  expect(flines$toID[which(flines$ID == 42)] == 18)
  expect(flines$toID[which(flines$ID == 19)] == 18)

  outlet <- flines[which(flines$member_COMID == "5329303"), ]
  expect(outlet$LevelPathID == outlet$Hydroseq,
         "Levelpath and hydroseq of outlet should be the same.")

  mainstem_headwater <- flines[which(flines$member_COMID == "5329435"), ]
  expect(mainstem_headwater$Hydroseq > outlet$Hydroseq,
         "Hydroseq of headwater should be greater than outlet.")
  expect(mainstem_headwater$LevelPathID == outlet$LevelPathID,
         "Levelpath of outlet and headwater should be the same.")

  mainstem <- arrange(flines[flines$LevelPathID == outlet$LevelPathID, ], Hydroseq)
  expect(all(mainstem$toID[!is.na(mainstem$toID)] %in% mainstem$ID),
         "Expect the mainstem to be well connected.")

  expect(nrow(mainstem) == 18, "Mainstem has 18 COMIDs")
  expect(tail(mainstem$member_COMID, 1) == "5329435",
         "Expect this to be the headwater of the mainstem.")
  expect(head(mainstem$member_COMID, 1) == "5329303",
         "Expect this to be the outlet of the mainstem.")

  expect(length(unique(walker_flowline$LevelPathI)) == length(unique(flines$LevelPathID)),
         "Expect the same number of level paths in both input and output.")
})

test_that("collapse works on a double pass", {

  nhdplus_flines <- readRDS("data/oswego_network.rds")
  split_flines_meters <- 2000
  split_flines_cores <- 3
  collapse_flines_meters <- 500
  collapse_flines_main_meters <- 500

  if (suppressWarnings(require(lwgeom)) &
      exists("st_linesubstring",
             where = "package:lwgeom",
             mode = "function")) {

    flines <- suppressWarnings(
      sf::st_set_geometry(nhdplus_flines, NULL) %>%
        prepare_nhdplus(0, 0) %>%
        dplyr::inner_join(select(nhdplus_flines, COMID), by = "COMID") %>%
        sf::st_as_sf() %>%
        sf::st_cast("LINESTRING") %>%
        sf::st_transform(5070) %>%
        split_flowlines(split_flines_meters, split_flines_cores))

    collapsed_flines <-
      collapse_flowlines(sf::st_set_geometry(flines, NULL),
                         (0.25 * collapse_flines_meters / 1000),
                         TRUE,
                         (0.25 * collapse_flines_main_meters / 1000))

    collapsed_flines <-
      collapse_flowlines(collapsed_flines,
                         (0.5 * collapse_flines_meters / 1000),
                         TRUE,
                         (0.5 * collapse_flines_main_meters / 1000),
                         warn = FALSE)

    collapsed_flines <-
      collapse_flowlines(collapsed_flines,
                         (collapse_flines_meters / 1000),
                         TRUE,
                         (collapse_flines_main_meters / 1000),
                         warn = FALSE)

    # Old Tests:
    expect_equal(collapsed_flines$joined_toCOMID[
      which(collapsed_flines$COMID == "21975773")], "21975819.1")
    expect_equal(collapsed_flines$joined_toCOMID[
      which(collapsed_flines$COMID == "21976313")], "21975819.1")

    expect_equal(collapsed_flines$joined_fromCOMID[
      which(collapsed_flines$COMID == "21976891")],
                 collapsed_flines$joined_fromCOMID[
                   which(collapsed_flines$COMID == "21974583")])

    collapsed <- reconcile_collapsed_flowlines(collapsed_flines,
                                               select(flines, COMID),
                                               id = "COMID")

    # collapsed$member_COMID <-
    #   unlist(lapply(collapsed$member_COMID,
    #                 function(x) paste(x, collapse = ",")))
    # write_sf(collapsed, "flines.gpkg")

    # These tests are dumb but don't know how else to handle.
    # Checking neighborhood of: c(21976315, 21975773,
    # 21976313, 21975819.1) and 21975817 Could get real
    # verbose relative to comids
    expect(collapsed$toID[which(collapsed$ID == 59)] == 3031)
    expect(collapsed$toID[which(collapsed$ID == 5810)] == 59)
    expect(collapsed$toID[which(collapsed$ID == 58)] == 3028)
    expect(collapsed$toID[which(collapsed$ID == 3031)] == 58)

    outlet <- collapsed[which(collapsed$member_COMID == "21972746.2"), ]
    expect(outlet$LevelPathID == outlet$Hydroseq,
           "Levelpath and hydroseq of outlet should be the same.")

    mainstem_headwater <- collapsed[which(collapsed$member_COMID == "21983615.1"), ]
    expect(mainstem_headwater$Hydroseq > outlet$Hydroseq,
           "Hydroseq of headwater should be greater than outlet.")
    expect(mainstem_headwater$LevelPathID == outlet$LevelPathID,
           "Levelpath of outlet and headwater should be the same.")

    mainstem <- arrange(collapsed[collapsed$LevelPathID == outlet$LevelPathID, ], Hydroseq)
    expect(all(mainstem$toID[!is.na(mainstem$toID)] %in% mainstem$ID),
           "Expect the mainstem to be well connected.")

    expect(nrow(mainstem) == 166, "Mainstem has 166 COMIDs")
    expect(tail(mainstem$member_COMID, 1) == "21983615.1",
           "Expect this to be the headwater of the mainstem.")
    expect(head(mainstem$member_COMID, 1) == "21972746.2",
           "Expect this to be the outlet of the mainstem.")

    expect(length(unique(flines$LevelPathI)) == length(unique(collapsed$LevelPathID)),
           "Expect the same number of level paths in both input and output.")
  }
})
