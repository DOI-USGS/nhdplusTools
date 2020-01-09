context("calculate network attributes")

test_that("total drainage area works", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  catchment_area <- prepare_nhdplus(walker_flowline, 0, 0,
                                    purge_non_dendritic = FALSE, warn = FALSE) %>%
    left_join(select(walker_flowline, COMID, AreaSqKM), by = "COMID") %>%
    select(ID = COMID, toID = toCOMID, area = AreaSqKM)

  new_da <- calculate_total_drainage_area(catchment_area)

  catchment_area$totda <- new_da
  catchment_area$nhdptotda <- walker_flowline$TotDASqKM

  expect(mean(abs(catchment_area$totda - catchment_area$nhdptotda)) < 1e-3, "drainage area not close enough")
  expect(max(abs(catchment_area$totda - catchment_area$nhdptotda)) < 1e-2, "drainage area not close enough")
})

test_that("arbolate sum works", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
  catchment_length <- prepare_nhdplus(walker_flowline, 0, 0,
                                      purge_non_dendritic = FALSE, warn = FALSE) %>%
    left_join(select(walker_flowline, COMID), by = "COMID") %>%
    select(ID = COMID, toID = toCOMID, length = LENGTHKM)

  arb_sum <- calculate_arbolate_sum(catchment_length)

  catchment_length$arb_sum <- arb_sum
  catchment_length$nhd_arb_sum <- walker_flowline$ArbolateSu

  expect(mean(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum)) < 1e-3, "arbolate sum not close enough")
  expect(max(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum)) < 1e-2, "arbolate sum not close enough")
})

test_that("calculate level path", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    ID = test_flowline$COMID,
    toID = test_flowline$toCOMID,
    nameID = walker_flowline$GNIS_ID,
    weight = walker_flowline$ArbolateSu,
    stringsAsFactors = FALSE)

  test_flowline <- left_join(test_flowline,
                             get_levelpaths(test_flowline, status = TRUE), by = "ID")

  nhdp_lp <- sort(unique(walker_flowline$LevelPathI))
  nhdt_lp <- sort(unique(test_flowline$levelpath))

  expect_true(length(nhdp_lp) == length(nhdt_lp))

  for(lp in seq_along(nhdp_lp)) {
    nhdp <- filter(walker_flowline, LevelPathI == nhdp_lp[lp])
    outlet_comid <- filter(nhdp, Hydroseq == min(Hydroseq))$COMID
    nhdt <- filter(test_flowline, outletID == outlet_comid)
    expect(all(nhdp$COMID %in% nhdt$ID), paste("Mismatch in", nhdp_lp[lp],
                                               "level path from NHDPlus."))
  }
})

test_that("calculate level path", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    ID = test_flowline$COMID,
    toID = test_flowline$toCOMID)

  test_flowline$order <- get_streamorder(test_flowline)

  walker_flowline <- left_join(walker_flowline, test_flowline, by = c("COMID" = "ID"))

  expect_equal(walker_flowline$order, walker_flowline$StreamOrde)

  pt_data <- sf::read_sf(system.file("extdata/petapsco_flowlines.gpkg",
                                     package = "nhdplusTools"))

  test_flowline <- prepare_nhdplus(pt_data, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    ID = test_flowline$COMID,
    toID = test_flowline$toCOMID)

  test_flowline$order <- get_streamorder(test_flowline)

  pt_data <- left_join(filter(pt_data, COMID %in% test_flowline$ID),
                       test_flowline, by = c("COMID" = "ID"))

  expect_equal(pt_data$order, pt_data$StreamOrde)
})

test_that("get_pfaf", {
  suppressMessages(
  source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools")))
  hr_flowline <- align_nhdplus_names(hr_data$NHDFlowline)

  suppressWarnings(
  fl <- prepare_nhdplus(hr_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
    left_join(select(hr_flowline, COMID, AreaSqKM), by = "COMID") %>%
    st_sf() %>%
    select(ID = COMID, toID = toCOMID, area = AreaSqKM))

  fl$nameID = ""
  fl$totda <- calculate_total_drainage_area(sf::st_set_geometry(fl, NULL))
  fl <- left_join(fl, get_levelpaths(dplyr::rename(sf::st_set_geometry(fl, NULL),
                                                   weight = totda)), by = "ID")

  pfaf <- get_pfaf(fl, max_level = 2)

  expect_equal(pfaf[pfaf$ID == 15000500028335,	], dplyr::tibble(ID = 15000500028335,
                                                         pf_level_1 = 5, pf_level_2 = 51))

  pfaf <- get_pfaf(fl, max_level = 4)

  expect_true(all(!is.na(c(pfaf$pf_level_1, pfaf$pf_level_4))))

  fl <- left_join(fl, pfaf, by = "ID")

  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500061836], 611)

  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500028338], 591)
  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500050711], 592)
  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500028337], 593)

  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500072804], 151)
  expect_equal(pfaf$pf_level_4[pfaf$ID == 15000500072804], 1511)

  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500084318], 161)
  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500028332], 181)

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  fl <- prepare_nhdplus(walker_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
    left_join(select(walker_flowline, COMID, AreaSqKM), by = "COMID") %>%
    st_sf() %>%
    select(ID = COMID, toID = toCOMID, area = AreaSqKM)

  fl$nameID = ""
  fl$totda <- calculate_total_drainage_area(sf::st_set_geometry(fl, NULL))
  fl <- left_join(fl, get_levelpaths(dplyr::rename(sf::st_set_geometry(fl, NULL),
                                                   weight = totda)), by = "ID")

  pfaf <- get_pfaf(fl, max_level = 2)

  expect_equal(nrow(pfaf), 57)
})

test_that("get_terminal", {
  suppressMessages(
    source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools")))
  hr_flowline <- align_nhdplus_names(hr_data$NHDFlowline)

  suppressWarnings(
    fl <- prepare_nhdplus(hr_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
      select(ID = COMID, toID = toCOMID))

  outlet <- fl$ID[which(!fl$toID %in% fl$ID)]
  fl$toID[which(!fl$toID %in% fl$ID)] <- 0
  terminal <- get_terminal(fl, outlet)

  expect_equal(names(terminal), c("terminalID", "ID"))
  expect_true(is.numeric(terminal$ID))
  expect_equal(nrow(terminal), nrow(fl))

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  fl <- prepare_nhdplus(walker_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
    select(ID = COMID, toID = toCOMID)

  outlet <- fl$ID[which(!fl$toID %in% fl$ID)]
  fl$toID[which(!fl$toID %in% fl$ID)] <- 0

  terminal <- get_terminal(fl, outlet)

  expect_equal(nrow(terminal), nrow(fl))
  expect_true(is.integer(terminal$ID))
})

test_that("get_terminal", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  fl <- prepare_nhdplus(walker_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
    select(ID = COMID, toID = toCOMID, length = LENGTHKM)

  suppressWarnings(pl <- get_pathlength(fl))

  expect_equal(nrow(fl), nrow(pl))

  pl <- left_join(pl, select(walker_flowline,
                             COMID, Pathlength),
                  by = c("ID" = "COMID"))

  expect_equal(pl$pathlength, pl$Pathlength)
})



