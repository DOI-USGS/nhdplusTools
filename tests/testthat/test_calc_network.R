test_that("get_sorted error", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  expect_error(nhdplusTools::get_sorted(test_data))

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 0, 7, 8, 9, 4))

  expect_equal(nrow(nhdplusTools::get_sorted(test_data)), nrow(test_data))

  test_data <- data.frame(id = c(1, 2, 3, 4),
                          toid = c(2, 3, 4, 0))

  expect_equal(nrow(nhdplusTools::get_sorted(test_data)), nrow(test_data))
})

test_that("total drainage area works", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  catchment_area <- prepare_nhdplus(walker_flowline, 0, 0,
                                    purge_non_dendritic = FALSE, warn = FALSE)

  catchment_area <- select(walker_flowline, COMID, AreaSqKM) %>%
    left_join(catchment_area, by = "COMID") %>%
    select(ID = COMID, toID = toCOMID, area = AreaSqKM)

  suppressMessages(suppressWarnings(new_da <- calculate_total_drainage_area(catchment_area)))

  catchment_area$totda <- new_da
  catchment_area$nhdptotda <- walker_flowline$TotDASqKM

  expect_true(mean(abs(catchment_area$totda - catchment_area$nhdptotda)) < 1e-3)
  expect_true(max(abs(catchment_area$totda - catchment_area$nhdptotda)) < 1e-2)

  catchment_area$area[1] <- NA

})

test_that("arbolate sum works", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
  catchment_length <- prepare_nhdplus(walker_flowline, 0, 0,
                                      purge_non_dendritic = FALSE, warn = FALSE)

  catchment_length <- select(walker_flowline, COMID) %>%
    left_join(catchment_length, by = "COMID") %>%
    select(ID = COMID, toID = toCOMID, length = LENGTHKM)

  suppressMessages(suppressWarnings(arb_sum <- calculate_arbolate_sum(catchment_length)))

  catchment_length$arb_sum <- arb_sum
  catchment_length$nhd_arb_sum <- walker_flowline$ArbolateSu

  expect_true(mean(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum)) < 1e-3)
  expect_true(max(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum)) < 1e-2)
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
  expect_message(terminal <- get_terminal(fl, outlet))

  expect_equal(names(terminal), c("ID", "terminalID"))
  expect_true(is.numeric(terminal$ID))
  expect_equal(nrow(terminal), nrow(fl))

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  fl <- prepare_nhdplus(walker_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
    select(ID = COMID, toID = toCOMID)

  outlet <- fl$ID[which(!fl$toID %in% fl$ID)]
  fl$toID[which(!fl$toID %in% fl$ID)] <- 0

  expect_message(terminal <- get_terminal(fl, outlet))

  expect_equal(nrow(terminal), nrow(fl))
  expect_true(is.integer(terminal$ID))
})

test_that("get_terminal", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  fl <- prepare_nhdplus(walker_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
    select(ID = COMID, toID = toCOMID, length = LENGTHKM)

  suppressWarnings(pl <- get_pathlength(fl))

  expect_equal(nrow(fl), nrow(pl))

  pl <- left_join(select(walker_flowline,
                         COMID, Pathlength),
                  pl,
                  by = c("COMID" = "ID"))

  expect_equal(pl$pathlength, pl$Pathlength)
})
