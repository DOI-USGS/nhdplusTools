source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

test_that("get streamorder", {
  skip_on_cran()

    test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE, warn = FALSE)

    test_flowline <- data.frame(
      ID = test_flowline$COMID,
      toID = test_flowline$toCOMID)

    test_flowline$order <- get_streamorder(test_flowline)

    walker_flowline <- dplyr::left_join(walker_flowline, test_flowline, by = c("COMID" = "ID"))

    expect_equal(walker_flowline$order, walker_flowline$StreamOrde)

    if(Sys.getenv("_R_CHECK_ON_GH_") != "true") { # broken on github actions

    source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

    pt_data <- sample_flines

    test_flowline <- prepare_nhdplus(pt_data, 0, 0, FALSE, warn = FALSE)

    test_flowline <- data.frame(
      ID = test_flowline$COMID,
      toID = test_flowline$toCOMID)

    test_flowline$order <- get_streamorder(test_flowline)

    pt_data <- dplyr::left_join(dplyr::filter(pt_data, COMID %in% test_flowline$ID),
                         test_flowline, by = c("COMID" = "ID"))

    expect_equal(pt_data$order, pt_data$StreamOrde)

  }
})

test_that("get_streamlevel", {

    test_flowline <- data.frame(
      levelpathi = walker_flowline$LevelPathI,
      dnlevelpat = walker_flowline$DnLevelPat)

    test_flowline$dnlevelpat[1] <- 0

    expect_equal(walker_flowline$StreamLeve, get_streamlevel(test_flowline))

    test_flowline$coastal <- rep(FALSE, nrow(test_flowline))
    expect_equal(walker_flowline$StreamLeve + 3, get_streamlevel(test_flowline))

    test_flowline$coastal[!test_flowline$dnlevelpat %in% test_flowline$levelpathi] <- TRUE
    expect_equal(walker_flowline$StreamLeve, get_streamlevel(test_flowline))

})

test_that("get_pfaf", {


  # layers <- "NHDFlowline"
  #
  # suppressMessages(
  #   source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools")))
  #
  # hr_flowline <- align_nhdplus_names(hr_data$NHDFlowline)
  #
  # suppressWarnings(
  #   fl <- prepare_nhdplus(hr_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE))
  #
  # fl <- select(hr_flowline, COMID, AreaSqKM) %>%
  #   left_join(fl, by = "COMID") %>%
  #   st_sf() %>%
  #   select(ID = COMID, toID = toCOMID, area = AreaSqKM)
  #
  # fl$nameID = " "
  # fl$totda <- calculate_total_drainage_area(sf::st_set_geometry(fl, NULL))
  # fl <- left_join(fl, get_levelpaths(dplyr::rename(sf::st_set_geometry(fl, NULL),
  #                                                  weight = totda)), by = "ID")
  #
  # saveRDS(sf::st_drop_geometry(fl), "tests/testthat/data/pfaf_net.rds")

  fl <- readRDS(list.files(pattern = "pfaf_net.rds", recursive = TRUE, full.names = TRUE))

  expect_warning(
  pfaf <- get_pfaf(fl, max_level = 2))

  expect_equal(pfaf[pfaf$ID == 15000500028335,	], dplyr::tibble(ID = 15000500028335,
                                                                pf_level_1 = 5, pf_level_2 = 51))
  expect_warning(
  pfaf <- get_pfaf(fl, max_level = 4))

  expect_equal(sum(!is.na(c(pfaf$pf_level_1, pfaf$pf_level_4))), 4496)

  fl <- dplyr::left_join(fl, pfaf, by = "ID")

  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500061836], 611)

  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500028338], 591)
  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500050711], 592)
  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500028337], 593)

  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500072804], 151)
  expect_equal(pfaf$pf_level_4[pfaf$ID == 15000500072804], 1511)

  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500084318], 161)
  expect_equal(pfaf$pf_level_3[pfaf$ID == 15000500028332], 181)

  fl <- prepare_nhdplus(walker_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE)

  fl <- dplyr::select(walker_flowline, COMID, AreaSqKM) %>%
    dplyr::left_join(fl, by = "COMID") %>%
    sf::st_sf() %>%
    dplyr::select(ID = COMID, toID = toCOMID, area = AreaSqKM)

  fl$nameID = ""
  suppressMessages(suppressWarnings(
  fl$totda <- calculate_total_drainage_area(sf::st_set_geometry(fl, NULL))
  ))

  expect_warning(
  fl <- dplyr::left_join(fl, get_levelpaths(dplyr::rename(sf::st_set_geometry(fl, NULL),
                                                   weight = totda)), by = "ID")
  )

  expect_warning(
  pfaf <- get_pfaf(fl, max_level = 2))

  expect_equal(nrow(pfaf), 57)
})
