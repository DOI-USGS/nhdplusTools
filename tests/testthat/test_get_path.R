context("levelpath")

test_that("reweight", {
  x <- readRDS(list.files(pattern = "reweight_test.rds",
                          full.names = TRUE, recursive = TRUE))
  w <- nhdplusTools:::reweight(x, override_factor = 5)
  expect_equal(w$weight[w$nameID == w$ds_nameID], 2)

  w <- nhdplusTools:::reweight(x, override_factor = 2)
  expect_equal(w$weight[w$nameID == w$ds_nameID], 1)
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

  test_flowline_out <- left_join(test_flowline,
                                 get_levelpaths(test_flowline, status = TRUE), by = "ID")

  nhdp_lp <- sort(unique(walker_flowline$LevelPathI))
  nhdt_lp <- sort(unique(test_flowline_out$levelpath))

  expect_true(length(nhdp_lp) == length(nhdt_lp))

  for(lp in seq_along(nhdp_lp)) {
    nhdp <- filter(walker_flowline, LevelPathI == nhdp_lp[lp])
    outlet_comid <- filter(nhdp, Hydroseq == min(Hydroseq))$COMID
    nhdt <- filter(test_flowline_out, outletID == outlet_comid)
    expect(all(nhdp$COMID %in% nhdt$ID), paste("Mismatch in", nhdp_lp[lp],
                                               "level path from NHDPlus."))
  }

  # break the data
  test_flowline$nameID[test_flowline$ID == 5329293] <- " "
  test_flowline$nameID[test_flowline$ID == 5329295] <- "255208"
  test_flowline_out2 <- left_join(test_flowline,
                                  get_levelpaths(test_flowline, status = TRUE), by = "ID")

  expect_equal(test_flowline_out2$levelpath[test_flowline_out2$ID == 5329295], 1)

  test_flowline_out2 <- left_join(test_flowline,
                                  get_levelpaths(test_flowline, override_factor = 10,
                                                 status = TRUE), by = "ID")

  expect_equal(test_flowline_out$levelpath, test_flowline_out2$levelpath)
})

test_that("calculate level path", {
  skip_on_cran()
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    ID = test_flowline$COMID,
    toID = test_flowline$toCOMID)

  test_flowline$order <- get_streamorder(test_flowline)

  walker_flowline <- left_join(walker_flowline, test_flowline, by = c("COMID" = "ID"))

  expect_equal(walker_flowline$order, walker_flowline$StreamOrde)

  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  pt_data <- sample_flines

  test_flowline <- prepare_nhdplus(pt_data, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    ID = test_flowline$COMID,
    toID = test_flowline$toCOMID)

  test_flowline$order <- get_streamorder(test_flowline)

  pt_data <- left_join(filter(pt_data, COMID %in% test_flowline$ID),
                       test_flowline, by = c("COMID" = "ID"))

  expect_equal(pt_data$order, pt_data$StreamOrde)
})

test_that("hr levelpath", {

  suppressMessages(
    source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools")))
  hr_flowline <- align_nhdplus_names(hr_data$NHDFlowline)

  suppressWarnings(
    fl <- prepare_nhdplus(hr_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE))

  fl <- select(hr_flowline, COMID, ArbolateSu, GNIS_Name) %>%
    left_join(fl, by = "COMID") %>%
    st_sf() %>%
    select(ID = COMID, toID = toCOMID, weight = ArbolateSu, nameID = GNIS_Name)

  lp <- get_levelpaths(sf::st_set_geometry(fl, NULL), cores = 2)

  cores <- parallel::makeCluster(2)

  lp <- get_levelpaths(sf::st_set_geometry(fl, NULL), cores = cores)

  expect_error(get_levelpaths(sf::st_set_geometry(fl, NULL), cores = "char"))

  # Same number of total flowlines
  expect_equal(length(unique(hr_flowline$LevelPathI)), length(unique(lp$levelpath)))

  # follows a semi tricky mainstem the same as HI
  expect_equal(lp[lp$ID == 15000500039693, ]$levelpath, lp[lp$ID == 15000500039696, ]$levelpath)

})

