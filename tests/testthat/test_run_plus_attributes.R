

test_that("example", {
  skip_on_cran()

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  expect_s3_class(get_sorted(walker_flowline), "sf")

  test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    comid = test_flowline$COMID,
    tocomid = test_flowline$toCOMID,
    nameID = walker_flowline$GNIS_ID,
    lengthkm = test_flowline$LENGTHKM,
    areasqkm = walker_flowline$AreaSqKM)

  mess <- capture_output(fl <- add_plus_network_attributes(test_flowline,
                                                           status = TRUE))

  expect_true(grepl("+| 100% elapsed=", mess))

  # make sure these are all present and not na.
  expect_true(!any(is.na(c(fl$tocomid, fl$terminalfl,
                           fl$hydroseq, fl$levelpathi,
                           fl$pathlength, fl$dnlevelpat,
                           fl$dnhydroseq, fl$totdasqkm))))

  expect_equal(length(unique(fl$levelpathi)), length(unique(walker_flowline$LevelPathI)))

  expect_equal(unique(fl$terminalpa), min(fl$hydroseq))

  fl2 <- add_plus_network_attributes(test_flowline, cores = 2,
                                     split_temp = (tempf <- tempfile(fileext = ".rds")),
                                     status = FALSE)

  expect_equal(fl, fl2, ignore_attr = TRUE)

  expect_true(file.exists(tempf))

  expect_type(readRDS(tempf), "list")

  unlink(tempf)
})

test_that("coastal", {
  net <- read.csv(list.files(pattern = "coastal_net.csv", full.names = TRUE, recursive = TRUE))

  net <- net[order(net$hydroseq), ]

  sorted <- get_sorted(net[, c("comid", "tocomid")], split = TRUE)

  expect_true(which(sorted$comid == 2544325) <
                which(sorted$comid == 2544321))

  expect_equal(length(unique(sorted$terminalID)), 32)

  sorted <- get_sorted(net[, c("comid", "tocomid")], split = TRUE, outlets = 2544457)

  expect_true(all(sorted$terminalID == 2544457))

})
