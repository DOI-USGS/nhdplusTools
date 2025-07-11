test_that("basic two flowline network", {

  net_new <- structure(list(comid = c(6170348, 6170858),
                            tocomid = c(0, 6170348),
                            gnis_id = c(" ", " "),
                            areasqkm = c(0.1089, 0),
                            lengthkm = c(0.224, 0.152)),
                       class = "data.frame",
                       row.names = c(NA, -2L))

  net_new <-
    add_plus_network_attributes(
      dplyr::rename(net_new, nameID = gnis_id), status = FALSE)


  expect_equal(nrow(net_new), 2)
})

test_that("example", {
  skip_on_cran()
  skip_on_ci()

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    comid = test_flowline$COMID,
    tocomid = test_flowline$toCOMID,
    nameID = walker_flowline$GNIS_ID,
    lengthkm = test_flowline$LENGTHKM,
    areasqkm = walker_flowline$AreaSqKM)

  expect_s3_class(get_sorted(sf::st_sf(test_flowline, sf::st_geometry(walker_flowline))), "sf")

  mess <- capture_output(fl <- add_plus_network_attributes(test_flowline,
                                                           status = TRUE)
  )

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
