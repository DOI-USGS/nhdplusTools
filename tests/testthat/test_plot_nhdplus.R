context("plot tests")

test_that("basics work", {
  skip_on_cran()
  site <- "USGS-05428500"
  d <-  nhdplusTools:::get_plot_data(site)
  expect_equal(names(d), c("plot_bbox", "outlets", "flowline", "basin", "catchment"))

  p_ready <- nhdplusTools:::gt(d$flowline)
  expect_equal(sf::st_crs(p_ready), sf::st_crs(3857))
  expect_s3_class(p_ready, "sfc_LINESTRING")

  pdf(NULL)
  tempd <- tempdir()
  dir.create(tempd, recursive = TRUE)
  tempf <- file.path(tempd, "temp.png")
  png(file.path(tempd, "temp.png"))
  plot_nhdplus("USGS-05428500")
  dev.off()

  expect_true(file.exists(tempf))

  expect_error(plot_nhdplus("USGS-05428500", streamorder = 3),
               "Streamoder not available without specifying nhdplus_data source. Can't filter.")
})

test_that("local data", {

  sample_data <- system.file("extdata/sample_natseamless.gpkg",
                             package = "nhdplusTools")

  fline <- sf::read_sf(sample_data, "NHDFlowline_Network")
  gage <- sf::read_sf(sample_data, "Gage")

  # site_data <- dataRetrieval::whatNWISdata(siteNumber = gage$SOURCE_FEA,
  #                                          parameterCd = c("00060"), statCd = c("00003"))
  #
  # site <- filter(site_data, stat_cd == "00003") %>%
  #   filter(count_nu == max(count_nu)) %>%
  #   sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = sf::st_crs(4326))
  #
  # start <- nhdplusTools::get_flowline_index(fline, sf::st_transform(site, sf::st_crs(fline)))$COMID
  # or in this case they are the same
  start <- gage$FLComID[which(gage$SOURCE_FEA == "05429500")]

  outlet <- c("comid", start)

  plot_data <- nhdplusTools:::get_plot_data(outlets = outlet, nhdplus_data = sample_data)

  expect_equal(names(plot_data), c("plot_bbox", "outlets", "flowline", "basin", "catchment"))
  expect_equal(nrow(plot_data$flowline), 251)

  plot_data <- nhdplusTools:::get_plot_data(outlets = outlet, streamorder = 3, nhdplus_data = sample_data)
  expect_equal(nrow(plot_data$flowline), 57)
  expect_true("comid" %in% names(plot_data$outlets))

  skip_on_cran()
  outlet <- c(list(outlet), list(c("nwissite", "USGS-05428500")))
  plot_data <- nhdplusTools:::get_plot_data(outlets = outlet, streamorder = 3, nhdplus_data = sample_data)

  expect_equal(names(plot_data$outlets), c("comid", "geom"))
  expect_equal(plot_data$outlets$comid, c("13293970", "13293750"))

  expect_s3_class(sf::st_geometry(plot_data$flowline)[[1]], "XY")

  plot_data <- nhdplusTools:::get_plot_data("USGS-05429500", outlets = outlet, streamorder = 3, nhdplus_data = sample_data)

  expect_equal(nrow(plot_data$outlets), 3)

  # plot_nhdplus(outlets = outlet, nhdplus_data = sample_data)
  #
  # plot_nhdplus(outlets = outlet, nhdplus_data = sample_data, streamorder = 3)

  plot_data <- nhdplusTools:::get_plot_data(nwissite = c("05427718", "05427850"),
                               outlets = list(c("comid", start),
                                              c("nwissite", "USGS-05428500"),
                                              c("huc12pp", "070900020603"),
                                              c("huc12pp", "070900020602")),
                               nhdplus_data = sample_data, streamorder = 3)

  expect_equal(nrow(plot_data$outlets), 6)

  # Also works with remote data.
  plot_data <- nhdplusTools:::get_plot_data(nwissite = c("05427718", "05427850"),
                                            outlets = list(c("comid", start),
                                                           c("nwissite", "USGS-05428500"),
                                                           c("huc12pp", "070900020603"),
                                                           c("huc12pp", "070900020602")))
  expect_equal(nrow(plot_data$outlets), 6)
  expect_true("sfc_POINT" %in% class(sf::st_geometry(plot_data$outlets)))
})
