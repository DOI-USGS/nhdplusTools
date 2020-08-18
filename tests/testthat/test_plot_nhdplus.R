context("plot tests")

sample_data <- system.file("extdata/sample_natseamless.gpkg",
                           package = "nhdplusTools")
options("rgdal_show_exportToProj4_warnings"="none")
test_that("basics work", {
  skip_on_cran()
  tempd <- tempdir(check = TRUE)

  site <- "USGS-05428500"
  g_temp <- file.path(tempd, "foo.gpkg")

  d <-  nhdplusTools:::get_plot_data(site, gpkg = g_temp, flowline_only = FALSE)
  expect_equal(names(d), c("plot_bbox", "outlets", "flowline", "basin", "catchment"))

  expect_true(all(c("comid", "type") %in% names(d$outlets)))
  l <- sf::st_layers(g_temp)
  expect_equal(l$name,
               c("CatchmentSP", "NHDFlowline_Network", "NHDArea", "NHDWaterbody"))
  expect_equal(l$features, c(433, 402, 1, 90))

  p_ready <- nhdplusTools:::gt(d$flowline)
  expect_equal(sf::st_crs(p_ready), sf::st_crs(3857))
  expect_s3_class(p_ready, "sfc_MULTILINESTRING")

  pdf(NULL)
  tempf <- file.path(tempd, "temp.png")

  png(file.path(tempd, "temp.png"))
  plot_nhdplus("USGS-05428500")
  dev.off()

  expect_true(file.exists(tempf))

  unlink(tempf)
  unlink(g_temp)

  png(file.path(tempd, "temp.png"))
  plot_nhdplus(list(list("comid", "13293970"),
                    list("nwissite", "USGS-05428500"),
                    list("huc12pp", "070900020603"),
                    list("huc12pp", "070900020602")),
               streamorder = 2,
               nhdplus_data = sample_data,
               gpkg = g_temp)
  dev.off()

  l <- sf::st_layers(g_temp)
  expect_equal(l$name,
               c("NHDFlowline_Network", "CatchmentSP", "NHDArea", "NHDWaterbody",
                 "Gage", "NHDFlowline_NonNetwork"))
  expect_equal(l$features, c(251, 250, 3, 117, 44, 48))

  expect_true(file.exists(tempf))
  unlink(tempf)
  unlink(g_temp)

  png(file.path(tempd, "temp.png"))
  plot_nhdplus(sf::st_as_sf(data.frame(x = -89.36083,
                                       y = 43.08944),
                            coords = c("x", "y"), crs = 4326),
               streamorder = 2,
               nhdplus_data = sample_data,
               gpkg = g_temp)
  dev.off()

  l <- sf::st_layers(g_temp)
  expect_equal(l$name,
               c("NHDFlowline_Network", "CatchmentSP", "NHDArea", "NHDWaterbody",
                 "Gage", "NHDFlowline_NonNetwork"))
  expect_equal(l$features, c(168, 167, 1, 90, 33, 45))

  expect_true(file.exists(tempf))
  unlink(tempf)

  png(file.path(tempd, "temp.png"))
  plot_nhdplus("USGS-05428500", streamorder = 3)
  dev.off()

  expect_true(file.exists(tempf))
  unlink(tempf)

})

test_that("local data", {

  testthat::skip_on_cran()

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

  expect_error(plot_nhdplus(outlets = outlet, nhdplus_data = "borked", gpkg = "borky"),
               "couldn't find nhdplus_data and output data not requested by the same path.")

  expect_error(plot_nhdplus(outlets = outlet, nhdplus_data = "borked", gpkg = "borked"),
               "output_file must end in '.gpkg'")

  plot_data_check <- plot_nhdplus(outlets = outlet, nhdplus_data = sample_data,
                                  actually_plot = FALSE)

  plot_data <- nhdplusTools:::get_plot_data(outlets = outlet, nhdplus_data = sample_data)

  expect_equal(names(plot_data), names(plot_data_check))

  expect_equal(names(plot_data), c("plot_bbox", "outlets", "flowline", "basin", "catchment"))
  expect_equal(nrow(plot_data$flowline), 251)
  expect_equal(plot_data$outlets$type, "comid")

  plot_data <- nhdplusTools:::get_plot_data(outlets = outlet, nhdplus_data = sample_data, flowline_only = TRUE)

  plot_data <- nhdplusTools:::get_plot_data(outlets = outlet, streamorder = 3, nhdplus_data = sample_data)
  expect_equal(nrow(plot_data$flowline), 57)
  expect_true(all(c("comid", "type") %in% names(plot_data$outlets)))

  skip_on_cran()
  outlet <- c(list(outlet), list(c("nwissite", "USGS-05428500")))
  plot_data <- nhdplusTools:::get_plot_data(outlets = outlet, streamorder = 3, nhdplus_data = sample_data)

  expect_true(all(names(plot_data$outlets) %in% c("comid", "geom", "type")))
  expect_equal(plot_data$outlets$comid, c("13293970", "13293750"))

  expect_s3_class(sf::st_geometry(plot_data$flowline)[[1]], "XY")

  plot_data <- nhdplusTools:::get_plot_data(outlets = c(outlet, "USGS-05428500"),
                                            streamorder = 3, nhdplus_data = sample_data)

  expect_equal(nrow(plot_data$outlets), 3)
  expect_equal(plot_data$outlets$type, c("comid", "nwissite", "nwissite"))

  # plot_nhdplus(outlets = outlet, nhdplus_data = sample_data)
  #
  # plot_nhdplus(outlets = outlet, nhdplus_data = sample_data, streamorder = 3)

  plot_data <- nhdplusTools:::get_plot_data(outlets = list("05427850",
                                              "05427718",
                                              c("comid", start),
                                              c("nwissite", "USGS-05428500"),
                                              c("huc12pp", "070900020603"),
                                              c("huc12pp", "070900020602")),
                               nhdplus_data = sample_data, streamorder = 3)

  expect_equal(nrow(plot_data$outlets), 6)

  # Also works with remote data.
  plot_data <- nhdplusTools:::get_plot_data(outlets = list("05427718",
                                                           "05427850",
                                                           c("comid", start),
                                                           c("nwissite", "USGS-05428500"),
                                                           c("huc12pp", "070900020603"),
                                                           c("huc12pp", "070900020602")))
  expect_equal(nrow(plot_data$outlets), 6)
  expect_true("sfc_POINT" %in% class(sf::st_geometry(plot_data$outlets)))

  plot_data <- nhdplusTools:::get_plot_data(sf::st_as_sf(data.frame(x = -89.36083,
                                                                    y = 43.08944),
                                                         coords = c("x", "y"), crs = 4326),
                                            streamorder = 2,
                                            nhdplus_data = sample_data)
  expect_equal(nrow(plot_data$outlets), 1)
})

test_that("test_as_outlets", {
  expect_equal(nhdplusTools:::as_outlets(NULL), NULL)

  o <- list(13293970, 13293971)

  expect_equal(nhdplusTools:::as_outlets(o),
               list(list(featureSource = "comid", featureID = "13293970"),
                    list(featureSource = "comid", featureID = "13293971")))

  o <- c(13293970, 13293971)

  expect_equal(nhdplusTools:::as_outlets(o),
               list(subset = c(13293970, 13293971)))

  o <- "USGS-05428500"

  expect_equal(nhdplusTools:::as_outlets(o), list(list(featureSource = "nwissite", featureID = "USGS-05428500")))

  o <- c("USGS-05428500", "USGS-05429500")

  expect_equal(nhdplusTools:::as_outlets(o), list(list(featureSource = "nwissite", featureID = "USGS-05428500"),
                                                  list(featureSource = "nwissite", featureID = "USGS-05429500")))

  o <- c("comid", 13293970)
  expect_equal(nhdplusTools:::as_outlets(o), list(list(featureSource = "comid", featureID = "13293970")))

  o <- list("comid", 13293970)

  expect_error(nhdplusTools:::as_outlets(o), regexp = "Error trying to interpret outlet specification.*Expected length 2 character vector or list with optional names: featureSource, featureID.*")

  o <- list("comid", "13293970")

  expect_equal(nhdplusTools:::as_outlets(o), list(list(featureSource = "comid", featureID = "13293970")))

  o <- c(list(o), list(c("nwissite", "USGS-05428500")))

  expect_equal(nhdplusTools:::as_outlets(o), list(list(featureSource = "comid",
                                        featureID = "13293970"),
                                   list(featureSource = "nwissite",
                                        featureID = "USGS-05428500")))

  o <- c(o,
            list(c("huc12pp", "070900020603")),
            list(c("huc12pp", "070900020602")))

  expect_equal(nhdplusTools:::as_outlets(o),
               list(list(featureSource = "comid", featureID = "13293970"),
                    list(featureSource = "nwissite", featureID = "USGS-05428500"),
                    list(featureSource = "huc12pp", featureID = "070900020603"),
                    list(featureSource = "huc12pp", featureID = "070900020602")))

  o <- c("05427718", "05427850")
  expect_equal(nhdplusTools:::as_outlets(o), list(list(featureSource = "nwissite", featureID = "USGS-05427718"),
                                                  list(featureSource = "nwissite", featureID = "USGS-05427850")))

  o <- list(c("comid", "13293970"), c("nwissite", "USGS-05428500"))

  expect_equal(nhdplusTools:::as_outlets(o),
               list(list(featureSource = "comid", featureID = "13293970"),
                    list(featureSource = "nwissite", featureID = "USGS-05428500")))

  skip_on_cran()
  o <- sf::st_as_sf(data.frame(x = -122.765037511658,
                               y = 45.6534111629304),
                    coords = c("x", "y"), crs = 4326)
  expect_equal(nhdplusTools:::as_outlets(o), list(list(featureSource = "comid", featureID = "23735691")))

})

test_that("test_styles", {
  st <- nhdplusTools:::get_styles(NULL)
  expect_named(st, c("basin", "flowline", "outlets"))
  expect_named(st$outlets, c("default", "nwissite", "huc12pp", "wqp"))
  expect_named(st$outlets$nwissite, c("col", "bg", "pch", "cex"))

  st <- nhdplusTools:::get_styles(list(basin = c(lwd = 2),
                                       flowline = c(col = "test"),
                                       outlets = list(nwissite = list(pch = "."),
                                                      test = list(pch = 27, cex = 2))))

  expect_equal(st$basin$lwd, 2)
  expect_equal(st$flowline$col, "test")
  expect_equal(st$outlets$test, list(col = "black", bg = NA, pch = 27, cex = 2))
  expect_equal(st$outlets$nwissite$pch, ".")

  expect_error(nhdplusTools:::get_styles(list(basin = c(lwc = 2))),
               'Expected one ore more of "lwd", "col", or "border" in basins plot_config, got:lwc')

  expect_error(nhdplusTools:::get_styles(list(lowline = c(bol = "test"))),
               'Expected one or more of "basin", "flowline", or "outlets" in plot_config, got: lowline')

  expect_error(nhdplusTools:::get_styles(list(flowline = c(bol = "test"))),
               'Expected one ore more of "lwd" and "col" in flowlines plot_config, got:bol')

  expect_error(nhdplusTools:::get_styles(list(outlets = list(nwissite = list(dch = "."),
                                                             test = list(pch = 27, pex = 2)))),
               'Expected one or more of "col", "bg", "pch", or "cex" in outlets plot_config, got: dch, pch, pex')
})

test_that("bbox", {
  skip_on_cran()

   bbox <- sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
                         xmax = -89.24681, ymax = 43.17192),
                       crs = "+proj=longlat +datum=WGS84 +no_defs")

   # With downloaded data
   d <- nhdplusTools:::get_plot_data(bbox = bbox)

   expect_equal(nrow(d$flowline), 183)

   # With Local Data (note this sanple is already subset to a watershed basis)
   d <- nhdplusTools:::get_plot_data(bbox = bbox, streamorder = 2,
                                     nhdplus_data = sample_data)

  expect_equal(nrow(d$flowline), 76)

  expect_error(nhdplusTools:::get_plot_data(c(1,2,3), bbox = bbox),
               "Both bbox and outlets not supported.")
})

test_that("comids", {
  fline <- sf::read_sf(sample_data, "NHDFlowline_Network")
  comids <- nhdplusTools::get_UT(fline, 13293970)
  d <- nhdplusTools:::get_plot_data(comids)

  expect_equal(names(d), c("plot_bbox", "outlets", "flowline", "basin", "catchment"))
  expect_true(all(d$flowline$comid %in% comids))
  expect_equal(d$catchment, NULL)

  d <- nhdplusTools:::get_plot_data(comids, flowline_only = FALSE)
  expect_true(is(d$catchment, "sf"))
  expect_true(is(d$basin, "sf"))
})

