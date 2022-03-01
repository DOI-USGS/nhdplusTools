
source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

test_that("osm_cache_dir", {
  testthat::skip_on_cran()
  dir <- nhdplusTools:::osm_cache_dir()

  expect_equal(dir,
               file.path(nhdplusTools_data_dir(),
                         "osm.cache"))

  temp_test <- file.path(tempdir(check = TRUE), "temptest.txt")

  cat("temp", file = temp_test)

  orig_dir <- nhdplusTools_data_dir()

  nhdplusTools_data_dir(temp_test)

  dir <- nhdplusTools:::osm_cache_dir()

  expect_equal(dir, file.path(tempdir(check = TRUE), "osm.cache"))

  nhdplusTools_data_dir(orig_dir)
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
  expect_named(st, c("basin", "flowline", "network_wtbd",
                     "off_network_wtbd","outlets"))
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
               'Expected one or more of "basin", "flowline", "outlets", or "waterbody" in plot_config, got: lowline')

  expect_error(nhdplusTools:::get_styles(list(flowline = c(bol = "test"))),
               'Expected one ore more of "lwd" and "col" in flowlines plot_config, got:bol')

  expect_error(nhdplusTools:::get_styles(list(outlets = list(nwissite = list(dch = "."),
                                                             test = list(pch = 27, pex = 2)))),
               'Expected one or more of "col", "bg", "pch", or "cex" in outlets plot_config, got: dch, pch, pex')
})

test_that("comids", {
  Sys.setenv(MAKE_BASIN="FALSE")

  testthat::skip_on_cran()
  fline <- sf::read_sf(sample_data, "NHDFlowline_Network")
  comids <- nhdplusTools::get_UT(fline, 13293970)
  d <- nhdplusTools:::plot_nhdplus(comids, flowline_only = TRUE,
                                   nhdplus_data = sample_data, cache_data = FALSE)

  expect_equal(names(d), c("plot_bbox", "outlets", "flowline", "basin", "catchment",
                           "network_wtbd","off_network_wtbd"))
  expect_true(all(d$flowline$COMID %in% comids))
  expect_equal(d$catchment, NULL)

  d <- nhdplusTools:::get_plot_data(comids, flowline_only = FALSE, nhdplus_data = sample_data)
  expect_true(is(d$catchment, "sf"))
  # expect_true(is(d$basin, "sf"))

  b <- nhdplusTools:::make_basin(d, "catchment", comids)

  expect_null(b)

  Sys.unsetenv("MAKE_BASIN")

  b <- nhdplusTools:::make_basin(d, "catchment", comids[1:10])

  expect_s3_class(b, "sf")

  Sys.setenv(MAKE_BASIN="FALSE")
})

test_that("waterbodies", {
  testthat::skip_on_cran()
  site <- "USGS-05428500"
  tempd <- tempdir(check = TRUE)
  g_temp <- file.path(tempd, "foo.gpkg")

  d <-  nhdplusTools:::plot_nhdplus(site, cache_data = FALSE)

  expect_equal(names(d), c("plot_bbox", "outlets", "flowline",
                           "basin", "catchment","network_wtbd",
                           "off_network_wtbd"))

  d <-  nhdplusTools:::plot_nhdplus(site, flowline_only = FALSE,
                                    cache_data = FALSE)

  expect_true(is(d$network_wtbd, "sf"))
  expect_true(is(d$off_network_wtbd, "sf"))

  bbox <- sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
                        xmax = -89.24681, ymax = 43.17192),
                      crs = "+proj=longlat +datum=WGS84 +no_defs")

  # With downloaded data
  d <- nhdplusTools:::get_plot_data(bbox = bbox, flowline_only = FALSE)

  expect_equal(nrow(d$off_network_wtbd), 43)
  expect_equal(nrow(d$network_wtbd), 10)

  # With Local Data (note this sample is already subset to a watershed basis)
  d <- nhdplusTools:::get_plot_data(bbox = bbox, streamorder = 2,
                                    nhdplus_data = sample_data)

  expect_equal(nrow(d$off_network_wtbd), 43)
  expect_equal(nrow(d$network_wtbd), 10)
})

test_that("get_waterbody_outlet", {
  testthat::skip_on_cran()
  lake_comid <- 13293262
  site <- "USGS-05428500"
  tempd <- tempdir(check = TRUE)
  g_temp <- file.path(tempd, "foo.gpkg")

  d <-  nhdplusTools:::get_plot_data(site, gpkg = g_temp, flowline_only = FALSE)
  out <-  nhdplusTools:::get_wb_outlet(lake_comid, d$flowline)

  expect_equal(out$comid, 13294312)
  expect_equal(out$gnis_name, "Yahara River")
  expect_true(is(out, "sf"))

  expect_equal(nrow(out), 1)

  lake_comid <- 14711354
  expect_error(nhdplusTools:::get_wb_outlet(lake_comid, d$flowline),
               "Lake COMID is not associated with NHDPlus flowlines and no outlet")
})
