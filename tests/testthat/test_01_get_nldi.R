

test_that("nldi basics work", {

  skip_on_cran()

  nldi_sources <- dataRetrieval::get_nldi_sources()

  expect_equal(class(nldi_sources), "data.frame")

  expect_true(all(c("comid", "huc12pp", "nwissite") %in% nldi_sources$source))

  expect_equal(names(nldi_sources), c("source", "sourceName", "features"))

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

  expect_error(nhdplusTools:::check_nldi_feature(nldi_nwis[1]),
                 "Missing some required input for NLDI. Expected length 2 character vector or list with optional names: featureID")

  expect_equal(nrow(get_nldi_index(c(-89.276, 42.988))), 2)
})

test_that("navigation works", {

  skip_on_cran()

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

  nav <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "UM",
                       data_source = "nwissite",
                       distance_km = 1)

  expect("sf" %in% class(nav$UM_nwissite), "expected an sf data.frame")

  expect_true("sfc_POINT" %in% class(sf::st_geometry(nav$UM_nwissite)),
         "expected point response")

  nav2 <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "upstreamMain",
                       data_source = "nwissite",
                       distance_km = 100)

  expect_true(nrow(nav2$UM_nwissite) > nrow(nav$UM_nwissite))

  nldi_nwis <- as.character(nldi_nwis)

  nav3 <- navigate_nldi(nldi_feature = nldi_nwis,
                        mode = "upstreamMain",
                        data_source = "flowlines",
                        distance_km = 10)

  expect_s3_class(sf::st_geometry(nav3$UM), "sfc_LINESTRING")

  expect_warning(nav3 <- navigate_nldi(nldi_feature = nldi_nwis,
                                       mode = "upstreamMain",
                                       data_source = "flowline",
                                       distance_km = 10),
                 "data source specified as flowline or '' is deprecated")

  nav <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-08279500/navigation/UM",
                       data_source = "dumb",
                       distance_km = 1)

  expect_equal(nav$sourceName, "NWIS Surface Water Sites")

  expect_equal(class(nav$comid), "character")

  nav <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-08279500/navigation/UM",
                       data_source = "nwissite",
                       distance_km = 1)

  expect("sf" %in% sapply(nav, class), "expected an sf data.frame")

  # expect_equal(navigate_nldi(list(featureSource = "wqp",
  #                                 featureID = "TCEQMAIN-16638"),
  #                            mode = "upstreamMain",
  #                            data_source = "nwissite"), dplyr::tibble())
})

test_that("basin works", {

  skip_on_cran()

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05428500")

  site <- get_nldi_feature(nldi_nwis)

  nav <- get_nldi_basin(nldi_feature = nldi_nwis)

  expect("sf" %in% class(nav), "expected an sf data.frame")

  expect_true("sfc_POLYGON" %in% class(sf::st_geometry(nav)),
         "expected polygon response")

  basin2 <- get_nldi_basin(nldi_feature = nldi_nwis,
                           simplify = FALSE, split = TRUE)

  expect_true(length(sf::st_coordinates(nav)) < length(sf::st_coordinates(basin2)))

  expect_true(!sf::st_crosses(sf::st_cast(nav, "LINESTRING"),
                              st_buffer(site, units::set_units(50, "m")),
                                 sparse = FALSE))

  expect_true(sf::st_crosses(sf::st_cast(basin2, "LINESTRING"),
                             st_buffer(site, units::set_units(50, "m")),
                             sparse = FALSE))
})

test_that("get feature works", {
  skip_on_cran()

  f <- get_nldi_feature(list(featureSource = "nwissite", featureID = "USGS-05428500"))

  expect_equal(nrow(f), 1)
  expect_equal(ncol(f), 9)
  expect_equal(f$identifier, "USGS-05428500")

  f <- get_nldi_feature(list("nwissite", "USGS-05428500"))

  expect_equal(nrow(f), 1)
  expect_equal(ncol(f), 9)
  expect_equal(f$identifier, "USGS-05428500")

})

test_that("characteristics", {
  skip_on_cran()

  expect_error(discover_nldi_characteristics(type = "test"), "Type must be one of all, local, total, divergence_routed")

  m <- discover_nldi_characteristics()

  expect_equal(names(m), c("local", "total", "divergence_routed"))

  expect_equal(names(m$local), c("characteristic_id", "characteristic_description", "units", "dataset_label", "dataset_url", "theme_label", "theme_url", "characteristic_type"))

  m <- discover_nldi_characteristics(type = "local")

  expect_equal(names(m$local), c("characteristic_id", "characteristic_description", "units", "dataset_label", "dataset_url", "theme_label", "theme_url", "characteristic_type"))

  m <- discover_nldi_characteristics(type = "total")

  expect_equal(names(m$total), c("characteristic_id", "characteristic_description", "units", "dataset_label", "dataset_url", "theme_label", "theme_url", "characteristic_type"))

  site <- list(featureSource = "nwissite", featureID = "USGS-05429700")

  chars <- get_nldi_characteristics(site)

  expect_equal(names(chars), "local")

  expect_equal(names(chars$local), c("characteristic_id", "characteristic_value", "percent_nodata"))

  chars <- get_nldi_characteristics(site, type = "all")

  expect_equal(names(chars), c("local", "total", "divergence_routed"))

  chars <- get_nldi_characteristics(site, type = "total")

  expect_equal(names(chars), "total")
})

test_that("raindrop", {

  skip_on_cran()

  point <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)

  trace <- get_raindrop_trace(point, direction = "up")

  expect_equal(trace$id[1], 'upstreamFlowline')

  expect_equal(trace$id[2], "raindropPath")

  expect_equal(nrow(trace), 2)

  expect_true(inherits(trace, "sf"))

  expect_type(trace$intersection_point, "list")
#
# Doesn't improve coverage
#   trace2 <- get_raindrop_trace(point, direction = "up")
#
#   expect_equal(trace2$id[1], 'upstreamFlowline')
#
#   trace3 <- get_raindrop_trace(point, direction = "none")
#
#   expect_equal(trace3$id[1], "nhdFlowline")
#
#   expect_equal(length(trace3$intersection_point[[1]]), 2)
#
#   expect_equal(length(trace3$intersection_point[[2]]), 0)

  expect_error(get_raindrop_trace(point, direction = "borked"),
               "direction must be in up, down, none")

})

test_that("split", {

  skip_on_cran()

  # Doesn't improve coverage
  # point <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)
  #
  # trace <- get_raindrop_trace(point)
  #
  # dput(sf::st_point(trace$intersection_point[[1]][2:1]))

  snap_point <- sf::st_sfc(sf::st_point(c(-89.213274, 42.956989)),
                           crs = 4326)

  catchment <- get_split_catchment(snap_point, upstream = TRUE)

  area <- sf::st_area(catchment)

  expect_true(area[1] < units::set_units(7000000, "m^2"))

  expect_true(area[2] > units::set_units(900000000, "m^2"))

  # Doesn't improve coverage
  # catchment2 <- get_split_catchment(snap_point, upstream = FALSE)
  #
  # area <- sf::st_area(catchment2)
  #
  # expect_true(area[1] < units::set_units(7000000, "m^2"))
  #
  # expect_true(area[2] < units::set_units(900000000, "m^2"))
  #
  # pour_point <- sf::st_sfc(sf::st_point(x = c(-89.25619, 42.98646)), crs = 4326)
  #
  # catchment3 <- get_split_catchment(pour_point, upstream = FALSE)
  #
  # area <- sf::st_area(catchment3)
  #
  # expect_true(area[1] < units::set_units(7000000, "m^2"))
  #
  # expect_true(area[2] < units::set_units(40000, "m^2"))

})

test_that("xs", {

  skip_on_cran()

  point <- sf::st_sfc(sf::st_point(x = c(-105.97218, 36.17592)), crs = 4326)

  xs <- get_xs_point(point, 300, 100)

  expect_true(inherits(xs, "sf"))
  expect_equal(nrow(xs), 101)

  expect_true(all(c("distance_m", "elevation_m") %in% names(xs)))


  point1 <- sf::st_sfc(sf::st_point(x = c(-105.9667, 36.17602)), crs = 4326)
  point2 <- sf::st_sfc(sf::st_point(x = c(-105.97768, 36.17526)), crs = 4326)

  xs <- get_xs_points(point1, point2, 100)

  expect_true(inherits(xs, "sf"))
  expect_equal(nrow(xs), 101)

  expect_true(all(c("distance_m", "elevation_m") %in% names(xs)))

  expect_error(get_xs_points(point1, point2, 100, 2),
               "res input must be on of 1, 3, 5, 10, 30, 60")

  point1 <- sf::st_sfc(sf::st_point(x = c(-105.9667, 36.17602)), crs = 4326)
  point2 <- sf::st_sfc(sf::st_point(x = c(-105.97768, 36.17526)), crs = 4326)
  point3 <- sf::st_sfc(sf::st_point(x = c(-105.98869, 36.17450)), crs = 4326)

  points <- sf::st_as_sf(c(point1, point2, point3))

  suppressMessages(xs <- get_elev_along_path(points, 100))

  expect_equal(names(xs), c("id", "distance_m", "elevation_m", "spatial_ref", "geometry",
                            ".group"))

  expect_equal(nrow(xs), 202)

  expect_true(mean(xs$elevation_m) - 1822 < 1)

  suppressMessages(xs <- get_elev_along_path(points, 33))

  expect_equal(nrow(xs), 66)
})

test_that("coverage", {
  assign("nldi_tier", "borked",
         envir = nhdplusTools:::nhdplusTools_env)

  expect_error(nhdplusTools:::get_nldi_url(),
               "only prod or test allowed.")

  assign("nldi_tier", "test",
         envir = nhdplusTools:::nhdplusTools_env)

  test <- nhdplusTools:::get_nldi_url()

  expect_equal(test, "https://labs-beta.waterdata.usgs.gov/api/nldi")

  assign("nldi_tier", "prod",
         envir = nhdplusTools:::nhdplusTools_env)

})
