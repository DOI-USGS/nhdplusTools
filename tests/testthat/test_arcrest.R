AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
                                   xmax = -89.24681, ymax = 43.17192),
                                 crs = "+proj=longlat +datum=WGS84 +no_defs"))

test_that("spatial_filter", {

  expect_error(nhdplusTools:::spatial_filter(AOI, format = "test"),
               "ogc or esri")

  expect_equal(
    nhdplusTools:::spatial_filter(AOI, format = "ogc")[[1]],
    "<ogc:BBOX><ogc:PropertyName>the_geom</ogc:PropertyName><gml:Envelope srsName=\"urn:x-ogc:def:crs:EPSG:4326\"><gml:lowerCorner>42.99816 -89.56684</gml:lowerCorner><gml:upperCorner>43.17192 -89.24681</gml:upperCorner></gml:Envelope></ogc:BBOX>")

  expect_equal(as.character(nhdplusTools:::spatial_filter(AOI, format = "esri")[[1]]),
               '{"xmin":-89.5668,"ymin":42.9982,"xmax":-89.2468,"ymax":43.1719,"spatialReference":{"wkid":4326}}')
})

test_that("check_query_params", {

  ids <- NULL
  type <- "test"
  source <- data.frame(user_call = c("test", "test2"))
  t_srs <- NULL
  buffer <- 0.5
  where <- NULL

  expect_equal(
    nhdplusTools:::check_query_params(AOI, ids, type, where, source, t_srs, buffer),
    list(AOI = AOI, t_srs = sf::st_crs(AOI)))

  expect_error(nhdplusTools:::check_query_params(AOI, c(1,2), type, where, source, t_srs, buffer),
               "Either IDs or")

  expect_error(nhdplusTools:::check_query_params(NULL, NULL, type, where, source, t_srs, buffer),
               "IDs or a spatial AOI must be passed.")

  expect_error(nhdplusTools:::check_query_params(AOI, ids, "test3", where, source, t_srs, buffer),
               "test, test2")

  expect_error(nhdplusTools:::check_query_params(c(AOI, AOI), ids, type, where, source, t_srs, buffer),
               "AOI must be one an only one feature.")

  AOI <- sf::st_sfc(sf::st_point(c(-89.56684, 42.99816)), crs = 4326)

  expect_equal(
    nhdplusTools:::check_query_params(AOI, ids, type, where, source, t_srs, buffer)$AOI[[1]],
    structure(list(structure(c(521279.507824339, 521279.507824339,
                               521280.507824339, 521280.507824339,
                               521279.507824339, 2240146.81449532,
                               2240147.81449532, 2240147.81449532,
                               2240146.81449532, 2240146.81449532),
                             dim = c(5L, 2L))),
              class = c("XY", "POLYGON", "sfg")), tolerance = 0.1)

})

test_that("basic 3dhp service requests", {
  skip_on_cran()

  AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.5, ymin = 43.0,
                                     xmax = -89.4, ymax = 43.1),
                                   crs = "+proj=longlat +datum=WGS84 +no_defs"))

  expect_message(expect_s3_class(nhdplusTools:::query_usgs_arcrest(AOI),
                                 "data.frame"))

  expect_warning(expect_warning(nhdplusTools:::query_usgs_arcrest(AOI, type = "hydrolocation")))

  test_data <- nhdplusTools:::query_usgs_arcrest(AOI, type = "reach code, external connection")

  expect_s3_class(test_data, "sf")
})
