context("nldi tests")

test_that("nldi basics work", {

  skip_on_cran()

  nldi_sources <- discover_nldi_sources()

  expect_equal(class(nldi_sources), "data.frame")

  expect_true(all(c("comid", "huc12pp", "nwissite") %in% nldi_sources$source))

  expect_equal(names(nldi_sources), c("source", "sourceName", "features"))

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

  expect_equal(length(discover_nldi_navigation(nldi_nwis)), 4)

  # expect_equal(length(discover_nldi_navigation(nldi_nwis, tier = "test")), 4)

  expect_error(discover_nldi_navigation(nldi_nwis, tier = "borked"),
               "only prod or test allowed.")

  expect_error(discover_nldi_navigation(nldi_nwis[1]),
                 "Missing some required input for NLDI. Expected length 2 character vector or list with optional names: featureID")

})

test_that("navigation works", {

  skip_on_cran()

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

  nav <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "UM",
                       data_source = "nwissite",
                       distance_km = 1)

  expect("sf" %in% class(nav), "expected an sf data.frame")

  expect_true("sfc_POINT" %in% class(sf::st_geometry(nav)),
         "expected point response")

  nav2 <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "upstreamMain",
                       data_source = "nwissite",
                       distance_km = 100)

  expect_true(nrow(nav2) > nrow(nav))

  nldi_nwis <- as.character(nldi_nwis)

  nav3 <- navigate_nldi(nldi_feature = nldi_nwis,
                        mode = "upstreamMain",
                        data_source = "flowlines",
                        distance_km = 10)

  expect_is(sf::st_geometry(nav3), "sfc_LINESTRING")

  expect_warning(nav3 <- navigate_nldi(nldi_feature = nldi_nwis,
                                       mode = "upstreamMain",
                                       data_source = "flowline",
                                       distance_km = 10),
                 "data source specified as flowline or '' is deprecated")

  nav <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-08279500/navigation/UM",
                       data_source = "dumb",
                       distance_km = 1)

  expect_equal(nav, dplyr::tibble())

  nav <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-08279500/navigation/UM",
                       data_source = "nwissite",
                       distance_km = 1)

  expect("sf" %in% class(nav), "expected an sf data.frame")

  # expect_equal(navigate_nldi(list(featureSource = "wqp",
  #                                 featureID = "TCEQMAIN-16638"),
  #                            mode = "upstreamMain",
  #                            data_source = "nwissite"), dplyr::tibble())
})

test_that("basin works", {

  skip_on_cran()

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05427880")

  nav <- get_nldi_basin(nldi_feature = nldi_nwis)

  expect("sf" %in% class(nav), "expected an sf data.frame")

  expect_true("sfc_POLYGON" %in% class(sf::st_geometry(nav)),
         "expected polygon response")
})

test_that("get feature works", {
  skip_on_cran()

  f <- get_nldi_feature(list(featureSource = "nwissite", featureID = "USGS-05428500"))

  expect_equal(nrow(f), 1)
  expect_equal(ncol(f), 8)
  expect_equal(f$identifier, "USGS-05428500")

  f <- get_nldi_feature(list("nwissite", "USGS-05428500"))

  expect_equal(nrow(f), 1)
  expect_equal(ncol(f), 8)
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
