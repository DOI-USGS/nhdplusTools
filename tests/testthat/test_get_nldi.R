context("nldi tests")

test_that("nldi basics work", {

  skip_on_cran()

  nldi_sources <- discover_nldi_sources()

  expect_equal(class(nldi_sources), "data.frame")

  expect_true(all(c("comid", "huc12pp", "nwissite") %in% nldi_sources$source))

  expect_equal(names(nldi_sources), c("source", "sourceName", "features"))

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

  expect_equal(length(discover_nldi_navigation(nldi_nwis)), 4)

  expect_equal(length(discover_nldi_navigation(nldi_nwis, tier = "test")), 4)

  expect_error(discover_nldi_navigation(nldi_nwis, tier = "borked"),
               "only prod or test allowed.")

  expect_error(discover_nldi_navigation(nldi_nwis[1]),
                 "Missing some required input for NLDI. Expected: featureID")

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
})

test_that("basin works", {

  skip_on_cran()

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05427880")

  nav <- get_nldi_basin(nldi_feature = nldi_nwis)

  expect("sf" %in% class(nav), "expected an sf data.frame")

  expect_true("sfc_POLYGON" %in% class(sf::st_geometry(nav)),
         "expected polygon response")
})
