context("discover nhdplus id")

test_that("nldi basics work", {

  nldi_sources <- discover_nldi_sources()

  expect_equal(class(nldi_sources), "data.frame")

  expect(all(c("comid", "huc12pp", "nwissite") %in% nldi_sources$source))

  expect_equal(names(nldi_sources), c("source", "sourceName", "features"))

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

  expect_equal(length(discover_nldi_navigation(nldi_nwis)), 4)

})

test_that("navigation works", {

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

  nav <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "UT",
                       data_source = "nwissite",
                       distance_km = 10000)

  expect("sf" %in% class(nav), "expected an sf data.frame")

  expect("sfc_POINT" %in% class(sf::st_geometry(nav)),
         "expected point response")

  nav2 <- navigate_nldi(nldi_feature = nldi_nwis,
                       mode = "upstreamTributaries",
                       data_source = "nwissite",
                       distance_km = 50)

  expect(nrow(nav) > nrow(nav2))
})
