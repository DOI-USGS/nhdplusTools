

test_that("navigation basics", {

  # 06287800
  net <-  navigate_network(list(featureSource = "nwissite", featureID = "USGS-11486500"),
                           "UM",
                           output = "flowlines",
                           trim_start = TRUE)

  expect_equal(nrow(net), 5)

  expect_warning(net2 <-  navigate_network(2562073,
                           "UM",
                           output = "flowlines",
                           trim_start = TRUE),
                 "trim_start ignored for comid start")


  start <- sf::st_sfc(sf::st_point(c(-121.6678, 42.14987)), crs = sf::st_crs(4269))

  net3 <-  navigate_network(start,
                           "UM",
                           output = "flowlines",
                           trim_start = TRUE)

  net_no_split <- navigate_network(start,
                                   "UM",
                                   output = "flowlines",
                                   trim_start = FALSE)


  expect_equal(names(net), names(net3))

  expect_true(length(sf::st_geometry(net_no_split)[[5]]) > length(sf::st_geometry(net)[[5]]))

  net4 <-  navigate_network(start,
                            "UM", network = net_no_split,
                            output = "flowlines",
                            trim_start = TRUE)

  expect_equal(names(net4), names(net))
  expect_equal(nrow(net4), nrow(net))

  expect_error(navigate_network(1234567,
                                "UM", network = net_no_split,
                                output = "flowlines",
                                trim_start = TRUE))

  expect_error(navigate_network("12345",
                                "UM",
                                output = "flowlines",
                                trim_start = TRUE))

  expect_error(navigate_network(12345.5,
                                "UM",
                                output = "flowlines",
                                trim_start = TRUE))

  net5 <-  navigate_network(start,
                            "UM", network = net_no_split,
                            output = "nwissite",
                            trim_start = TRUE)
})
