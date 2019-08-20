context("point indexing")

test_that("point indexing to nearest existing node works as expected", {
    skip_on_cran()
    flines_in <- sf::read_sf(system.file("extdata/petapsco_flowlines.gpkg",
                                         package = "nhdplusTools"))
    flines_in <- sf::st_transform(flines_in, 4269)

    point <- sf::st_sfc(sf::st_point(c(-76.86934, 39.49328)), crs = 4269)

    expect_equal(get_flowline_index(flines_in, point, search_radius = 0.1),
                 data.frame(COMID = 11688298,
                            REACHCODE = "02060003000579",
                            REACH_meas = 0,
                            offset = 0.00006026811,
                            stringsAsFactors = FALSE), tolerance = 0.0001)

    expect_equal(get_flowline_index(flines_in, point, search_radius = 0.1,
                                    precision = 30),
           data.frame(COMID = 11688298,
                      REACHCODE = "02060003000579",
                      REACH_meas = 0,
                      offset = 0.00006026811,
                      stringsAsFactors = FALSE), tolerance = 0.0001)

    point_w <- sf::st_sfc(sf::st_point(c(-76.86934, 39.49328)), crs = 4326)

    expect_warning(get_flowline_index(flines_in, point_w,
                                      search_radius = 0.1),
     "crs of lines and points don't match. attempting st_transform of points")

    names(flines_in)[1] <- "broken"
    expect_error(get_flowline_index(flines_in, point, search_radius = 0.1),
                 paste("Missing some required attributes in call to:",
                       "get_flowline_index. Expected: COMID."))
})

test_that("point indexing to for multiple points works", {
  skip_on_cran()
  flines_in <- sf::read_sf(system.file("extdata/petapsco_flowlines.gpkg",
                                       package = "nhdplusTools"))
  flines_in <- sf::st_transform(flines_in, 4269)

  point <- sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
                           sf::st_point(c(-76.91711, 39.40884)),
                           sf::st_point(c(-76.88081, 39.36354))), crs = 4269)

  expect_equal(get_flowline_index(flines_in, point, search_radius = 0.1),
               data.frame(COMID = c(11688298, 11688808, 11688980),
                          REACHCODE = c("02060003000579",
                                        "02060003000519",
                                        "02060003000253"),
                          REACH_meas = c(0, 53.58737, 75.37795),
                          offset = c(0.00006026811,
                                     0.00056414104,
                                     0.00031029699),
                          stringsAsFactors = F), tolerance = 1e-2)

  expect_equal(get_flowline_index(flines_in, point, search_radius = 0.1,
                                  precision = 30),
               data.frame(COMID = c(11688298, 11688808, 11688980),
                          REACHCODE = c("02060003000579",
                                        "02060003000519",
                                        "02060003000253"),
                          REACH_meas = c(0, 50.52674, 77.40798),
                          offset = c(0.0000602681,
                                     0.0002523808,
                                     0.0001566810),
                          stringsAsFactors = F), tolerance = 1e-2)

})
