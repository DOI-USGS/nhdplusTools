sr <- units::set_units(0.1, "degrees")

source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

flines_in <- sample_flines

flines_in <- sf::st_transform(flines_in, 4269)

test_that("point indexing to nearest existing node works as expected", {
    skip_on_cran()

  point <- sf::st_sfc(sf::st_point(c(-76.86876, 39.49345)), crs = 4269)

  expect_equal(get_flowline_index(flines_in, point),
               data.frame(id = 1,
                          COMID = 11688298,
                          REACHCODE = "02060003000579",
                          REACH_meas = 34.6,
                          offset = 0.000348), tolerance = 0.01)


  expect_equal(get_flowline_index(sf::st_transform(flines_in, 5070),
                                  sf::st_transform(point, 5070)),
               data.frame(id = 1,
                          COMID = 11688298,
                          REACHCODE = "02060003000579",
                          REACH_meas = 33.8,
                          offset = 30.27), tolerance = 0.01)

    expect_equal(suppressWarnings(get_flowline_index("download_nhdplusv2", point, search_radius = sr)$COMID),
                 11688298)

    expect_equal(nrow(get_flowline_index(flines_in, point, search_radius = sr,
                                         max_matches = 5)),
                 5)

    expect_equal(get_flowline_index(flines_in, point, search_radius = sr,
                                    precision = 30),
                 data.frame(id = 1,
                            COMID = 11688298,
                            REACHCODE = "02060003000579",
                            REACH_meas = 25.9,
                            offset = 0.0000959), tolerance = 0.001)

    point_w <- sf::st_sfc(sf::st_point(c(-76.86934, 39.49328)), crs = 4326)

    expect_warning(get_flowline_index(flines_in, point_w,
                                      search_radius = sr),
     "crs of lines and points don't match. attempting st_transform of lines")

    names(flines_in)[1] <- "broken"
    expect_error(get_flowline_index(flines_in, point, search_radius = sr),
                 paste("Missing some required attributes in call to:",
                       "get_flowline_index. Expected: COMID."))
})

test_that("point indexing to for multiple points works", {
  skip_on_cran()

  point <- sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
                           sf::st_point(c(-76.91711, 39.40884)),
                           sf::st_point(c(-76.88081, 39.36354))), crs = 4269)

  expect_equal(get_flowline_index(flines_in, point, search_radius = sr),
               data.frame(id = c(1, 2, 3),
                          COMID = c(11688298, 11688808, 11688980),
                          REACHCODE = c("02060003000579",
                                        "02060003000519",
                                        "02060003000253"),
                          REACH_meas = c(0, 53.58737, 75.37795),
                          offset = c(0.00006026811,
                                     0.00056414104,
                                     0.00031029699)), tolerance = 1e-2)

  expect_equal(get_flowline_index(flines_in, point, search_radius = sr,
                                  precision = 30),
               data.frame(id = c(1, 2, 3),
                          COMID = c(11688298, 11688808, 11688980),
                          REACHCODE = c("02060003000579",
                                        "02060003000519",
                                        "02060003000253"),
                          REACH_meas = c(0, 50.52674, 77.40798),
                          offset = c(0.0000602681,
                                     0.0002523808,
                                     0.0001566810)), tolerance = 1e-2)

  matches <- get_flowline_index(flines_in, point, search_radius = sr, max_matches = 10)
  expect_true("id" %in% names(matches))

  matches2 <- get_flowline_index(flines_in, point, search_radius = sr,
                                 precision = 30, max_matches = 10)

  expect_equal(nrow(matches), nrow(matches2))

  expect_true(all(matches$REACHCODE %in% matches2$REACHCODE))

})

test_that("multipart indexing", {

  points <- sf::read_sf(list.files(pattern = "*flowline_index_reprex.gpkg",
                                   recursive = TRUE, full.names = TRUE), "sites")
  lines <- sf::read_sf(list.files(pattern = "*flowline_index_reprex.gpkg",
                                  recursive = TRUE, full.names = TRUE), "reaches")

  warn <- capture_warnings(index <- nhdplusTools::get_flowline_index(lines, points,
                                                           search_radius = 500))

  expect_true(all(c("Attempting to combine multipart lines into single part lines. Check results!!",
                    "search_radius units not set, trying units of points.")
                  %in% warn))

  expect_true(all(index$COMID == 51664))

})

test_that("disambiguate", {

  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  hydro_location <- sf::st_sf(id = c(1, 2, 3),
                              geom = sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
                                                     sf::st_point(c(-76.91711, 39.40884)),
                                                     sf::st_point(c(-76.88081, 39.36354))),
                                                crs = 4326),
                              totda = c(23.6, 7.3, 427.9),
                              nameid = c("Patapsco", "", "Falls Run River"))

  flowpath <- dplyr::select(sample_flines,
                            comid = COMID,
                            totda = TotDASqKM,
                            nameid = GNIS_NAME,
                            REACHCODE,
                            ToMeas,
                            FromMeas)

  indexes <- get_flowline_index(flowpath,
                                hydro_location,
                                search_radius = units::set_units(0.2, "degrees"),
                                max_matches = 10)

  result <- disambiguate_flowline_indexes(indexes,
                                          dplyr::select(flowpath, comid, totda),
                                          dplyr::select(hydro_location, id, totda))

  expect_equal(nrow(result), 3)

  result <- disambiguate_flowline_indexes(indexes,
                                          dplyr::select(flowpath, comid, nameid),
                                          dplyr::select(hydro_location, id, nameid))

  expect_equal(nrow(result[result$id == 1, ]), 3)

  expect_equal(nrow(result[result$id == 2, ]), 10)

  expect_equal(nrow(result[result$id == 3, ]), 1)

  expect_error(disambiguate_flowline_indexes(indexes,
                                             dplyr::select(flowpath, comid, nameid),
                                             hydro_location),
               "flowpath and hydrolocation must be two-column data.frames")

  expect_error(disambiguate_flowline_indexes(indexes,
                                             dplyr::select(flowpath, comid, nameid),
                                             dplyr::select(hydro_location, id, totda)),
               "flowpath and hydrolocation metrics must both be numeric or character")
})


test_that("rescale", {
  expect_equal(rescale_measures(50, 50, 100), 0)
  expect_equal(rescale_measures(50, 0, 50), 100)
  expect_equal(rescale_measures(25, 0, 50), 50)
  expect_error(rescale_measures(75, 0, 50), "measure must be between from and to")
})

test_that("get location", {
  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  points <- sf::st_sfc(sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
                                       sf::st_point(c(-76.91711, 39.40884)),
                                       sf::st_point(c(-76.88081, 39.36354))),
                                  crs = 4326))

  indexes <- get_flowline_index(sample_flines, points)

  locations <- get_hydro_location(indexes, sample_flines)

  expect_equal(sf::st_coordinates(locations)[, 1:2],
               structure(c(-76.8693957911233, -76.9176139910484, -76.8810037244386,
                           39.4932572053652, 39.4090934721626, 39.3632976055671),
                         .Dim = 3:2, .Dimnames = list(c("1", "2", "3"),
                                                      c("X", "Y"))))

  points <- sf::st_sfc(sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328))),
                                  crs = 4326))

  indexes <- get_flowline_index(sample_flines, points)

  locations <- get_hydro_location(indexes, sample_flines)

  expect_equal(sf::st_coordinates(locations),
               sf::st_coordinates(sf::st_sfc(sf::st_point(c(-76.8694, 39.49326)), crs= 4326)),
               tolerance = 0.001)

})
