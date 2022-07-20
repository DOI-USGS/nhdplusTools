
test_that("path_lengths", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
  fline <- walker_flowline

  names(fline) <- tolower(names(fline))

  outlets <- c(5329357, 5329317, 5329365, 5329435, 5329817)

  #' Add toCOMID
  fline <- nhdplusTools::get_tocomid(fline)

  fl <- dplyr::select(fline, ID = comid, toID = tocomid, lengthkm = lengthkm)

  pl <- get_path_members(outlets, fl)

  expect_equal(nrow(pl), 10)

  expect_type(pl$path, "list")

  pl <- get_path_lengths(outlets, fl)

  expect_equal(pl$network_distance_km[pl$ID_1 == 5329357 & pl$ID_2 == 5329365],
               3.6, tolerance = 0.01)
  expect_equal(pl$network_distance_km[pl$ID_1 == 5329357 & pl$ID_2 == 5329817],
               8.9, tolerance = 0.01)

  expect_equal(nrow(pl), 10)

  outlets <- c(outlets, 5329303) # add the terminal

  pl <- get_path_lengths(outlets, fl)

  expect_equal(nrow(pl), 15)

  expect_equal(pl$network_distance_km[pl$ID_1 == 5329317 & pl$ID_2 == 5329303],
               5.8, tolerance = 0.01)

  expect_error(get_path_lengths(c(outlets, 12345), fl),
               "All outlets must be in network.")
})

test_that("get_partial_length", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
  hydro_location <- list(comid = 5329339,
                         reachcode = "18050005000078",
                         reach_meas = 30)

  pl30 <- get_partial_length(hydro_location, walker_flowline)

  hydro_location <- list(comid = 5329339,
                         reachcode = "18050005000078",
                         reach_meas = 60)

  pl60 <- get_partial_length(hydro_location, walker_flowline)

  expect_true(pl30$dn < pl60$dn)

  expect_true(pl30$up > pl60$up)

  expect_error(get_partial_length(hydro_location),
               "network must be supplied if flowline is null")

  hydro_location <- list(comid = 5329339,
                         reachcode = "180500050000bork",
                         reach_meas = 60)

  expect_error(get_partial_length(hydro_location, walker_flowline),
               "hydrolocation not found in network provided")

  hydro_location <- list(comid = 5329339,
                         reachcode = "18050005000078",
                         reach_meas = 100)

  pl100 <- get_partial_length(hydro_location, walker_flowline)

  expect_equal(pl100, list(dn = 4.786, up = 0))

  hydro_location <- list(comid = 5329339,
                         reachcode = "18050005000078",
                         reach_meas = 0)

  pl0 <- get_partial_length(hydro_location, walker_flowline)

  expect_equal(pl0, list(dn = 0, up = 4.786))
})
