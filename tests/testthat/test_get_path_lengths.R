
test_that("path_lengths", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
  fline <- walker_flowline

  outlets <- c(5329357, 5329317, 5329365, 5329435, 5329817)

  #' Add toCOMID
  fline[["toCOMID"]] <- nhdplusTools::get_tocomid(fline)

  fl <- dplyr::select(fline, ID = COMID, toID = toCOMID, lengthkm = LENGTHKM)

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
})
