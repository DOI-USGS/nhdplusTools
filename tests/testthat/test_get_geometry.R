test_that("get_node", {
  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

  fline <- sf::read_sf(sample_data,
                       "NHDFlowline_Network")

  start <- get_node(fline, "start")
  end <- get_node(fline, "end")

  # plot(sf::st_zm(fline$geom[1]))
  # plot(sf::st_geometry(start)[1], add = TRUE)
  # plot(sf::st_geometry(end)[1], add = TRUE)
  # dput(sf::st_coordinates(sf::st_geometry(start)[1]))
  # dput(sf::st_coordinates(sf::st_geometry(end)[1]))

  expect_equal(sf::st_coordinates(sf::st_geometry(start)[1]),
               structure(c(-89.2233517052809, 42.9689495999704), .Dim = 1:2, .Dimnames = list(
                 "1", c("X", "Y"))), ignore_attr = TRUE)

  expect_equal(sf::st_coordinates(sf::st_geometry(end)[1]),
               structure(c(-89.2222937052824, 42.9692021333033), .Dim = 1:2, .Dimnames = list(
                 "1", c("X", "Y"))), ignore_attr = TRUE)
})

test_that("fix_flowdir", {
  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

  fline <- sf::read_sf(sample_data, "NHDFlowline_Network")

  suppressWarnings(
  # We add a tocomid with prepare_nhdplus
  fline <- sf::st_sf(prepare_nhdplus(fline, 0, 0, 0, FALSE),
                     geom = sf::st_zm(sf::st_geometry(fline)))
  )
  # Look at the end node of the 10th line.
  n1 <- get_node(fline[10, ], position = "end")

  # Break the geometry by reversing it.
  sf::st_geometry(fline)[10] <- sf::st_reverse(sf::st_geometry(fline)[10])

  # Note that the end node is different now.
  n2 <- get_node(fline[10, ], position = "end")

  # Pass the broken geometry to fix_flowdir with the network for toCOMID
  sf::st_geometry(fline)[10] <- fix_flowdir(fline$COMID[10], fline)

  # Note that the geometry is now in the right order.
  n3 <- get_node(fline[10, ], position = "end")

  expect_equal(n1, n3)

  n1 <- get_node(fline[1, ], position = "end")
  sf::st_geometry(fline)[1] <- sf::st_reverse(sf::st_geometry(fline)[1])
  sf::st_geometry(fline)[1] <- fix_flowdir(fline$COMID[1], fline)
  expect_equal(n1, get_node(fline[1, ], position = "end"))

  fline$toCOMID[1] <- 0
  n1 <- get_node(fline[1, ], position = "end")
  sf::st_geometry(fline)[1] <- sf::st_reverse(sf::st_geometry(fline)[1])
  sf::st_geometry(fline)[1] <- fix_flowdir(fline$COMID[1], fline)
  expect_equal(n1, get_node(fline[1, ], position = "end"))

  fn_list <- list(flowline = fline[1, ],
                  network = fline[fline$toCOMID == fline$COMID[1],],
                  check_end = "start")

  sf::st_geometry(fline)[1] <- sf::st_reverse(sf::st_geometry(fline)[1])
  sf::st_geometry(fline)[1] <- fix_flowdir(fline$COMID[1], fn_list = fn_list)
  expect_equal(n1, get_node(fline[1, ], position = "end"))
})
