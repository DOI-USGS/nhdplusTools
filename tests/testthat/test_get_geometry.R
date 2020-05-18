test_that("get_node", {
  fline <- sf::read_sf(system.file("extdata/sample_natseamless.gpkg",
                               package = "nhdplusTools"),
                   "NHDFlowline_Network")
  start <- get_node(fline, "start")
  end <- get_node(fline, "end")

  # plot(sf::st_zm(fline$geom[1]))
  # plot(sf::st_geometry(start)[1], add = TRUE)
  # plot(sf::st_geometry(end)[1], add = TRUE)
  # dput(sf::st_coordinates(sf::st_geometry(start)[1]))
  # dput(sf::st_coordinates(sf::st_geometry(end)[1]))

  expect_equivalent(sf::st_coordinates(sf::st_geometry(start)[1]),
                    structure(c(-89.2233517052809, 42.9689495999704), .Dim = 1:2, .Dimnames = list(
                      "1", c("X", "Y"))))

  expect_equivalent(sf::st_coordinates(sf::st_geometry(end)[1]),
                    structure(c(-89.2222937052824, 42.9692021333033), .Dim = 1:2, .Dimnames = list(
                      "1", c("X", "Y"))))
})
