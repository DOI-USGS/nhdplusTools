context("get_nhdplus")

test_that("get_nhdplus_byid", {
  comid_set <- get_UT(readRDS("data/petapsco_network.rds"), 11687180)

  catchmentsp <- get_nhdplus_byid(comid_set, "catchmentsp")

  expect("sf" %in% class(catchmentsp), "expected class sf")

  expect_equal(nrow(catchmentsp), 5)

  nhdflowline_network <- get_nhdplus_byid(comid_set, "nhdflowline_network")

  expect("sf" %in% class(nhdflowline_network), "expected class sf")

  expect_equal(nrow(nhdflowline_network), 5)

  expect_error(get_nhdplus_byid(comid_set, "testest"),
               "Layer must be one of catchmentsp, nhdflowline_network")

})

test_that("get_nhdplus_bybox", {
  bbox <- readRDS("data/petapsco_network.rds") %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  layers <- c("nhdarea", "nhdwaterbody")

  for (layer in layers) {
    l <- get_nhdplus_bybox(bbox, layer)
    expect(nrow(l) > 1, "expected to get data")
    expect("sf" %in% class(l))
  }

})
