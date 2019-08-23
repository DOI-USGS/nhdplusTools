context("get_nhdplus")

pt_data <- sf::read_sf(system.file("extdata/petapsco_flowlines.gpkg",
                                   package = "nhdplusTools"))

test_that("get_nhdplus_byid", {
  skip_on_cran()
  comid_set <- get_UT(pt_data, 11687180)

  catchmentsp <- nhdplusTools:::get_nhdplus_byid(comid_set, "catchmentsp")

  expect("sf" %in% class(catchmentsp), "expected class sf")

  expect_equal(nrow(catchmentsp), 5)

  nhdflowline_network <- nhdplusTools:::get_nhdplus_byid(comid_set, "nhdflowline_network")

  expect("sf" %in% class(nhdflowline_network), "expected class sf")

  expect_equal(nrow(nhdflowline_network), 5)

  expect_error(nhdplusTools:::get_nhdplus_byid(comid_set, "testest"),
               "Layer must be one of catchmentsp, nhdflowline_network")

})

test_that("get_nhdplus_bybox", {
  skip_on_cran()
  bbox <- pt_data %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  layers <- c("nhdarea", "nhdwaterbody")

  for (layer in layers) {
    l <- nhdplusTools:::get_nhdplus_bybox(bbox, layer)
    expect(nrow(l) > 1, "expected to get data")
    expect_true("sf" %in% class(l))
  }

  expect_error(nhdplusTools:::get_nhdplus_bybox(bbox, "borked"),
               "Layer must be one of nhdarea, nhdwaterbody")

})

test_that("we get urls for nhdplushr", {
  skip_on_cran()
  urls <- download_nhdplushr(tempdir(), c("01", "0203"), download_files = FALSE)

  expect_equal(length(urls), 11)
})

test_that("get_nhdplushr runs", {
  skip_on_cran()
  work_dir <- tempdir()

  get_test_file(work_dir)

  out <- get_nhdplushr(work_dir, out_gpkg = file.path(work_dir, "temp.gpkg"))

  layers <- sf::st_layers(out)
  expect_equal(layers$name, c("NHDFlowline", "NHDPlusCatchment"))
  expect_equal(layers$features, c(2691, 2603))

  out <- get_nhdplushr(work_dir, out_gpkg = file.path(work_dir, "temp.gpkg"), layers = NULL)

  layers <- sf::st_layers(out)

  expect_equal(length(layers$name), 7)
  expect_equal(layers$fields[which(layers$name == "NHDFlowline")], 57)

  out <- get_nhdplushr(work_dir, layers = NULL)

  expect(length(names(out)), 7)
})
