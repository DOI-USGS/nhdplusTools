context("get_nhdplushr")

test_that("we get urls for nhdplushr", {
  skip_on_cran()
  urls <- download_nhdplushr(tempdir(), c("01", "0203"), download_files = FALSE)

  expect_equal(length(urls), 11)
})

test_that("get_nhdplushr runs", {
  skip_on_cran()
  work_dir <- tempdir()

  get_test_file(work_dir)

  out_gpkg <- file.path(work_dir, "temp.gpkg")

  out <- get_nhdplushr(work_dir, out_gpkg = out_gpkg)

  layers <- sf::st_layers(out_gpkg)
  expect_equal(layers$name, c("NHDFlowline", "NHDPlusCatchment"))
  expect_equal(layers$features, c(2691, 2603))

  out <- get_nhdplushr(work_dir, out_gpkg = out_gpkg, layers = NULL)

  layers <- sf::st_layers(out_gpkg)

  expect_equal(length(layers$name), 7)
  expect_equal(layers$fields[which(layers$name == "NHDFlowline")], 57)

  out <- get_nhdplushr(work_dir, layers = NULL)

  expect(length(names(out)), 7)
})
