
if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}

context("package setup")

test_that("nhdplus_data_path sets and gets right", {
  expect_equal(nhdplus_path(), "../NHDPlusV21_National_Seamless.gdb")

  expect_equal(nhdplus_path("test", warn = FALSE), 1)

  expect_equal(nhdplus_path(), "test")

  expect_warning(nhdplus_path("test", warn = TRUE), "Path does not exist.")

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")
})

test_that("prep_nhdplus runs as expected", {
  if (!dir.exists("data/temp")) dir.create("data/temp")

  expect_error(suppressWarnings(prep_national_data()),
                 paste("Didn't find NHDPlus national data in default",
                       "location: ../NHDPlusV21_National_Seamless.gdb"))

  sample_gpkg <- "data/sample_natseamless.gpkg"

  nhdplus_path(sample_gpkg)

  expect_warning(temp_data <- prep_national_data(),
                 "No output path provided, using: data")

  temp_data <- lapply(temp_data, unlink)

  temp_data <- prep_national_data(output_path = "data/temp")

  expect(suppressWarnings(all(lapply(temp_data, file.exists))))

  temp_data <- lapply(temp_data, unlink)

  nhdplus_path("bogus")

  expect_error(suppressWarnings(prep_national_data()),
               paste("Didn't find NHDPlus national data in",
                     "user specified location: bogus"))

  nhdplus_path(sample_gpkg)

  expect_error(prep_national_data(include = c("bogus"),
                                  output_path = "data/temp"),
               paste("Got invalid include entries. Expect one",
                     "or more of: attribute, flowline, catchment."))

  temp_data <- prep_national_data(output_path = "data/temp")

  expect_equal(
    capture_warnings(
      temp_data <- prep_national_data(output_path = "data/temp")),
    c("attributes file exists", "flowline file exists", "catchment already exists."))

  temp_data <- lapply(temp_data, unlink)

})

context("discover nhdplus id")

test_that("discover nhdplus id errors", {
  expect_error(discover_nhdplus_id(),
               "Must provide point or nldi_feature input.")

  point <- sf::st_sfc(sf::st_point(c(-76.89303, 39.57934)), crs = 4269)
  expect_warning(discover_nhdplus_id(point),
                 "point too close to edge of catchment.")
})

test_that("discover nhdplus id works as expected", {

  point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
  expect_equal(discover_nhdplus_id(point), 11689978)
  expect_equal(discover_nhdplus_id(point = point), 11689978)

  nldi_huc12 <- list(featureSource = "huc12pp", featureID = "070700051701")
  expect_equal(discover_nhdplus_id(nldi_feature = nldi_huc12), 13637491)

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
  expect_equal(discover_nhdplus_id(nldi_feature = nldi_nwis), 17864756)


})
