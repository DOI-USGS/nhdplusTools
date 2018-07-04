if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}

context("discover nhdplus id")

test_that("discover nhdplus id errors", {
  expect_error(discover_nhdplus_id(),
               "Must provide point or nldi_feature input.")

  point <- sf::st_sfc(sf::st_point(c(-76.89303, 39.57934)), crs = 4269)
  expect_warning(discover_nhdplus_id(point), "point too close to edge of catchment.")
})

test_that("discover nhdplus id works as expected", {
  point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
  expect_equal(discover_nhdplus_id(point), 11689978)
})
