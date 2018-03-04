context("prepare_nhdplus")

test_that("prep_nhdplus_works", {
  expect_equal(
    suppressWarnings(prepare_nhdplus(
      readRDS("data/petapsco_network.rds"),
      min_network_size = 10,
      min_path_length = 1)),
    readRDS("data/petapsco_prepared.rds"))
})
