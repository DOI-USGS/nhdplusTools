context("prepare_nhdplus")

test_that("prep_nhdplus_works", {
  expect_equal_to_reference(
    suppressWarnings(prepare_nhdplus(
      readRDS("data/baltimore_network.rds"),
      min_network_size = 10,
      min_path_length = 1)),
    "data/baltimore_prepared.rds")
})
