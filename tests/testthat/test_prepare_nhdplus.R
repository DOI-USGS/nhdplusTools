context("prepare_nhdplus")

test_that("prep_nhdplus_works", {
  flines <- suppressWarnings(prepare_nhdplus(
    readRDS("data/petapsco_network.rds"),
    min_network_size = 10,
    min_path_length = 1))
  expect_equal(
    flines,
    readRDS("data/petapsco_prepared.rds"))
})

test_that("prep_nhdplus leaves non-dendritic", {
  flines <- suppressWarnings(prepare_nhdplus(readRDS("data/petapsco_network.rds"),
    min_network_size = 10,
    min_path_length = 1, purge_non_dendritic = FALSE))
  expect_equal(nrow(flines), 707)
})
