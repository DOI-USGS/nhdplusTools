context("prepare_nhdplus")

test_that("remove flowlines works for first pass", {
  expect_equal(
    reconcile_removed_flowlines(
      readRDS("data/baltimore_main_remove_flines.rds"),
      readRDS("data/baltimore_main_reroute_set.rds"),
      readRDS("data/baltimore_main_removed.rds"),
      readRDS("data/baltimore_network.rds")),
    readRDS("data/baltimore_main_reconciled.rds"))
})

test_that("remove flowlines works for second pass", {
  expect_equal(
    reconcile_removed_flowlines(
      readRDS("data/baltimore_confluence_remove_flines.rds"),
      readRDS("data/baltimore_confluence_reroute_set.rds"),
      readRDS("data/baltimore_confluence_removed.rds"),
      readRDS("data/baltimore_network.rds")),
    readRDS("data/baltimore_confluence_reconciled.rds"))
})
