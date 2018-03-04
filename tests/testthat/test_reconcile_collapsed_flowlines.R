context("reconcile_collapse_flowlines")

test_that("reconcile collapse flowlines works as expected", {
  flines <- readRDS("data/petapsco_collapsed.rds")
  flines <- reconcile_collapsed_flowlines(flines)
  expect_equal(flines,readRDS("data/petapsco_collapsed_reconciled.rds"))
})
