context("reconcile_collapse_flowlines")

test_that("reconcile collapse flowlines works as expected", {
  flines <- readRDS("data/baltimore_collapsed.rds")
  flines <- reconcile_collapsed_flowlines(flines)
  expect_equal_to_reference(flines,
                            "data/baltimore_collapsed_reconciled.rds")
})
