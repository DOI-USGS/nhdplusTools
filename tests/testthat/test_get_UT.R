context("get_UT")

test_that("get_UT works", {
  result <- get_UT(readRDS("data/petapsco_network.rds"),11689276)
  expect_equal(length(result),683)
})
