context("get_DM")

test_that("get_DM works", {
  result <- get_DM(readRDS("data/petapsco_network.rds"),11689050)
  expect_equal(length(result),26)
})
