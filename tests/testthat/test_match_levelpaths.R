context("match levelpaths")
test_that("match levelpaths runs", {
  start_comid <- 2279159
  net_prep <- readRDS("data/match_levelpaths_2279159.rds")
  matched <- match_levelpaths(net_prep, start_comid, add_checks = TRUE)
  expect(matched$LevelPathI[which(matched$HUC12 == "102702020404")] == 550002171)
  expect(matched$LevelPathI[which(matched$HUC12 == "102702050201")] == 550002171)
  expect(matched$LevelPathI[which(matched$HUC12 == "102702020205")] == 550002171)

  # This tests for bad toHUC corrections where the intersection doesn't match the
  # TOHUC coding.
  expect(matched$LevelPathI[which(matched$HUC12 == "102702020102")] == 550031020)


  expect_equal(sum(matched$trib_intersect), 20)
  expect_equal(sum(matched$trib_no_intersect), 2)
  expect_equal(sum(matched$headwater_error), 0)
})

test_that("match levelpaths multi-overlap-outlet", {
  matched <- match_levelpaths(readRDS("data/match_levelpaths_931010009.rds"), 931010009)
  expect(nrow(matched) == 4)
})

test_that("match levelpaths funky heatwater", {
  matched <- match_levelpaths(readRDS("data/match_levelpaths_4292649.rds"), 4292649, add_checks = TRUE)
  expect(sum(matched$headwater_error) == 8)
})
