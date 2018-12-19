context("match levelpaths")
test_that("match levelpaths runs", {
  start_comid <- 2279159
  net_prep <- readRDS("data/match_levelpaths_2279159.rds")
  matched <- match_levelpaths(net_prep, start_comid)
  expect(matched$LevelPathI[which(matched$HUC12 == "102702020404")] == 550002171)
  expect(matched$LevelPathI[which(matched$HUC12 == "102702050201")] == 550002171)
  expect(matched$LevelPathI[which(matched$HUC12 == "102702020205")] == 550002171)

  # This tests for bad toHUC corrections where the intersection doesn't match the
  # TOHUC coding.
  expect(matched$LevelPathI[which(matched$HUC12 == "102702020102")] == 550031020)
})
