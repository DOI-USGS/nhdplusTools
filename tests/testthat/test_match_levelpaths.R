context("match levelpaths")
test_that("match levelpaths runs", {
  start_comid <- 2279159
  net_prep <- readRDS("data/match_levelpaths_2279159.rds")
  matched <- match_levelpaths(net_prep, start_comid, add_checks = TRUE)
  expect(matched$intersected_LevelPathI[which(matched$HUC12 == "102702020404")] == 550002171)
  expect(matched$intersected_LevelPathI[which(matched$HUC12 == "102702050201")] == 550002171)
  expect(matched$intersected_LevelPathI[which(matched$HUC12 == "102702020205")] == 550002171)

  # This tests for bad toHUC corrections where the intersection doesn't match the
  # TOHUC coding.
  expect(matched$intersected_LevelPathI[which(matched$HUC12 == "102702020102")] == 550031020)
  expect(matched$trib_no_intersect[which(matched$HUC12 == "102702020102")])

  # This could be fixed by fixing toHUC codes, but will test it as is for now to verify behavior.
  expect(matched$outlet_HUC12[which(matched$HUC12 == "102702020102")] == "102702050705")

  expect_equal(sum(matched$trib_intersect), 20)
  expect_equal(sum(matched$trib_no_intersect), 2)

  # Corrected levelpath and head huc when first order tributary gets corrected.
  expect(matched$corrected_LevelPathI[matched$HUC12 == "102702050701"] == 550049901)
  expect(matched$head_HUC12[matched$HUC12 == "102702050701"] == "102702050701")

  # Corrected head huc when part of a multi-HU tributary.
  expect(matched$head_HUC12[matched$HUC12 == "102702060404"] == "102702060403")

  # If we remove the trib_no_intersect errors this should be unique.
  matched <- filter(matched, !trib_no_intersect) %>%
    select(corrected_LevelPathI, head_HUC12, outlet_HUC12) %>%
    distinct()
  expect(length(which(duplicated(matched$corrected_LevelPathI))) == 0)
})

test_that("match levelpaths multi-overlap-outlet", {
  matched <- match_levelpaths(readRDS("data/match_levelpaths_931010009.rds"), 931010009)
  expect(nrow(matched) == 4)
})

test_that("match levelpaths funky heatwater", {
  matched <- match_levelpaths(readRDS("data/match_levelpaths_4292649.rds"), 4292649, add_checks = TRUE)
  expect(!150020702 %in% matched$corrected_LevelPathI)

  # not much to do with this one. 010100040905 has multiple overlaps from the wrong watershed.
  # expect(!150067066 %in% matched$corrected_LevelPathI)
  expect(nrow(matched) == 152)
  expect(sum(matched$headwater_error) == 2)
})

test_that("match levelpaths ", {
  matched <- match_levelpaths(readRDS("data/match_levelpaths_20204804.rds"), 20204804, add_checks = TRUE)
  expect(sum(matched$trib_no_intersect), 5)

  # goofy closed basin breaks things.
  expect(matched$intersected_LevelPathI[matched$HUC12 == "180901030502"] == 10004787)
  expect(matched$corrected_LevelPathI[matched$HUC12 == "180901030502"] == 10030198)
})

# outlet:
# 4292649
# look at: 010100030308

# test_that("match levelpaths ", {
#   matched <- match_levelpaths(readRDS("data/match_levelpaths_4292649.rds"), 4292649, add_checks = TRUE)
#
# })
