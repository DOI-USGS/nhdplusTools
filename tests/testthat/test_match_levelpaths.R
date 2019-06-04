context("match levelpaths")
clean_huc12 <- function(huc12) {
  bad_lps <- select(huc12, corrected_LevelPathI, trib_no_intersect, headwater_error) %>%
    filter(trib_no_intersect | headwater_error)

  huc12 %>%
    filter(!corrected_LevelPathI %in% bad_lps$corrected_LevelPathI & !is.na(outlet_HUC12)) %>%
    distinct()
}

test_that("match levelpaths runs 2279159", {
  start_comid <- 2279159
  net_prep <- readRDS(system.file("extdata/match_levelpaths_2279159.rds", package = "nhdplusTools"))
  matched <- match_levelpaths(net_prep, start_comid, add_checks = TRUE)
  expect_true(matched$intersected_LevelPathI[which(matched$HUC12 == "102702020404")] == 550002171)
  expect_true(matched$intersected_LevelPathI[which(matched$HUC12 == "102702050201")] == 550002171)
  expect_true(matched$intersected_LevelPathI[which(matched$HUC12 == "102702020205")] == 550002171)

  # This tests for bad toHUC corrections where the intersection doesn't match the
  # TOHUC coding.
  expect_true(matched$intersected_LevelPathI[which(matched$HUC12 == "102702020102")] == 550031020)
  expect_true(matched$trib_no_intersect[which(matched$HUC12 == "102702020102")])

  # This could be fixed by fixing toHUC codes, but will test it as is for now to verify behavior.
  expect_true(matched$outlet_HUC12[which(matched$HUC12 == "102702020102")] == "102702050705")

  expect_equal(sum(matched$trib_intersect), 20)
  expect_equal(sum(matched$trib_no_intersect), 2)

  # Corrected levelpath and head huc when first order tributary gets corrected.
  expect_true(matched$corrected_LevelPathI[matched$HUC12 == "102702050701"] == 550049901)
  expect_true(matched$head_HUC12[matched$HUC12 == "102702050701"] == "102702050701")

  # Corrected head huc when part of a multi-HU tributary.
  expect_true(matched$head_HUC12[matched$HUC12 == "102702060404"] == "102702060403")

  # If we remove the trib_no_intersect errors this should be unique.
  matched <- filter(matched, !trib_no_intersect & matched$corrected_LevelPathI != "none") %>%
    select(corrected_LevelPathI, head_HUC12, outlet_HUC12) %>%
    distinct()
  expect_true(length(which(duplicated(matched$corrected_LevelPathI))) == 0)
})

test_that("match levelpaths multi-overlap-outlet 931010009", {
  matched <- match_levelpaths(readRDS("data/match_levelpaths_931010009.rds"), 931010009)
  expect_true(nrow(matched) == 4)

  huc12 <- dplyr::select(matched, levelpath = corrected_LevelPathI, head_huc12 = head_HUC12, outlet_huc12 = outlet_HUC12) %>%
    dplyr::filter(!is.na(outlet_huc12)) %>%
    dplyr::distinct()

  expect_true(length(unique(huc12$levelpath)) == nrow(huc12))
})

test_that("match levelpaths funky heatwater 4292649", {
  matched <- match_levelpaths(readRDS("data/match_levelpaths_4292649.rds"), 4292649, add_checks = TRUE)
  expect_true(!150020702 %in% matched$corrected_LevelPathI)

  # not much to do with this one. 010100040905 has multiple overlaps from the wrong watershed.
  # expect_true(!150067066 %in% matched$corrected_LevelPathI)
  expect_true(nrow(matched) == 152)
  expect_true(sum(matched$headwater_error) == 2)

  huc12 <- dplyr::select(clean_huc12(matched), corrected_LevelPathI, head_HUC12, outlet_HUC12) %>%
    dplyr::filter(!is.na(outlet_HUC12)) %>%
    dplyr::distinct()


  expect_true(length(unique(huc12$corrected_LevelPathI)) == nrow(huc12))

  # strange headwater behavior.
  expect_true(huc12$outlet_HUC12[huc12$corrected_LevelPathI == 150014576] == "010100030308")
  expect_true(huc12$head_HUC12[huc12$corrected_LevelPathI == 150014576] == "010100030308")
})

test_that("match levelpaths 20204804", {
  matched <- match_levelpaths(readRDS("data/match_levelpaths_20204804.rds"), 20204804, add_checks = TRUE)
  expect_equal(sum(matched$trib_no_intersect), 5)

  # goofy closed basin breaks things.
  expect_true(matched$intersected_LevelPathI[matched$HUC12 == "180901030502"] == 10004787)
  expect_true(matched$corrected_LevelPathI[matched$HUC12 == "180901030502"] == 10030198)
})

test_that("match levelpaths 10055266", {
  # headwaters of levelpath 200011667
  matched <- match_levelpaths(readRDS("data/match_levelpaths_10055266.rds"), 10055266, add_checks = TRUE)
  expect_true(all(matched$head_HUC12[matched$corrected_LevelPathI == 200011667] == "020802010601"))

  huc12 <- dplyr::select(matched, levelpath = corrected_LevelPathI, head_huc12 = head_HUC12, outlet_huc12 = outlet_HUC12) %>%
    dplyr::filter(!is.na(outlet_huc12)) %>%
    dplyr::distinct()

  expect_true(length(unique(huc12$levelpath)) == nrow(huc12))

})

test_that("match levelpaths 10390202", {
  # uncorrected outlet_HUC12 on levelpath: 800015797
  matched <- match_levelpaths(readRDS("data/match_levelpaths_10390202.rds"), 10390202, add_checks = TRUE)

  huc12 <- dplyr::select(matched,
                         levelpath = corrected_LevelPathI,
                         head_huc12 = head_HUC12,
                         outlet_huc12 = outlet_HUC12) %>%
    dplyr::filter(!is.na(outlet_huc12)) %>%
    dplyr::distinct()

  expect_true(length(unique(huc12$levelpath)) == nrow(huc12))

})


test_that("match levelpaths runs 12228521", {
  start_comid <- 12228521
  net_prep <- readRDS("data/match_levelpaths_12228521.rds")
  matched <- match_levelpaths(net_prep, start_comid, add_checks = TRUE)

  expect_true(matched$head_HUC12[matched$HUC12 == "040601040203"] == "040601040203")
})
