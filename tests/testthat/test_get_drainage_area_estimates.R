test_that("find_immediate_huc12_outlets with synthetic network", {
  # Linear: 1 -> 2 -> 3 -> 4 -> 5 (outlet)
  # HUC12 outlets at 2 and 4.
  # Trimmed (remove {2,4}): {1,3,5} with toids {2,4,0}.
  # navigate_network_dfs up from 5 in trimmed: nothing flows to 5
  # -> reachable = {5}.
  # comid 4: toid=5 == start -> immediate.
  # comid 2: toid=3, 3 not in {5} -> NOT immediate (blocked by 4).
  net <- data.frame(
    comid = c(1, 2, 3, 4, 5),
    toid = c(2, 3, 4, 5, 0),
    areasqkm = rep(10, 5),
    totdasqkm = c(50, 40, 30, 20, 10)
  )

  result <- nhdplusTools:::find_immediate_huc12_outlets(
    net, 5, c(2, 4)
  )
  expect_equal(result, 4)

  # Branching: 1, 2, 3 all flow to 4, 4 flows to 5 (outlet)
  # HUC12 outlets at 1 and 2.
  # Trimmed (remove {1,2}): {3,4,5} with toids {4,5,0}.
  # navigate up from 5: 4->5, 3->4. reachable = {5,4,3}.
  # comid 1: toid=4, 4 in reachable -> immediate.
  # comid 2: toid=4, 4 in reachable -> immediate.
  net2 <- data.frame(
    comid = c(1, 2, 3, 4, 5),
    toid = c(4, 4, 4, 5, 0)
  )

  result2 <- nhdplusTools:::find_immediate_huc12_outlets(
    net2, 5, c(1, 2)
  )
  expect_equal(sort(result2), c(1, 2))

  # Linear: 1 -> 2 -> 3 -> 4 (outlet)
  # HUC12 outlets at 1 and 3. 1 is behind 3.
  # Trimmed (remove {1,3}): {2,4} with toids {3,0}.
  # navigate up from 4: nothing flows to 4 -> reachable = {4}.
  # comid 3: toid=4 == start -> immediate.
  # comid 1: toid=2, 2 not in {4} -> NOT immediate (blocked by 3).
  net3 <- data.frame(
    comid = c(1, 2, 3, 4),
    toid = c(2, 3, 4, 0)
  )

  result3 <- nhdplusTools:::find_immediate_huc12_outlets(
    net3, 4, c(1, 3)
  )
  expect_equal(result3, 3)
})

test_that("add_huc12_area_columns", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
    ))),
    crs = 5070
  )
  hu12 <- sf::st_sf(ncontrb_a = 100, geometry = poly)

  result <- nhdplusTools:::add_huc12_area_columns(hu12)

  expect_true("dasqkm" %in% names(result))
  expect_true("ncontrb_sqkm" %in% names(result))
  expect_true("contrib_sqkm" %in% names(result))
  expect_true(result$dasqkm > 0)
  expect_equal(result$ncontrb_sqkm, 100 * 0.00404686)
  expect_equal(result$contrib_sqkm, result$dasqkm - result$ncontrb_sqkm)
})

test_that("plan_upstream_huc12_fetches single HUC10", {
  # All IDs in one HUC10 -> HUC10 and HUC08 estimates reuse HUC12-only
  ids <- c("070702010101", "070702010102", "070702010103")
  outlet <- "070702010101"

  plan <- nhdplusTools:::plan_upstream_huc12_fetches(ids, outlet)

  expect_equal(plan$huc12_est$fetch_ids, ids)
  expect_true(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, character(0))
  expect_null(plan$huc10_est$outlet_backfill)
  expect_true(plan$huc08_est$same_as_huc10)
  expect_equal(plan$huc08_est$bulk_huc08s, character(0))
  expect_null(plan$huc08_est$outlet_backfill)
})

test_that("plan_upstream_huc12_fetches multi-HUC10 single-HUC08", {
  # Two HUC10s in the same HUC08 (07070201)
  ids <- c("070702010101", "070702010102", "070702010501")
  outlet <- "070702010101"

  plan <- nhdplusTools:::plan_upstream_huc12_fetches(ids, outlet)

  # HUC10 estimate: one upstream HUC10, one local HUC12 in outlet HUC10
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, "0707020105")
  expect_equal(plan$huc10_est$local_huc12_ids, "070702010102")

  # outlet_backfill present for multi-HUC10
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc10s, "0707020101")
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc12_ids, "070702010101")

  # HUC08 estimate: single HUC08 -> same as HUC10
  expect_true(plan$huc08_est$same_as_huc10)
})

test_that("plan_upstream_huc12_fetches multi-HUC08", {
  ids <- c(
    "070702010101", "070702010102",  # outlet HUC08 07070201, outlet HUC10
    "070703020301",                   # different HUC08 07070302
    "070801010101"                    # different HUC08 07080101
  )
  outlet <- "070702010101"

  plan <- nhdplusTools:::plan_upstream_huc12_fetches(ids, outlet)

  # HUC10 estimate: 2 upstream HUC10s
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(sort(plan$huc10_est$bulk_huc10s),
    sort(c("0707030203", "0708010101")))
  expect_equal(plan$huc10_est$local_huc12_ids, "070702010102")

  # outlet_backfill for HUC10 estimate
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc10s, "0707020101")
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc12_ids, "070702010101")

  # HUC08 estimate: two upstream HUC08s
  expect_false(plan$huc08_est$same_as_huc10)
  expect_equal(sort(plan$huc08_est$bulk_huc08s),
    sort(c("07070302", "07080101")))
  # local HUC12s = huc12pp in outlet HUC08 (excluding outlet HUC12)
  expect_equal(plan$huc08_est$local_huc12_ids, "070702010102")

  # outlet_backfill for HUC08 estimate
  expect_equal(plan$huc08_est$outlet_backfill$outlet_huc10s, "0707020101")
  expect_equal(plan$huc08_est$outlet_backfill$outlet_huc12_ids, "070702010101")
})

test_that("plan_upstream_huc12_fetches single ID equals outlet", {
  plan <- nhdplusTools:::plan_upstream_huc12_fetches(
    "070702010101", "070702010101"
  )

  expect_equal(plan$huc12_est$fetch_ids, "070702010101")
  expect_true(plan$huc10_est$same_as_huc12)
  expect_null(plan$huc10_est$outlet_backfill)
  expect_true(plan$huc08_est$same_as_huc10)
  expect_null(plan$huc08_est$outlet_backfill)
})

test_that("plan_upstream_huc12_fetches multiple outlets same HUC10", {
  ids <- c("070702010101", "070702010102", "070702010103", "070702010501")
  outlets <- c("070702010101", "070702010102")

  plan <- nhdplusTools:::plan_upstream_huc12_fetches(ids, outlets)

  expect_equal(plan$huc12_est$fetch_ids, ids)
  # Both outlets in HUC10 0707020101. One upstream HUC10 0707020105.
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, "0707020105")
  expect_equal(plan$huc10_est$local_huc12_ids, "070702010103")

  # outlet_backfill has both outlets
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc10s, "0707020101")
  expect_equal(sort(plan$huc10_est$outlet_backfill$outlet_huc12_ids),
    sort(c("070702010101", "070702010102")))
})

test_that("plan_upstream_huc12_fetches multiple outlets different HUC10s", {
  ids <- c("070702010101", "070702010501", "070702010502", "070703020301")
  outlets <- c("070702010101", "070702010501")

  plan <- nhdplusTools:::plan_upstream_huc12_fetches(ids, outlets)

  # outlet HUC10s: 0707020101, 0707020105
  # upstream HUC10: 0707030203
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, "0707030203")
  expect_equal(plan$huc10_est$local_huc12_ids, "070702010502")

  # outlet_backfill has both outlet HUC10s
  expect_equal(sort(plan$huc10_est$outlet_backfill$outlet_huc10s),
    sort(c("0707020101", "0707020105")))
  expect_equal(sort(plan$huc10_est$outlet_backfill$outlet_huc12_ids),
    sort(c("070702010101", "070702010501")))
})

test_that("plan_upstream_huc12_fetches multiple outlets different HUC08s", {
  ids <- c("070702010101", "070703020301", "070703020302", "070801010101")
  outlets <- c("070702010101", "070703020301")

  plan <- nhdplusTools:::plan_upstream_huc12_fetches(ids, outlets)

  # outlet HUC08s: 07070201, 07070302
  # upstream HUC08: 07080101
  expect_false(plan$huc08_est$same_as_huc10)
  expect_equal(plan$huc08_est$bulk_huc08s, "07080101")
  expect_equal(plan$huc08_est$local_huc12_ids, "070703020302")

  # outlet_backfill for HUC08 estimate
  expect_equal(sort(plan$huc08_est$outlet_backfill$outlet_huc10s),
    sort(c("0707020101", "0707030203")))
  expect_equal(sort(plan$huc08_est$outlet_backfill$outlet_huc12_ids),
    sort(c("070702010101", "070703020301")))
})

test_that("plan_upstream_huc12_fetches multiple outlets all same HUC10", {
  ids <- c("070702010101", "070702010102", "070702010103")
  outlets <- c("070702010101", "070702010102")

  plan <- nhdplusTools:::plan_upstream_huc12_fetches(ids, outlets)

  expect_true(plan$huc10_est$same_as_huc12)
  expect_null(plan$huc10_est$outlet_backfill)
  expect_true(plan$huc08_est$same_as_huc10)
  expect_null(plan$huc08_est$outlet_backfill)
})

test_that("plan_upstream_huc12_fetches backfill for missing huc12pp", {
  # Scenario matching the real edge case: outlet HUC12 171200010710.
  # HUC12 171200010707 is in the same HUC10 (1712000107) and upstream
  # by sort order, but has no huc12pp pour point.
  # The plan should carry outlet_backfill info so fetch_upstream_huc12s

  # can query all HUC12s in HUC10 1712000107 and filter by sort order.
  ids <- c("171200010701", "171200010703", "171200010710",
    "171200010501")
  outlet <- "171200010710"

  plan <- nhdplusTools:::plan_upstream_huc12_fetches(ids, outlet)

  # multi-HUC10: HUC10s 1712000107 (outlet) and 1712000105
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, "1712000105")

  # outlet_backfill present with correct values
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc10s, "1712000107")
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc12_ids, "171200010710")
})

test_that("get_drainage_area_estimates Black Earth Creek smoke test", {
  skip_on_cran()

  start <- list(featureSource = "nwissite", featureID = "USGS-05406500")

  result <- get_drainage_area_estimates(start)

  # check return structure
  expect_type(result, "list")
  expected_names <- c(
    "da_huc12_sqkm", "da_huc10_sqkm", "da_huc08_sqkm",
    "contrib_da_huc12_sqkm", "contrib_da_huc10_sqkm",
    "contrib_da_huc08_sqkm",
    "network_da_sqkm", "start_feature",
    "hu12_by_huc12", "hu12_by_huc10", "hu12_by_huc08",
    "extra_catchments", "split_catchment",
    "all_network", "all_catchments", "hu12_outlet"
  )
  expect_true(all(expected_names %in% names(result)))

  # catchments should be NULL when not requested
  expect_null(result$all_catchments)

  # HUC12-level DA should be positive and close to network DA
  expect_true(result$da_huc12_sqkm > 0)
  expect_true(result$network_da_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm <= result$da_huc12_sqkm)

  # single HUC10 basin so huc10 and huc08 estimates should be NA
  expect_true(is.na(result$da_huc10_sqkm))
  expect_true(is.na(result$da_huc08_sqkm))
  expect_true(is.na(result$contrib_da_huc10_sqkm))
  expect_true(is.na(result$contrib_da_huc08_sqkm))
  expect_null(result$hu12_by_huc10)
  expect_null(result$hu12_by_huc08)

  # spatial outputs should be sf
  expect_s3_class(result$hu12_by_huc12, "sf")
  expect_s3_class(result$extra_catchments, "sf")
  expect_s3_class(result$split_catchment, "sf")
})

test_that("get_drainage_area_estimates Lake Mendota multi-outlet smoke test", {
  skip_on_cran()

  start <- list(featureSource = "nwissite", featureID = "USGS-05428000")

  result <- get_drainage_area_estimates(start)

  # check return structure
  expect_type(result, "list")
  expected_names <- c(
    "da_huc12_sqkm", "da_huc10_sqkm", "da_huc08_sqkm",
    "contrib_da_huc12_sqkm", "contrib_da_huc10_sqkm",
    "contrib_da_huc08_sqkm",
    "network_da_sqkm", "start_feature",
    "hu12_by_huc12", "hu12_by_huc10", "hu12_by_huc08",
    "extra_catchments", "split_catchment",
    "all_network", "all_catchments", "hu12_outlet"
  )
  expect_true(all(expected_names %in% names(result)))

  # DA should be positive
  expect_true(result$da_huc12_sqkm > 0)
  expect_true(result$network_da_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm <= result$da_huc12_sqkm)

  # split_catchment should have rows for multiple outlets (2 rows each)
  expect_true(nrow(result$split_catchment) >= 4)

  # spatial outputs should be sf
  expect_s3_class(result$hu12_by_huc12, "sf")
  expect_s3_class(result$split_catchment, "sf")
})
