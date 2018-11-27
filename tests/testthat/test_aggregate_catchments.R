context("aggregate catchment")
test_that("walker aggregate runs", {
source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

get_id <- function(mc) {
  ind <- match(mc, walker_catchment_rec$member_COMID)
  walker_catchment_rec$ID[ind]
}

outlets <- data.frame(ID = get_id(c("5329843", "5329339.1", "5329385", "5329303")),
                      type = c("outlet", "outlet", "outlet", "terminal"),
                      stringsAsFactors = FALSE)

aggregated <- aggregate_catchments(walker_fline_rec, walker_catchment_rec, outlets)
aggregated_fline <- aggregated$fline_sets
aggregated_cat <- aggregated$cat_sets

expect_equal(aggregated_cat$ID, get_id(c("5329385", "5329843", "5329339.1", "5329303")))
expect_equal(aggregated_fline$ID, get_id(c("5329385", "5329843", "5329339.1", "5329303")))
expect(aggregated_cat$ID[1] %in% aggregated_cat$set[[1]], "outlet ids should be in the result")
expect(length(aggregated_cat$set[[2]]) == 5, "got the wrong number in catchment set")
expect(!5 %in% aggregated_cat$set[[2]], "an upstream outlet should not be in another set")

expect(length(aggregated_fline$set[[2]] == 3), "got the wrong number of flowpaths")

expect_equal(aggregated_cat$toID, get_id(c("5329843", "5329339.1", "5329303", NA)), info = "Expect these toIDs")
expect(all(aggregated_cat$toID[!is.na(aggregated_cat$toID)] %in% aggregated_cat$ID),
       "All not NA toIDs should be in IDs")

outlets <- data.frame(ID = get_id(c("5329843", "5329339.1", "5329385", "5329303", "5329321")),
                      type = c("outlet", "outlet", "outlet", "terminal", "outlet"),
                      stringsAsFactors = FALSE)

aggregated <- aggregate_catchments(walker_fline_rec, walker_catchment_rec, outlets)
aggregated_fline <- aggregated$fline_sets
aggregated_cat <- aggregated$cat_sets

expect_equal(aggregated_cat$ID, get_id(c("5329321", "5329385", "5329313", "5329843", "5329339.1", "5329339.3",
                                         "5329303")))
expect(length(aggregated_cat$set[[1]]) == 5, "got the wrong number in catchment set")

outlets <- data.frame(ID = get_id(c("5329363", "5329303")),
                      type = c("outlet", "terminal"),
                      stringsAsFactors = FALSE)

aggregated <- aggregate_catchments(walker_fline_rec, walker_catchment_rec, outlets)
aggregated_fline <- aggregated$fline_sets
aggregated_cat <- aggregated$cat_sets

expect(!any(aggregated_cat$set[[1]] == get_id("5329373")), "shouldn't have a parallel stem in the set")

outlets <- data.frame(ID = get_id(c("5329293", "5329303")),
                      type = c("outlet", "terminal"),
                      stringsAsFactors = FALSE)

aggregated <- aggregate_catchments(walker_fline_rec, walker_catchment_rec, outlets)
aggregated_fline <- aggregated$fline_sets
aggregated_cat <- aggregated$cat_sets

expect(length(aggregated_cat$set[[1]]) == 101, "got the wrong number in catchment set")

# nolint start
# sf::write_sf(aggregated$cat_sets, "walker_collapse.gpkg", "boundary")
# sf::write_sf(aggregated$fline_sets, "walker_collapse.gpkg", "flowpath")
# nolint end
})

test_that("new_hope aggregate", {
  source(system.file("extdata", "new_hope_data.R", package = "nhdplusTools"))

  get_id <- function(mc) {
    ind <- match(mc, new_hope_catchment_rec$member_COMID)
    new_hope_catchment_rec$ID[ind]
  }

  # # From manual testing with NHDPlus Gage layer.
  # outlets <- data.frame(ID = c(162L, 153L, 155L, 59L, 17L, 118L, 398L, 399L, 400L, 135L,
  #                              268L, 6L, 365L, 366L, 39L, 102L, 35L, 362L, 335L),
  #                       type = c("outlet", "outlet", "outlet", "outlet", "outlet",
  #                                "outlet", "outlet", "outlet", "outlet", "outlet", "outlet",
  #                                "outlet", "outlet", "outlet", "outlet", "outlet", "outlet",
  #                                "outlet", "terminal"))
  #
  # aggregated <- aggregate_catchments(new_hope_fline_rec, new_hope_catchment_rec, outlets)

  outlets <- data.frame(ID = get_id(c("8896032.1", "8896032.2", "8896032.3", "8894360,8897784")),
                        type = c("outlet", "outlet", "outlet", "terminal"),
                        stringsAsFactors = FALSE)

  aggregated <- aggregate_catchments(new_hope_fline_rec, new_hope_catchment_rec, outlets)

  fline_sets <- aggregated$fline_sets
  cat_sets <- aggregated$cat_sets

  expect(fline_sets$ID[1] == fline_sets$set[[1]][1],
         "A small headwater that was a divergence should show up as such")

  expect(all(fline_sets$ID %in% cat_sets$ID), "flines and cats should have the same ids")

  expect(all(!fline_sets$set[fline_sets$ID == get_id("8894360,8897784")][[1]] %in% fline_sets$set[fline_sets$ID == get_id("8896032.1")][[1]]),
         "a downstream catchment should not contain flowpaths from upstream catchments")

  expect(all(!fline_sets$set[fline_sets$ID == get_id("8893780.2,8894326")][[1]] %in% fline_sets$set[fline_sets$ID == get_id("8896032.1")][[1]]),
         "a downstream catchment should not contain flowpaths from upstream catchments")

  expect(all(!fline_sets$set[fline_sets$ID == get_id("8896032.2")][[1]] %in% fline_sets$set[fline_sets$ID == get_id("8896032.1")][[1]]),
         "a downstream catchment should not contain flowpaths from upstream catchments")

  new_hope_catchment_rec$area_sqkm <- as.numeric(st_area(new_hope_catchment_rec)) / (1000^2)
  new_hope_fline_rec <- dplyr::inner_join(new_hope_fline_rec,
                                   select(st_set_geometry(new_hope_catchment_rec, NULL),
                                          ID, area_sqkm), by = "ID")
  new_hope_fline_rec$TotDASqKM <-
    calculate_total_drainage_area(rename(st_set_geometry(new_hope_fline_rec, NULL),
                                         area = area_sqkm))

  aggregated <- aggregate_catchments(new_hope_fline_rec, new_hope_catchment_rec, outlets,
                                     da_thresh = 2, only_larger = TRUE)

  fline_sets_2 <- aggregated$fline_sets
  cat_sets_2 <- aggregated$cat_sets

  expect(nrow(fline_sets_2) == 6, "Should have six catchments in the output")

  expect(!any(get_id(c("8893788,8893784", "8894184,8894448")) %in% fline_sets_2$ID), "Shouldn't have a couple small catchments in output.")
  # nolint start
  # sf::write_sf(aggregated$cat_sets, "new_hope_collapse.gpkg", "boundary")
  # sf::write_sf(aggregated$fline_sets, "new_hope_collapse.gpkg", "flowpath")
  # nolint end


})

test_that("new_hope aggregate", {
  source(system.file("extdata", "new_hope_data.R", package = "nhdplusTools"))

  get_id <- function(mc) {
    ind <- match(mc, new_hope_catchment_rec$member_COMID)
    new_hope_catchment_rec$ID[ind]
  }

  new_hope_catchment_rec$area_sqkm <- as.numeric(st_area(new_hope_catchment_rec)) / (1000^2)
  new_hope_fline_rec <- dplyr::inner_join(new_hope_fline_rec,
                                          select(st_set_geometry(new_hope_catchment_rec, NULL),
                                                 ID, area_sqkm), by = "ID")
  new_hope_fline_rec$TotDASqKM <-
    calculate_total_drainage_area(rename(st_set_geometry(new_hope_fline_rec, NULL),
                                         area = area_sqkm))

  # HU12 FPP st_joined to get these
  outlets <- data.frame(ID = get_id(c("8894358", "8894344.2", "8893780.2,8894326",
                               "8895792", "8894336,8894342",
                               "8894154.2", "8894142.1", "8894360,8897784")),
                        type = c("outlet", "outlet", "outlet", "outlet",
                                 "outlet", "outlet", "outlet", "terminal"),
                        stringsAsFactors = FALSE)

  aggregated <- aggregate_catchments(new_hope_fline_rec, new_hope_catchment_rec, outlets)

  fline_sets <- aggregated$fline_sets
  cat_sets <- aggregated$cat_sets

  expect(length(which(sapply(fline_sets$set, function(x) get_id("8893342") %in% x))) == 1,
         "A connector flowpath should be added downstream of an upper hu.")

  outlets <- data.frame(ID = get_id(c("8895638", "8894360,8897784")),
                        type = c("outlet", "terminal"),
                        stringsAsFactors = FALSE)

  aggregated <- aggregate_catchments(new_hope_fline_rec, new_hope_catchment_rec, outlets,
                                     only_larger = FALSE)

  expect(get_id("8893780.2,8894326") %in% aggregated$cat_sets$ID,
         "expect catchment downstream of outlet where levelpath changes to be in output")
  expect(all(get_id(c("8896032.4,8896016", "8896054", "8895888.2,8897468")) %in% aggregated$cat_sets$ID),
         "expect contributing to the same nexus as another specified outlet")

  expect(length(aggregated$cat_sets$ID) == 11, "Expect 11 output catchments")
  # nolint start
  # sf::write_sf(aggregated$cat_sets, "new_hope_collapse.gpkg", "boundary")
  # sf::write_sf(aggregated$fline_sets, "new_hope_collapse.gpkg", "flowpath")
  # nolint end
})
