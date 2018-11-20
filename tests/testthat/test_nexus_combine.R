context("nexus combine")
test_that("walker combine runs", {
source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

outlets <- data.frame(ID = c(31, 40, 5, 1),
                      type = c("outlet", "outlet", "outlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets, walker_flowline)
collapsed_fline <- collapsed$fline_sets
collapsed_cat <- collapsed$cat_sets

expect_equal(collapsed_cat$ID, c(5, 31, 40, 1))
expect_equal(collapsed_fline$ID, c(5, 31, 40, 1))
expect(collapsed_cat$ID[1] %in% collapsed_cat$set[[1]], "outlet ids should be in the result")
expect(length(collapsed_cat$set[[2]]) == 5, "got the wrong number in catchment set")
expect(!5 %in% collapsed_cat$set[[2]], "an upstream outlet should not be in another set")

expect(length(collapsed_fline$set[[2]] == 3), "got the wrong number of flowlines")

outlets <- data.frame(ID = c(31, 40,  5, 1, 23),
                      type = c("outlet", "outlet", "outlet", "terminal", "outlet"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets, walker_flowline)
collapsed_fline <- collapsed$fline_sets
collapsed_cat <- collapsed$cat_sets

expect_equal(collapsed_cat$ID, c(23, 5, 9, 31, 40,  42, 1))
expect(length(collapsed_cat$set[[1]]) == 5, "got the wrong number in catchment set")

outlets <- data.frame(ID = c(14, 1),
                      type = c("outlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets, walker_flowline)
collapsed_fline <- collapsed$fline_sets
collapsed_cat <- collapsed$cat_sets

expect(!any(collapsed_cat$set[[1]] == 4), "shouldn't have a parallel stem in the set")

outlets <- data.frame(ID = c(2, 1),
                      type = c("outlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets, walker_flowline)
collapsed_fline <- collapsed$fline_sets
collapsed_cat <- collapsed$cat_sets

expect(length(collapsed_cat$set[[1]]) == 101, "got the wrong number in catchment set")
})

test_that("new_hope combine", {
  source(system.file("extdata", "new_hope_data.R", package = "nhdplusTools"))

  # From manual testing with NHDPlus Gage layer.
  outlets <- data.frame(ID = c(162L, 153L, 155L, 59L, 17L, 118L, 398L, 399L, 400L, 135L,
                               268L, 6L, 365L, 366L, 39L, 102L, 35L, 362L, 335L),
                        type = c("outlet", "outlet", "outlet", "outlet", "outlet",
                                 "outlet", "outlet", "outlet", "outlet", "outlet", "outlet",
                                 "outlet", "outlet", "outlet", "outlet", "outlet", "outlet",
                                 "outlet", "terminal"))

  collapsed <- collapse_catchments(new_hope_fline_rec, new_hope_catchment_rec, outlets, new_hope_flowline)

  outlets <- data.frame(ID = c(398L, 399L, 400L, 335L),
                        type = c("outlet", "outlet", "outlet", "terminal"))

  collapsed <- collapse_catchments(new_hope_fline_rec, new_hope_catchment_rec, outlets, new_hope_flowline)

  fline_sets <- collapsed$fline_sets
  cat_sets <- collapsed$cat_sets

  expect(fline_sets$ID[1] == fline_sets$set[[1]][1],
         "A small headwater that was a divergence should show up as such")

  expect(all(fline_sets$ID %in% cat_sets$ID), "flines and cats should have the same ids")

  expect(all(!fline_sets$set[fline_sets$ID == 335][[1]] %in% fline_sets$set[fline_sets$ID == 398][[1]]),
         "a downstream catchment should not contain flowlines from upstream catchments")

  expect(all(!fline_sets$set[fline_sets$ID == 342][[1]] %in% fline_sets$set[fline_sets$ID == 398][[1]]),
         "a downstream catchment should not contain flowlines from upstream catchments")

  expect(all(!fline_sets$set[fline_sets$ID == 399][[1]] %in% fline_sets$set[fline_sets$ID == 398][[1]]),
         "a downstream catchment should not contain flowlines from upstream catchments")

  # sf::write_sf(collapsed$cat_sets, "new_hope_collapse.gpkg", "boundary")
  # sf::write_sf(collapsed$fline_sets, "new_hope_collapse.gpkg", "flowpath")
})

test_that("new_hope combine", {
  source(system.file("extdata", "new_hope_data.R", package = "nhdplusTools"))

  # HU12 FPP st_joined to get these
  outlets <- data.frame(ID = c(336, 447, 342, 39, 332, 384, 444, 335),
                        type = c("outlet", "outlet", "outlet", "outlet",
                                 "outlet", "outlet", "outlet", "terminal"),
                        stringsAsFactors = FALSE)

  collapsed <- collapse_catchments(new_hope_fline_rec, new_hope_catchment_rec, outlets, new_hope_flowline)

  fline_sets <- collapsed$fline_sets
  cat_sets <- collapsed$cat_sets

  expect(length(which(sapply(fline_sets$set, function(x) 17 %in% x))) == 1,
         "A connector flowpath should be added downstream of an upper hu.")

  # sf::write_sf(collapsed$cat_sets, "new_hope_collapse.gpkg", "boundary")
  # sf::write_sf(collapsed$fline_sets, "new_hope_collapse.gpkg", "flowpath")
})


