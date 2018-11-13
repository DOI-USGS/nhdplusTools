context("nexus combine")
test_that("walker combine runs", {
source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

outlets <- data.frame(ID = c(31, 3, 5, 1),
                      type = c("outlet", "outlet", "outlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets, walker_flowline)
collapsed_fline <- collapsed$fline_sets
collapsed_cat <- collapsed$cat_sets

expect_equal(collapsed_cat$ID, c(5, 31, 3, 1))
expect_equal(collapsed_fline$ID, c(5, 31, 3, 1))
expect(collapsed_cat$ID[1] %in% collapsed_cat$set[[1]], "outlet ids should be in the result")
expect(length(collapsed_cat$set[[2]]) == 5, "got the wrong number in catchment set")
expect(!5 %in% collapsed_cat$set[[2]], "an upstream outlet should not be in another set")

expect(length(collapsed_fline$set[[2]] == 3), "got the wrong number of flowlines")

outlets <- data.frame(ID = c(31, 3, 5, 1, 12),
                      type = c("outlet", "outlet", "outlet", "terminal", "inlet"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets, walker_flowline)
collapsed_fline <- collapsed$fline_sets
collapsed_cat <- collapsed$cat_sets

expect_equal(collapsed_cat$ID, c(12, 5, 9, 31, 3, 42, 1))
expect(length(collapsed_cat$set[[1]]) == 7, "got the wrong number in catchment set")
expect(!(any(collapsed_cat$set[[1]] == 12)), "shouldn't have inlet catchment in set")

outlets <- data.frame(ID = c(14, 1),
                      type = c("outlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets, walker_flowline)
collapsed_fline <- collapsed$fline_sets
collapsed_cat <- collapsed$cat_sets

expect(!any(collapsed_cat$set[[1]] == 4), "shouldn't have a parallel stem in the set")

outlets <- data.frame(ID = c(2, 1),
                      type = c("inlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets, walker_flowline)
collapsed_fline <- collapsed$fline_sets
collapsed_cat <- collapsed$cat_sets

expect(length(collapsed_cat$set[[1]]) == 100, "got the wrong number in catchment set")
})
