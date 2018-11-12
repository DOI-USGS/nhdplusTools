context("nexus combine")
test_that("walker combine runs", {
source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

outlets <- data.frame(ID = c(31, 3, 5, 1),
                      type = c("o", "o", "o", "t"),
                      stringsAsFactors = FALSE)
# fline_rec <- walker_fline_rec
# cat_rec <- walker_catchment_rec

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets)

expect_equal(collapsed$ID, c(5, 31, 3, 1))
expect(length(collapsed$set[[2]]) == 5, "got the wrong number in catchment set")

outlets <- data.frame(ID = c(31, 3, 5, 1, 12),
                      type = c("o", "o", "o", "t", "i"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets)

expect_equal(collapsed$ID, c(12, 5, 31, 3, 1))
expect(length(collapsed$set[[1]]) == 7, "got the wrong number in catchment set")
expect(!(any(collapsed$set[[1]] == 12)), "shouldn't have inlet catchment in set")

outlets <- data.frame(ID = c(14, 1),
                      type = c("o", "t"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets)

expect(!any(collapsed$set[[1]] == 4), "shouldn't have a parallel stem in the set")

outlets <- data.frame(ID = c(2, 1),
                      type = c("i", "t"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets)

expect(length(collapsed$set[[1]]) == 100, "got the wrong number in catchment set")
})
