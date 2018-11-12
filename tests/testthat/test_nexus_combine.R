context("nexus combine")
test_that("walker combine runs", {
source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

outlet_cats <- c(1, 31, 3, 5)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlet_cats)

expect_equal(collapsed$ID, c("5", "31", "3", "1"))
expect(length(collapsed$set[[2]][[1]]) == 5, "got the wrong number in catchment set")
})
# nolint start
# plot(collapse_catchments(fline_rec, cat_rec, outlet_cats)$geom, lwd = 3, border = "red")
# plot(cat_rec$geom, lwd = 1.5, border = "green", col = NA, add = TRUE)
# plot(walker_catchment$geom, lwd = 1, add = TRUE)
# plot(walker_flowline$geom, lwd = .7, col = "blue", add = TRUE)
# nolint end
