context("plot tests")

test_that("basics work", {
  site <- "USGS-05428500"
  d <-  nhdplusTools:::get_plot_data(site)
  expect_equal(names(d), c("plot_bbox", "sites", "flowline", "basin"))
})
