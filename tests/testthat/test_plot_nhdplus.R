context("plot tests")

test_that("basics work", {
  skip_on_cran()
  site <- "USGS-05428500"
  d <-  nhdplusTools:::get_plot_data(site)
  expect_equal(names(d), c("plot_bbox", "sites", "flowline", "basin"))

  p_ready <- nhdplusTools:::gt(d$flowline)
  expect_equal(st_crs(p_ready), st_crs(3857))
  expect_s3_class(p_ready, "sfc_LINESTRING")

  pdf(NULL)
  tempd <- tempdir()
  dir.create(tempd, recursive = TRUE)
  tempf <- file.path(tempd, "temp.png")
  png(file.path(tempd, "temp.png"))
  plot_nhdplus("USGS-05428500")
  dev.off()

  expect_true(file.exists(tempf))
})
