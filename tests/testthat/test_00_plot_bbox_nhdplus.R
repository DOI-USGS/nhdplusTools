
source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

test_that("bbox", {
  skip_on_cran()

  bbox <- sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
                        xmax = -89.24681, ymax = 43.17192),
                      crs = "+proj=longlat +datum=WGS84 +no_defs")

  # With downloaded data
  d <- nhdplusTools:::get_plot_data(bbox = bbox)

  expect_equal(nrow(d$flowline), 183)

  # With Local Data (note this sample is already subset to a watershed basis)
  d <- nhdplusTools:::get_plot_data(bbox = bbox, streamorder = 2,
                                    nhdplus_data = sample_data)

  expect_equal(nrow(d$flowline), 76)

  expect_error(nhdplusTools:::get_plot_data(c(1,2,3), bbox = bbox),
               "Both bbox and outlets not supported.")
})
