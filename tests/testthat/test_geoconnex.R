test_that("discover", {

  expect_s3_class(discover_geoconnex_reference(), "data.frame")

  expect_equal(names(discover_geoconnex_reference()),
               c("id", "title", "description", "url", "attribute", "type"))

})

test_that("get", {
  expect_warning(avail <- get_geoconnex_reference())

  expect_equal(avail, discover_geoconnex_reference())

  AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
                                     xmax = -89.24681, ymax = 43.17192),
                                   crs = "+proj=longlat +datum=WGS84 +no_defs"))

  expect_message(out <- get_geoconnex_reference(AOI, type = "hu04"), "Starting download")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2)

  expect_silent(out <- get_geoconnex_reference(type = "hu04",
                                               ids = c("0707", "0709"),
                                               status = FALSE))

  expect_s3_class(out, "sf")
  expect_true(all(c("0707", "0709") %in% out$id))

  AOI <- sf::st_sfc(sf::st_point(c(-89.56684, 42.99816)),
                    crs = "+proj=longlat +datum=WGS84 +no_defs")

  out <- get_geoconnex_reference(AOI, type = "hu04", buffer = 10000,
                                 t_srs = 5070, status = FALSE)

  expect_true(sf::st_crs(out) == sf::st_crs(5070))
})
