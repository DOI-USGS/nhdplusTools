

test_that("vaa examples", {
  skip_on_cran()

  vaa_names <- get_vaa_names()

  expect_type(vaa_names, "character")

  vaa_path <- get_vaa_path()

  expect_type(vaa_path, "character")

  vaa <- get_vaa()

  expect_s3_class(vaa, "data.frame")

  expect_message(empty <- get_vaa(atts = "baddies"), "baddies not in vaa data. Ignoring...")

  expect_equal(nrow(empty), 2691339)

  expect_message(vaa_path_2 <- download_vaa(), "File already cached")

  expect_equal(vaa_path, vaa_path_2)

  expect_message(update <- get_vaa("reachcode", updated_network = TRUE))

  expect_equal(names(update), c("comid", "reachcode"))

  expect_error(capture_messages(get_vaa("bad", updated_network = TRUE)))

  expect_true("tocomid" %in% get_vaa_names(updated_network = TRUE))
})
