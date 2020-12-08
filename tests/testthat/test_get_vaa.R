context("get vaa")

test_that("vaa examples", {
  skip_on_cran()

  vaa_names <- get_vaa_names()

  expect_is(vaa_names, "character")

  vaa_path <- get_vaa_path()

  expect_is(vaa_path, "character")

  vaa <- get_vaa()

  expect_is(vaa, "data.frame")

  expect_message(empty <- get_vaa(atts = "baddies"), "baddies not in vaa data. Ignoring...")

  expect_equal(nrow(empty), 2691339)

  expect_message(vaa_path_2 <- download_vaa(), "File already cached")

  expect_equal(vaa_path, vaa_path_2)
})
