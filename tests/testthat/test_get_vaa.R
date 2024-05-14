

test_that("vaa examples", {
  skip_on_cran()
  skip_on_ci()

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

test_that("catchment chars", {

  skip_on_cran()
  skip_on_os("linux")
  skip_on_os("mac")

  httptest::without_internet({
    suppressMessages(expect_warning(w <- get_characteristics_metadata(cache = FALSE)))
    expect_null(w)
  })

  meta <- nhdplusTools::get_characteristics_metadata(cache = FALSE)

  expect_true(inherits(meta, "data.frame"))
  expect_true(nrow(meta) > 1000)

  meta <- nhdplusTools::get_characteristics_metadata()

  expect_true(inherits(meta, "data.frame"))
  expect_true(nrow(meta) > 1000)

  meta <- nhdplusTools::get_characteristics_metadata("BFI")
  expect_equal(nrow(meta), 3)

  expect_equal(names(meta), c("ID", "description", "units", "datasetLabel", "datasetURL",
                              "themeLabel", "themeURL", "watershedType",
                              "sbid", "end", "s3_url", "http_url"))

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  dat <- get_catchment_characteristics(c("CAT_BFI", "ACC_BFI", "TOT_BFI"), walker_catchment$FEATUREID)

  expect_equal(names(dat), c("characteristic_id", "comid",
                             "characteristic_value", "percent_nodata"))

  expect_true(all(c("CAT_BFI", "ACC_BFI", "TOT_BFI") %in% dat$characteristic_id))

  expect_true(all(walker_catchment$FEATUREID %in% dat$comid))

  expect_warning(w <- get_catchment_characteristics("CAT_CFI",
                                                    walker_catchment$FEATUREID),
                 "Variable CAT_CFI not found in metadata.")
  expect_null(w)

})
