test_that("get_3dhp", {

  skip_on_cran()

  # 3dhp types get returned
  expect_type(get_3dhp(), "list")
  expect_message(get_3dhp(), "type")

  expect_error(get_3dhp(universalreferenceid = "01234", type = "test"),
               "only be specified for hydrolocation")

  expect_error(get_3dhp(universalreferenceid = "01234", type = "hydrolocation",
                        ids = "1"),
               "can not specify both")

  skip_on_cran()

  ms <- get_3dhp(ids = "https://geoconnex.us/ref/mainstems/377002",
                 type = "flowline")

  expect_equal(unique(ms$mainstemid), "https://geoconnex.us/ref/mainstems/377002")

  expect_s3_class(ms, "sf")

  suppressWarnings({
    hl <- get_3dhp(ids = "https://geoconnex.us/ref/mainstems/377002",
                   type = "hydrolocation - reach code, external connection")
  })
  expect_equal(unique(hl$mainstemid), "https://geoconnex.us/ref/mainstems/377002")
})
