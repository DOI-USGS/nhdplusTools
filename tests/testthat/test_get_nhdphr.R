test_that("get_nhdphr", {

  skip_on_cran()

  expect_error(get_nhdphr(reachcode = "01234", type = "test"),
               "not defined")

  expect_error(get_nhdphr(reachcode = "01234", type = "networknhdflowline",
                        ids = "1"),
               "can not specify both")

  f <- get_nhdphr(ids = "50001000103671",
                  type = "networknhdflowline")

  expect_equal(f$nhdplusid, 50001000103671)

  expect_s3_class(f, "sf")

  # skip("performance, need to mock?")
  # this test was mostly a stub I think -- it is slow
  # f2 <- get_nhdphr(reachcode = f$reachcode,
  #                  type = "networknhdflowline")

})
