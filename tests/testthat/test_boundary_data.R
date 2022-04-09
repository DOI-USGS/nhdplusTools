test_that("data", {
  expect_equal(nrow(nhdplusTools::vpu), 23)
  expect_equal(nrow(nhdplusTools::rpu), 70)


  expect_true(inherits(nhdplusTools::vpu, 'sf'))
  expect_true(inherits(nhdplusTools::rpu, 'sf'))
})
