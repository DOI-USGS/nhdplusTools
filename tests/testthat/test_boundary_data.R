test_that("data", {
  expect_equal(nrow(nhdplusTools::vpu_boundaries), 23)
  expect_equal(nrow(nhdplusTools::rpu_boundaries), 70)


  expect_true(inherits(nhdplusTools::vpu_boundaries, 'sf'))
  expect_true(inherits(nhdplusTools::rpu_boundaries, 'sf'))
})
