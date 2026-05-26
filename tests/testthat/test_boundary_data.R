test_that("data", {
  expect_equal(nrow(hydrogeofetch::vpu_boundaries), 23)
  expect_equal(nrow(hydrogeofetch::rpu_boundaries), 70)


  expect_true(inherits(hydrogeofetch::vpu_boundaries, 'sf'))
  expect_true(inherits(hydrogeofetch::rpu_boundaries, 'sf'))
})
