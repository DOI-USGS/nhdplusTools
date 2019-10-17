context("align_nhdplus_names")

source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
names(new_hope_flowline) <- tolower(names(new_hope_flowline))

test_that("broken names dont work with get_UM", {
  expect_error(get_DM(new_hope_flowline, 8893770))
})

test_that("aligned names work", {
  aligned = align_nhdplus_names(new_hope_flowline)
  result <- get_DM(aligned, 8893770)
  expect_equal(length(result), 13)
})
