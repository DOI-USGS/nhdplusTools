context("align_nhdplus_names")

source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
names(new_hope_flowline) <- tolower(names(new_hope_flowline))

test_that("aligned names work", {
  aligned = align_nhdplus_names(new_hope_flowline)
  expect_true("COMID" %in% names(aligned))
  result <- get_DM(aligned, 8893770)
  expect_equal(length(result), 13)
})
