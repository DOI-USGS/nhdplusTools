context("get_DM")

pt_data <- sf::read_sf(system.file("extdata/petapsco_flowlines.gpkg",
                                   package = "nhdplusTools"))

test_that("get_DM works normal", {
  result <- get_DM(pt_data, 11689050)
  expect_equal(length(result), 26)
})

test_that("get_DM works short", {
  result <- get_DM(pt_data, 11690570)
  expect_equal(length(result), 6)
})

test_that("get_DM works for no divergence", {
  result <- get_DM(pt_data, 11688810)
  expect_true(!11688828 %in% result)
  expect_equal(length(result), 35)
})

test_that("get_DM works upstream of diversion", {
  result <- get_DM(pt_data, 11689280)
  expect_true(!11689758 %in% result)
  expect_true(11689286 %in% result)
  expect_equal(length(result), 29)
})

test_that("get_DM with distance 0 returns 1 comid", {
  result <- get_DM(pt_data,
                   11688810, distance = 0)
  expect_equal(length(result), 1)
})

test_that("get_DM with distance 2 returns specific COMIDs", {
  result <- get_DM(pt_data,
                   11688810, distance = 2)
  expect_equal(length(result), 3)
  expect_true(all(c(11688810, 11688826, 11688884) %in% result))
})

test_that("get_DM with distance big returns specific same as no distance", {
  result <- get_DM(pt_data,
                   11688810, distance = 100000)
  result2 <- get_DM(pt_data, 11688810)
  expect_equal(result,  result2)
})

test_that("get_DM works upstream of diversion", {
  result <- get_DM(pt_data, 11689280)
  expect_true(!11689758 %in% result)
  expect_true(11689286 %in% result)
  expect_equal(length(result), 29)
})

context("get_UM")

test_that("get_UM works short", {
  result <- get_UM(pt_data, 11689050)
  expect_equal(length(result), 18)
})

test_that("get_UM works long", {
  result <- get_UM(pt_data, 11690570)
  expect_equal(length(result), 80)
})

test_that("get_UM returns 1 for distance 0", {
  result <- get_UM(pt_data,
                   11690570, distance = 0)
  expect_equal(length(result), 1)
})

test_that("get_UM returns a certain length for given distance", {
  result <- get_UM(pt_data,
                   11690570, distance = 10)
  expect_equal(length(result), 12)
})

context("get_UT")

test_that("get_UT works", {
  result <- get_UT(pt_data, 11687180)
  expect_equal(length(result), 5)
  result <- get_UT(pt_data, 11687224)
  expect_equal(length(result), 7)
})

test_that("get_UT works with distance", {
  result <- get_UT(pt_data,
                   11689276, distance = 0)
  expect_equal(result, 11689276)
  result <- get_UT(pt_data,
                   11689276, distance = 3)
  expect_equal(length(result), 6)
})

test_that("get_UT works with distance specific", {
  result <- get_UT(pt_data, 11687180, 2)
  expect_equal(length(result), 3)
})

test_that("get_UT returns diverted paths.", {
  result <- get_UT(pt_data, 11690184)
  expect_true(all(c(11689276, 11690200) %in% result),
         "missing a diverted or main path")
})


context("get_DD")

test_that("get_DD works with two divergences", {
  result <- get_DD(pt_data, 11689316)
  expect_true(all(c(11689294, 11690224, 11689268, 11689758, 11689276) %in% result))

})

test_that("get_DD works", {
  result <- get_DD(pt_data, 11688810)
  expect_true(11688810 %in% result)
  expect_true(11688828 %in% result)
  expect_equal(length(result), 36)
})

test_that("get_DD works upstream of diversion", {
  result <- get_DD(pt_data, 11689280)
  expect_true(11689758 %in% result)
  expect_true(11689286 %in% result)
  expect_equal(length(result), 33)
})

test_that("get_DD with distance 0.2 returns 3", {
  result <- get_DD(pt_data,
                   11688810, distance = 0.5)
  expect_equal(length(result), 3)
})

test_that("get_DD with distance 2 returns 4 specific", {
  result <- get_DD(pt_data,
                   11688810, distance = 2)
  expect_equal(length(result), 4)
  expect_true(all(c(11688810, 11688826, 11688828, 11688884) %in% result))
})

test_that("get_DM works if missing the outlet", {
  pt_data_borkd <- dplyr::filter(pt_data, TerminalFl == 0)
  result <- get_DM(pt_data_borkd, 11688810)
  expect_equal(length(result), 34)

})
