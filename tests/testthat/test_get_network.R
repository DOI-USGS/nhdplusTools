

source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

pt_data <- sample_flines %>%
  align_nhdplus_names()

cida <- sf::read_sf(list.files(pattern = "cida_flowlines.gpkg", full.names = TRUE, recursive = TRUE)) %>%
  align_nhdplus_names()

test_that("get_DM works normal", {
  result <- get_DM(pt_data, 11689050)
  result2 <- get_DM(pt_data, 11689050, include = FALSE)
  expect_equal(length(result), 26)
  expect_equal(length(result2), 25)
})

test_that("get_DM works short", {
  result <- get_DM(pt_data, 11690570)
  result2 <- get_DM(pt_data, 11690570, include = FALSE)
  expect_equal(length(result), 6)
  expect_equal(length(result2), 5)
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
  result <- get_DM(pt_data, 11688810, distance = 0)
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

test_that("get_DM sorts (eg returns different order)", {
  result  <- get_DM(pt_data, 11689280)
  result2 <- get_DM(pt_data, 11689280, sort = TRUE)
  expect_equal(length(result), length(result2))
  expect_true(sum(result == result2) < length(result))
})

test_that("get_DM sorts correctly", {

  comid = cida$COMID[33]
  result <- get_DM(cida, comid, sort = TRUE)

  sort_order = c(8585070, 8585080, 8585102, 8585124, 8585128, 8585130,
                    8585142, 8585160, 8585158, 8585178, 8585176, 8585910, 8585922,
                    8585966, 8585976, 8585978, 8585986, 8585980, 8585972, 8585908,
                    8585902, 8585836, 8585840, 8585810, 8585796, 8585800)

  expect_true(sum(result == sort_order) == length(result))
})

test_that("get_DM sorts correctly with distance parameter", {

  result <- get_DM(cida, 8585070, sort = TRUE, distance = 5)

  sort_order = c(8585070, 8585080, 8585102, 8585124, 8585128, 8585130,
                    8585142, 8585160, 8585158, 8585178, 8585176, 8585910, 8585922,
                    8585966, 8585976, 8585978, 8585986, 8585980, 8585972, 8585908,
                    8585902, 8585836, 8585840, 8585810, 8585796, 8585800)

  expect_true(sum(result == sort_order[1:length(result)]) == length(result))
})




test_that("get_UM works short", {
  result <- get_UM(pt_data, 11689050)
  result2 <- get_UM(pt_data, 11689050, include = FALSE)
  expect_equal(length(result), 18)
  expect_equal(length(result2), 17)
})

test_that("get_UM works long", {
  result <- get_UM(pt_data, 11690570)
  result2 <- get_UM(pt_data, 11690570, include = FALSE)
  expect_equal(length(result), 80)
  expect_equal(length(result2), 79)
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




test_that("get_UM sorts (eg returns different order)", {
  result <- get_UM(pt_data, 11689280)
  result2 <- get_UM(pt_data, 11689280, sort = TRUE)
  expect_equal(length(result), length(result2))
  expect_true(sum(result == result2) < length(result))
})


test_that("get_UM sorts correctly", {

  result <- get_UM(cida, 8585070, sort = TRUE)
  correct_order = c(8585070, 8585022, 8585002, 8584996, 8584888)
  expect_true(sum(result == correct_order) == length(result))
})

test_that("get_UM sorts correctly with distance parameter", {
  comid = cida$COMID[33]
  result <- get_DM(cida, comid, sort = TRUE, distance = 1)
  correct_order = c(8585070, 8585022, 8585002, 8584996, 8584888)
  expect_true(sum(result == correct_order[1:length(result)]) == length(result))
})






test_that("get_UT works", {
  result <- get_UT(pt_data, 11687180)
  expect_equal(length(result), 5)
  result <- get_UT(pt_data, 11687224)
  expect_equal(length(result), 7)

  test_error <- rbind(pt_data, pt_data)

  expect_error(get_UT(test_error, 11687180),
               "Found duplicate ID for starting catchment. Duplicate rows in network?")
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


test_that("get_DM sorts (eg returns different order)", {
  result <- get_DM(pt_data, 11689280)
  result2 <- get_DM(pt_data, 11689280, sort = TRUE)
  expect_equal(length(result), length(result2))
  expect_true(sum(result == result2) < length(result))
})

test_that("get_DM sorts correctly", {
  comid = pt_data$COMID[33]
  result <- get_DM(pt_data, comid, sort = TRUE)
  correct_order = c(c(11690560, 11690226, 11690556, 11690558, 11690604, 11690566,
                      11690602, 11690246, 11690538, 11690564, 11690570, 11690256,
                      11690258, 11690568, 11690262, 11690260))
  expect_true(sum(result == correct_order) == length(result))
})

test_that("get_DM sorts correctly with distance parameter", {
  comid = pt_data$COMID[33]
  result <- get_DM(pt_data, comid, sort = TRUE, distance = 5)

  correct_order = c(c(11690560, 11690226, 11690556, 11690558, 11690604, 11690566,
                      11690602, 11690246, 11690538, 11690564, 11690570, 11690256,
                      11690258, 11690568, 11690262, 11690260))

  expect_true(sum(result == correct_order[1:length(result)]) == length(result))
})



