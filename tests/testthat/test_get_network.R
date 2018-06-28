context("get_DM")

test_that("get_DM works normal", {
  result <- get_DM(readRDS("data/petapsco_network.rds"), 11689050)
  expect_equal(length(result),26)
})

test_that("get_DM works short", {
  result <- get_DM(readRDS("data/petapsco_network.rds"), 11690570)
  expect_equal(length(result),6)
})

test_that("get_DM works for no divergence", {
  result <- get_DM(readRDS("data/petapsco_network.rds"),11688810)
  expect(!11688828 %in% result)
  expect_equal(length(result),35)
})

context("get_UM")

test_that("get_UM works short", {
  result <- get_UM(readRDS("data/petapsco_network.rds"), 11689050)
  expect_equal(length(result),18)
})

test_that("get_UM works long", {
  result <- get_UM(readRDS("data/petapsco_network.rds"), 11690570)
  expect_equal(length(result),80)
})

context("get_UT")

test_that("get_UT works", {
  result <- get_UT(readRDS("data/petapsco_network.rds"),11689276)
  expect_equal(length(result),683)
})

context("get_DD")

test_that("get_DD works", {
  result <- get_DD(readRDS("data/petapsco_network.rds"),11688810)
  expect(11688810 %in% result)
  expect(11688828 %in% result)
  expect_equal(length(result),36)
})
