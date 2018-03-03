context("collapse_flowlines")

test_that("collapse flowlines works as expected", {
  flines <- readRDS("data/baltimore_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 20, 1))
  flines <- collapse_flowlines(flines, 1)
  ex_flines <- readRDS("data/baltimore_collapsed.rds")
  expect_equal(flines, ex_flines)
})

test_that("collapse flowlines works with small networks", {
  flines <- readRDS("data/smallish_networks.rds")
  flines_collapse <- collapse_flowlines(flines, 2)
  expect_equal(length(which(flines_collapse$joined_fromCOMID == -9999)),
               3)
})

test_that("collapse flowlines works as expected with add category", {
  flines <- readRDS("data/baltimore_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 20, 1))
  flines <- collapse_flowlines(flines, 1, add_category = TRUE)
  expect_equal(names(flines)[7], "join_category")
})

test_that("collapse flowlines works as expected with mainstem thresh", {
  flines <- readRDS("data/baltimore_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 20, 1))
  expect_equal_to_reference(collapse_flowlines(flines, .5, add_category = TRUE, mainstem_thresh = 1),
                            "data/baltimore_collapsed_mainthresh.rds", update = FALSE)
})
