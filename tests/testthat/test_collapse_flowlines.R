context("collapse_flowlines")

test_that("collapse flowlines works as expected", {
  flines <- readRDS("data/baltimore_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 20, 1))
  expect_equal_to_reference(collapse_flowlines(flines, 1),
                            "data/baltimore_collapsed.rds")
})

test_that("collapse flowlines works with small networks", {
  flines <- readRDS("data/smallish_networks.rds")
  flines_collapse <- collapse_flowlines(flines, 2)
  expect_equal(length(which(flines_collapse$joined_fromCOMID == -9999)),
               3)
})
