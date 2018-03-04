context("collapse_flowlines")

test_that("collapse flowlines works as expected", {
  flines <- readRDS("data/petapsco_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 20, 1))
  flines_out <- collapse_flowlines(flines, 1)

  # problem headwater
  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 11687206)] == -9999)

  # confluence join worked
  expect(flines_out$joined_fromCOMID[which(flines_out$COMID == 11687234)] == 11687224)

  expect(flines_out$toCOMID[which(flines_out$COMID == 11687226)] == 11687358 &
           flines_out$toCOMID[which(flines_out$COMID == 11687224)] == 11687358)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 11687226)] ==
           flines$LENGTHKM[which(flines$COMID == 11687226)] + flines$LENGTHKM[which(flines$COMID == 11687234)])

  # mainstem join worked

  expect(flines_out$toCOMID[which(flines_out$COMID == 11687548)] == 11689978)

  expect(flines_out$joined_fromCOMID[which(flines_out$COMID == 11689928)] == 11687548 &
           flines_out$joined_fromCOMID[which(flines_out$COMID == 11690532)] == 11687548)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 11687548)] ==
           (flines$LENGTHKM[which(flines_out$COMID == 11687548)] +
              flines$LENGTHKM[which(flines_out$COMID == 11689928)] +
              flines$LENGTHKM[which(flines_out$COMID == 11690532)]))

  # outlet worked

  expect(is.na(flines_out$toCOMID[which(flines_out$COMID == 11690256)]))

  expect(flines_out$joined_fromCOMID[which(flines_out$COMID == 11690260)] == 11690256)
  expect(flines_out$joined_fromCOMID[which(flines_out$COMID == 11690262)] == 11690256)
  expect(flines_out$joined_fromCOMID[which(flines_out$COMID == 11690568)] == 11690256)
  expect(flines_out$joined_fromCOMID[which(flines_out$COMID == 11690258)] == 11690256)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 11690256)] ==
           (flines$LENGTHKM[which(flines_out$COMID == 11690258)] +
              flines$LENGTHKM[which(flines_out$COMID == 11690568)] +
              flines$LENGTHKM[which(flines_out$COMID == 11690262)] +
              flines$LENGTHKM[which(flines_out$COMID == 11690260)] +
              flines$LENGTHKM[which(flines_out$COMID == 11690256)]))

})

test_that("mainstem collapes works as expected", {
  flines <- readRDS("data/guadalupe_network.rds")
  flines_out <- collapse_flowlines(flines, 0.5, mainstem_thresh = 0.5)

  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 24670381)] == 3839043)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 24670381)] == 0)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 3839043)] ==
           (flines$LENGTHKM[which(flines$COMID == 3839043)] + flines$LENGTHKM[which(flines$COMID == 24670381)]))
})

test_that("collapse flowlines works with small networks", {
  flines <- readRDS("data/smallish_networks.rds")
  flines_collapse <- collapse_flowlines(flines, 2)
  expect_equal(length(which(flines_collapse$joined_fromCOMID == -9999)),
               3)
})

test_that("collapse flowlines works as expected with add category", {
  flines <- readRDS("data/petapsco_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 20, 1))
  flines <- collapse_flowlines(flines, 1, add_category = TRUE)
  expect_equal(names(flines)[7], "join_category")
})

# then go look at problem headwater combinations.
test_that("collapse flowlines works as expected with mainstem thresh", {
  flines <- readRDS("data/petapsco_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 20, 1))
  flines <- collapse_flowlines(flines, .5, add_category = TRUE, mainstem_thresh = 1)

  expect(is.na(flines$joined_fromCOMID[which(flines$COMID == 11689092)]))

  expect(flines$toCOMID[which(flines$COMID == 11688868)] == 11690128)

  expect(flines$joined_fromCOMID[which(flines$COMID == 11690124)] == 11688868)
})
