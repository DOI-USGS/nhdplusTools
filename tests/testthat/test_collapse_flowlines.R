context("collapse_flowlines")

test_that("collapse flowlines works as expected", {
  flines <- readRDS("data/petapsco_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 20, 1))
  flines_out <- collapse_flowlines(flines, 1)

  # problem headwater
  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 11687206)] == -9999)
  expect(flines_out$toCOMID[which(flines_out$COMID == 11687206)] == flines$toCOMID[which(flines$COMID == 11687206)])

  # Multi combination headwater
  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 11690332)] == 11689092)
  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 11689030)] == 11689092)

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

test_that("headwater / top of mainstem collapes works as expected", {
  flines <- readRDS("data/guadalupe_network.rds")
  flines_out <- collapse_flowlines(flines, 0.5, mainstem_thresh = 0.5)

  # small headwater gets combined downstream.
  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 24670381)] == 3839043)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 24670381)] == 0)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 3839043)] ==
           (flines$LENGTHKM[which(flines$COMID == 3839043)] + flines$LENGTHKM[which(flines$COMID == 24670381)]))

  # Very short top of interconfluence flow path gets combined with next downstream correctly
  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 1628129)] == 1628527)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 1628129)] == 0)
  expect(is.na(flines_out$toCOMID[which(flines_out$COMID == 1628129)]))

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 1628527)] ==
           (flines$LENGTHKM[which(flines$COMID == 1628527)] + flines$LENGTHKM[which(flines$COMID == 1628129)]))

  # second instance of very short top of interconfluence flow path gets combined
  # with next downstream correctly and also has a collapse upstream below it.
  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 1629537)] == 1629821)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 1629537)] == 0)
  expect(is.na(flines_out$toCOMID[which(flines_out$COMID == 1629537)]))

  expect(flines_out$joined_fromCOMID[which(flines_out$COMID == 1629565)] == 1629821)

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 1629565)] == 0)
  expect(is.na(flines_out$toCOMID[which(flines_out$COMID == 1629565)]))

  expect(flines_out$LENGTHKM[which(flines_out$COMID == 1629821)] ==
           (flines$LENGTHKM[which(flines$COMID == 1629821)] +
              flines$LENGTHKM[which(flines$COMID == 1629537)] +
              flines$LENGTHKM[which(flines$COMID == 1629565)])) # This one combined in upstream direction

  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 10840906)] == 10840550)
  expect(flines_out$joined_toCOMID[which(flines_out$COMID == 10840550)] == -9999)
  expect(flines_out$toCOMID[which(flines_out$COMID == 10840550)] == flines$toCOMID[which(flines$COMID == 10840554)])

  expect_equal(flines_out$LENGTHKM[which(flines_out$COMID == 10840550)],
           (flines$LENGTHKM[which(flines$COMID == 10840550)] +
              flines$LENGTHKM[which(flines$COMID == 10840906)] +
              flines$LENGTHKM[which(flines$COMID == 10840554)]))

  flines <- readRDS("data/petapsco_network.rds")
  flines <- sf::st_set_geometry(flines, NULL)
  flines <- suppressWarnings(prepare_nhdplus(flines, 0, 0))

  flines_out <- collapse_flowlines(flines, 0.5, mainstem_thresh = 1)

})

test_that("collapse flowlines works with small networks", {
  flines <- readRDS("data/smallish_networks.rds")
  flines_collapse <- collapse_flowlines(flines, 2)

  flines <- suppressWarnings(prepare_nhdplus(readRDS("data/frost_network.rds"), 0, 0))

  c1 <- collapse_flowlines(flines, 1000, F, 1000)

  expect(c1$joined_fromCOMID[which(c1$COMID == 5876961)] == -9999)
  expect(c1$joined_fromCOMID[which(c1$COMID == 5876993)] == 5876961)

  expect_equal(c1$LENGTHK[which(c1$COMID == 5876989)], 0.878)

  r1 <- reconcile_collapsed_flowlines(c1)

  expect_equal(length(which(r1$ID == 1)), 7)
  expect(all(is.na(r1$toID)))

  flines <- suppressWarnings(prepare_nhdplus(readRDS("data/tiny_network.rds"), 0, 0))
  c1 <- collapse_flowlines(flines, 1000, F, 1000)

  expect_equal(c1$LENGTHKM[which(c1$COMID == 7733111)], 0.221)

  flines <- suppressWarnings(prepare_nhdplus(readRDS("data/flag_network.rds"), 0, 0))

  c1 <- collapse_flowlines(flines, 1000, F, 1000)

  expect(c1$joined_fromCOMID[which(c1$COMID == 1797871)] == 1798051)
  r1 <- reconcile_collapsed_flowlines(c1)

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

test_that("repeat collapse doesn't leave orphans", {
  library(sf)
  nhdplus_flines <- readRDS("data/oswego_network.rds")
  flines <- suppressWarnings(sf::st_set_geometry(nhdplus_flines, NULL) %>%
    prepare_nhdplus(0, 0) %>%
    inner_join(select(nhdplus_flines, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(5070))

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring", where = "package:lwgeom", mode = "function")) {

  flines <- nhdplusTools::split_flowlines(flines, 2000, 3)
  flines <- collapse_flowlines(sf::st_set_geometry(flines, NULL), (0.125), TRUE, (0.125))

  # this is right the first pass.
  expect(flines$joined_fromCOMID[which(flines$COMID == 21974341)] == 21975097)

  flines <- suppressWarnings(collapse_flowlines(flines, (0.25), TRUE, (0.25)))

  # needs to get redirected on the second pass.
  # Old Tests:
  expect(flines$joined_toCOMID[which(flines$COMID == 21974341)] == 21975095)
  expect(flines$joined_toCOMID[which(flines$COMID == 21975097)] == 21975095)
  expect(flines$toCOMID[which(flines$COMID == 21975777)] == flines$joined_toCOMID[which(flines$COMID == 21975773)])

  }
})
