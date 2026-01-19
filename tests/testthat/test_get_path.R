test_that("degenerate levelpath", {
  x <- structure(list(ID = c(203071, 202863, 202883, 205509, 203069, 202875, 942110034),
                      toID = c(202863, 202883, 205509, 203069, 202875, 942110034, 0),
                      fcode = c(33600, 33600, 33600, 33600, 33600, 46006, 55800),
                      nameID = c(630020286, 630020286, 630020286, 630020286, 630020286, 630020286, 630020286),
                      lengthkm = c(14.962, 4.881, 13.204, 2.054, 9.601, 2.893, 10.988),
                      reachcode = c("12110208001272", "12110208000093", "12110208001144",
                                    "12110208001144", "12110208001144", "12110208000099", "12110208017253"),
                      frommeas = c(0, 0, 46.98279, 38.69724, 0, 0, 0),
                      tomeas = c(99.35641, 100, 100, 46.98279, 38.69724, 100, 100),
                      areasqkm = c(21.7197, 1019.7891, 74.0889, 37.3122, 530.9604, 5.562, 17.8893),
                      weight = c(14.962, 19.843, 33.047, 35.101, 44.702, 47.595, 58.583),
                      terminalID = c(942110034, 942110034, 942110034, 942110034, 942110034, 942110034, 942110034
                      )), row.names = c(NA, 7L), class = "data.frame")

  expect_warning(
  y <- nhdplusTools::get_levelpaths(x)
  )
  expect_equal(nrow(y), nrow(x))
})

test_that("calculate level path", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE, warn = FALSE)

  test_flowline <- data.frame(
    ID = test_flowline$COMID,
    toID = test_flowline$toCOMID,
    nameID = walker_flowline$GNIS_ID,
    weight = walker_flowline$ArbolateSu,
    stringsAsFactors = FALSE)

  expect_warning(
  test_flowline_out <- left_join(test_flowline,
                                 get_levelpaths(test_flowline, status = TRUE), by = "ID")
  )
  nhdp_lp <- sort(unique(walker_flowline$LevelPathI))
  nhdt_lp <- sort(unique(test_flowline_out$levelpath))

  expect_true(length(nhdp_lp) == length(nhdt_lp))

  for(lp in seq_along(nhdp_lp)) {
    nhdp <- filter(walker_flowline, LevelPathI == nhdp_lp[lp])
    outlet_comid <- filter(nhdp, Hydroseq == min(Hydroseq))$COMID
    nhdt <- filter(test_flowline_out, outletID == outlet_comid)
    expect_true(all(nhdp$COMID %in% nhdt$ID), info = paste("Mismatch in", nhdp_lp[lp],
                                               "level path from NHDPlus."))
  }

  # break the data
  test_flowline$nameID[test_flowline$ID == 5329293] <- " "
  test_flowline$nameID[test_flowline$ID == 5329295] <- "255208"
  expect_warning(
  test_flowline_out2 <- left_join(test_flowline,
                                  get_levelpaths(test_flowline, status = TRUE), by = "ID")
  )
  expect_equal(test_flowline_out2$levelpath[test_flowline_out2$ID == 5329295], 1)

  expect_warning(
  test_flowline_out2 <- left_join(test_flowline,
                                  get_levelpaths(test_flowline, override_factor = 10,
                                                 status = TRUE), by = "ID")
  )
  expect_equal(test_flowline_out$levelpath, test_flowline_out2$levelpath)
})

test_that("hr levelpath", {
  skip_on_cran()
  skip_on_ci()

  suppressMessages(
    source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools")))
  hr_flowline <- align_nhdplus_names(hr_data$NHDFlowline)

  suppressWarnings(
    fl <- prepare_nhdplus(hr_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE))

  fl <- select(hr_flowline, COMID, ArbolateSu, GNIS_Name) %>%
    left_join(fl, by = "COMID") %>%
    st_sf() %>%
    select(ID = COMID, toID = toCOMID, weight = ArbolateSu, nameID = GNIS_Name)

  expect_warning(expect_message(
  lp <- get_levelpaths(sf::st_set_geometry(fl, NULL), cores = 2)
  ))

  expect_warning(expect_message(
  expect_error(get_levelpaths(sf::st_set_geometry(fl, NULL), cores = "char"))
  ))

  future::plan(future::sequential)

  # Same number of total flowlines
  expect_equal(length(unique(hr_flowline$LevelPathI)), length(unique(lp$levelpath)))

  # follows a semi tricky mainstem the same as HI
  expect_equal(lp[lp$ID == 15000500039693, ]$levelpath, lp[lp$ID == 15000500039696, ]$levelpath)

})

test_that("degenerate", {
  net <- structure(list(ID = 11000020, toID = 0, nameID = "constant",
                        lengthkm = 12.2243026760847, areasqkm = 54.2851667150928,
                        weight = 12.2243026760847, terminalID = 11000020), row.names = 2938080L, class = "data.frame")

  suppressWarnings(
  er <- get_levelpaths(net, 5)
  )
  expect_equal(er$topo_sort, 1)

  expect_equal(er$levelpath, 1)
})

test_that("from vignette works", {
  source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))

  suppressWarnings(fpath <- get_tocomid(
    dplyr::select(new_hope_flowline, COMID, FromNode, ToNode, Divergence, FTYPE,
                  AreaSqKM, LENGTHKM, GNIS_ID)) %>%
    sf::st_cast("LINESTRING") %>%
    select(-tonode, -fromnode, -divergence, -ftype) %>%
    get_sorted(split = TRUE))

  suppressWarnings(
  fpath[["arbolatesum"]] <- calculate_arbolate_sum(
    dplyr::select(fpath, ID = comid, toID = tocomid, length = lengthkm))
  )

  expect_warning(
  lp <- get_levelpaths(
    dplyr::select(fpath, ID = comid, toID = tocomid,
                  nameID = gnis_id, weight = arbolatesum),
    status = FALSE)
  )
  fpath <- dplyr::left_join(fpath, lp, by = c("comid" = "ID"))

  expect_true(all(names(fpath) %in%
               c("comid", "tocomid", "areasqkm", "lengthkm",
                 "gnis_id", "terminalID",
                 "arbolatesum", "outletID",
                 "topo_sort", "levelpath", "geom")))

  expect_equal(length(unique(fpath$levelpath)),
               length(unique(new_hope_flowline$LevelPathI)))

  expect_equal(length(unique(fpath$levelpath)),
               length(unique(fpath$outletID)))

  suppressWarnings(
  plus <- add_plus_network_attributes(dplyr::select(fpath, comid, tocomid,
                                                    lengthkm, areasqkm,
                                                    nameID = gnis_id),
                                      status = FALSE)
  )
  expect_s3_class(plus, "sf")
})
