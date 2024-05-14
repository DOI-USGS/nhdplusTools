test_that("subset by bounding box", {
  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

  bbox <- sf::st_bbox(c(xmin = -89.4, ymin = 43, xmax = -89.3, ymax = 43.1), crs = sf::st_crs(4326))

  expect_error(subset_nhdplus(bbox = (bbox + c(-200, -200, 200, 200)),
                              nhdplus_data = sample_data,
                              simplified = TRUE,
                              status = FALSE),
               "invalid bbox entry")

  fi <- subset_nhdplus(bbox = bbox,
                       nhdplus_data = sample_data,
                       simplified = TRUE,
                       status = FALSE,
                       flowline_only = TRUE)

  expect_equal(names(fi), "NHDFlowline_Network")

  out_file <- tempfile(fileext = ".gpkg")

  fi <- subset_nhdplus(bbox = bbox,
                       output_file = out_file,
                       nhdplus_data = sample_data,
                       simplified = TRUE,
                       status = FALSE)

  check_layers <- function() {
    expect_equal(nrow(sf::read_sf(out_file, "CatchmentSP")), 66)
    expect_equal(nrow(sf::read_sf(out_file, "NHDWaterbody")), 12)
    expect_true(sf::st_crs(sf::read_sf(out_file, "CatchmentSP")) ==
                  sf::st_crs(4269))
    expect_true(sf::st_crs(sf::read_sf(out_file, "NHDWaterbody")) ==
                  sf::st_crs(4269))
    expect_true(sf::st_crs(sf::read_sf(out_file, "NHDFlowline_Network")) ==
                  sf::st_crs(4269))

  }

  check_layers()
  unlink(out_file)

  fi <- subset_nhdplus(bbox = as.numeric(bbox),
                       output_file = out_file,
                       nhdplus_data = sample_data,
                       simplified = TRUE,
                       status = FALSE)

  check_layers()

  expect_warning(fi <- subset_nhdplus(comids = c(1, 2, 3),
                                      bbox = as.numeric(bbox),
                                      nhdplus_data = sample_data,
                                      overwrite = TRUE,
                                      simplified = TRUE,
                                      status = FALSE),
                 "using bounding box rather than submitted comids")

  expect_error(fi <- subset_nhdplus(bbox = as.character(bbox),
                                    nhdplus_data = sample_data,
                                    status = FALSE),
               "invalid bbox entry")
})

test_that("by rpu", {
  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

  nhdplus_path(sample_data)

  sample_flines <- sf::read_sf(nhdplus_path(), "NHDFlowline_Network")

  out <- subset_rpu(sample_flines, rpu = "07b")

  expect_equal(names(out), names(sample_flines))
  expect(nrow(out), 267)
  expect(nrow(subset_rpu(sample_flines, rpu = "07b",
                         run_make_standalone = TRUE)), 267)

  expect_equal(nrow(subset_vpu(sample_flines, vpu = "07")), 267)

  suppressWarnings(sample_flines <- dplyr::left_join(
    dplyr::select(sample_flines, COMID, RPUID, TerminalPa,
                  ArbolateSu, DnHydroseq, Pathlength, FCODE, DnLevelPat),
    prepare_nhdplus(sample_flines,
                    0, 0, 0, FALSE), by = "COMID"))

  expect(nrow(subset_rpu(sample_flines, rpu = "07b")), 267)
  expect(nrow(subset_rpu(sample_flines, rpu = "07b", run_make_standalone = FALSE)), 267)

})

test_that("big rpu test", {
  skip_on_cran()

  vaa <- get_vaa(atts = c("comid", "pathlength", "lengthkm",
                          "hydroseq", "dnhydroseq", "levelpathi",
                          "rpuid", "vpuid", "fcode", "arbolatesu",
                          "terminalfl", "terminalpa", "dnlevelpat"))


  vaa_sub <- dplyr::filter(vaa, vpuid == "17")

  sub <- subset_rpu(vaa_sub, "17b", strict = FALSE)

  expect_false(24192564 %in% sub$comid)

  vaa <- dplyr::filter(vaa, vpuid == "14")

  sub <- subset_rpu(vaa, "14a", strict = TRUE)

  expect_equal(names(sub), names(vaa))

  sub2 <- subset_rpu(vaa, "14a", strict = FALSE)

  expect_false(1356902 %in% sub$comid)
  expect_true(1356902 %in% sub2$comid)

  vaa_new <- get_vaa(atts = c("comid", "tocomid", "pathlength", "lengthkm",
                          "hydroseq", "dnhydroseq", "levelpathi",
                          "fcode", "terminalfl", "terminalpa",
                          "dnlevelpat"),
                 updated_network = TRUE)

  vaa_new <- dplyr::right_join(vaa_new,
                               vaa[c("comid", "rpuid", "vpuid")],
                               by = "comid")

  suppressWarnings(
  vaa_new$arbolatesu <- calculate_arbolate_sum(
    dplyr::select(vaa_new, ID = comid, toID = tocomid, length = lengthkm))
  )

  suppressWarnings(
  sub <- subset_rpu(vaa_new, "14a", strict = TRUE)
  )

  expect_equal(names(sub), names(vaa_new))

  sub2 <- subset_rpu(vaa, "14a", strict = FALSE)

  expect_false(1356902 %in% sub$comid)
  expect_true(1356902 %in% sub2$comid)
})

test_that("projection check", {

  skip_on_cran()

  out <- tempfile(fileext = ".gpkg")

  unlink(out, recursive = TRUE)

  mr <- nhdplusTools::plot_nhdplus(list(13293970), gpkg = out,
                                   nhdplus_data = out,
                                   overwrite = FALSE,
                                   actually_plot = FALSE)

  expect_true(sf::st_crs(mr$flowline) == sf::st_crs(4269))

  expect_equal(nrow(mr$basin), 1)

})

test_that("extras", {
  expect_equal(nhdplusTools:::get_empty("POLYGON"), sf::st_polygon())
  expect_equal(nhdplusTools:::get_empty("LINESTRING"), sf::st_linestring())
  expect_equal(nhdplusTools:::get_empty("MULTIPOLYGON"), sf::st_multipolygon())
  expect_equal(nhdplusTools:::get_empty("MULTILINESTRING"), sf::st_multilinestring())
  expect_equal(nhdplusTools:::get_empty("POINT"), sf::st_point())
  expect_equal(nhdplusTools:::get_empty("MULTIPOINT"), sf::st_multipoint())
  expect_error(nhdplusTools:::get_empty("BORKED"), "unexpected geometry type")
})

test_that("unify_types", {

  check <- list(data.frame(), data.frame(one = 1, two = "2"), data.frame(one = "1", two = 2))

  out <- nhdplusTools:::unify_types(check)

  expect_equal(out,
               list(structure(list(one = 1, two = 2), row.names = c(NA, -1L), class = "data.frame"),
                    structure(list(one = 1, two = 2), row.names = c(NA, -1L), class = "data.frame")))
})
