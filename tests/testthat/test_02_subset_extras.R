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

test_that("prep_nhdplus runs as expected", {

  temp_dir <- tempdir()

  if (!dir.exists(temp_dir)) dir.create(temp_dir)

  expect_error(suppressWarnings(stage_national_data()),
               paste("Didn't find NHDPlus national data in default",
                     "location: ../NHDPlusV21_National_Seamless.gdb"))

  sample_gpkg <- file.path(temp_dir, "sample_natseamless.gpkg")

  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

  file.copy(sample_data, sample_gpkg)

  nhdplus_path(sample_gpkg)

  expect_warning(temp_data <- stage_national_data(),
                 regexp = paste0("No output path provided, using:.*"))

  temp_data <- lapply(temp_data, unlink)

  temp_data <- stage_national_data(output_path = temp_dir)

  expect_true(suppressWarnings(all(lapply(temp_data, file.exists))))

  temp_data <- lapply(temp_data, unlink)

  nhdplus_path("bogus")

  expect_error(suppressWarnings(stage_national_data()),
               paste("Didn't find NHDPlus national data in",
                     "user specified location: bogus"))

  nhdplus_path(sample_gpkg)

  expect_error(stage_national_data(include = c("bogus"),
                                   output_path = "data/temp"),
               paste("Got invalid include entries. Expect one",
                     "or more of: attribute, flowline, catchment."))

  temp_data <- stage_national_data(output_path = temp_dir)

  expect_equal(
    capture_warnings(
      temp_data <- stage_national_data(output_path = temp_dir)),
    c("attributes file exists", "flowline file exists",
      "catchment already exists."))

  temp_data <- lapply(temp_data, unlink)

  unlink(sample_gpkg)

})

test_that("by rpu", {
  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

  nhdplus_path(sample_data)

  staged_nhdplus <- stage_national_data(output_path = tempdir())

  sample_flines <- readRDS(staged_nhdplus$flowline)

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

  vaa_new$arbolatesu <- calculate_arbolate_sum(
    dplyr::select(vaa_new, ID = comid, toID = tocomid, length = lengthkm))

  sub <- subset_rpu(vaa_new, "14a", strict = TRUE)

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
                                   overwrite = FALSE, actually_plot = FALSE)


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
