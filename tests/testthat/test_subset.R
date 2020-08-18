context("subset")

test_that("subset errors", {

  temp_dir <- tempdir()

  nhdplus_path(file.path(temp_dir, "borked"))

  expect_error(subset_nhdplus(1234567,
                              output_file = tempfile(fileext = ".gpkg")),
               "couldn't find nhdplus data")

  expect_error(subset_nhdplus(comids = "dud",
                              output_file = "test",
                              "output_file must end in '.gpkg'"))

  expect_error(subset_nhdplus(output_file = "test",
                              "must provide comids or bounding box"))

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")
})

test_that("subset runs as expected", {

  temp_dir <- tempdir()
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  sample_data <- system.file("extdata/sample_natseamless.gpkg",
                             package = "nhdplusTools")

  expect_equal(nhdplusTools:::get_catchment_layer_name(TRUE, sample_data), "CatchmentSP")
  expect_equal(nhdplusTools:::get_catchment_layer_name(TRUE, "download"), "CatchmentSP")
  expect_equal(nhdplusTools:::get_catchment_layer_name(FALSE, "download"), "CatchmentSP")

  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
  tempf <- file.path(temp_dir, "temp.geojson")
  sf::write_sf(nc, tempf, "Catchment")
  expect_equal(nhdplusTools:::get_catchment_layer_name(FALSE, tempf), "Catchment")

  nhdplus_path(sample_data)

  if (!dir.exists(temp_dir)) dir.create(temp_dir)

  staged_nhdplus <- stage_national_data(output_path = temp_dir)

  comids <- get_UM(readRDS(staged_nhdplus$flowline), 13293392)

  out_file <- tempfile(fileext = ".gpkg")

  # No output file
  fi <- subset_nhdplus(comids = comids,
                       output_file = NULL,
                       nhdplus_data = sample_data,
                       status = FALSE)

  expect_equal(names(fi), c("NHDFlowline_Network", "CatchmentSP", "NHDArea",
                            "NHDWaterbody", "Gage", "Sink",
                            "NHDFlowline_NonNetwork"))
  expect_equal(nrow(fi$CatchmentSP), 4)
  expect_equal(nrow(fi$NHDWaterbody), 1)

  # flowline only
  fi <- subset_nhdplus(comids = comids,
                       output_file = NULL,
                       nhdplus_data = sample_data,
                       status = FALSE,
                       flowline_only = TRUE)

  expect_equal(names(fi), "NHDFlowline_Network")

  # write to output file
  fi <- subset_nhdplus(comids = comids,
                 output_file = out_file,
                 nhdplus_data = sample_data,
                 return_data = FALSE,
                 status = FALSE)

  expect_equal(fi, out_file)

  # don't overwrite
  expect_error(
    subset_nhdplus(comids = comids,
                   output_file = out_file,
                   nhdplus_data = sample_data,
                   overwrite = FALSE,
                   status = FALSE),
    "output_file exists and overwrite is false.")

  # check that status works
  messages <- capture_messages(
    fi <- subset_nhdplus(comids = comids,
                         output_file = out_file,
                         overwrite = TRUE,
                         status = TRUE))

  expect_equal(length(messages), 15)

  check_layers <- function() {
    expect_equal(nrow(sf::read_sf(out_file, "CatchmentSP")), 4)
    expect_equal(nrow(sf::read_sf(out_file, "NHDWaterbody")), 1)
  }

  check_layers()

  unlink(file.path(temp_dir, "*"))

  # Make sure passing data directly works
  fi <- subset_nhdplus(comids = comids,
                       output_file = out_file,
                       nhdplus_data = sample_data,
                       overwrite = FALSE,
                       status = FALSE)

  check_layers()

  unlink(file.path(temp_dir, "*"))

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")

  skip_on_cran()

  # download option
  fi <- subset_nhdplus(comids = comids,
                       output_file = out_file,
                       nhdplus_data = "download",
                       overwrite = FALSE,
                       status = FALSE, flowline_only = FALSE)

  check_layers()

  unlink(file.path(temp_dir, "*"))

  fi <- subset_nhdplus(comids = comids,
                       output_file = out_file,
                       nhdplus_data = "download",
                       overwrite = FALSE,
                       status = FALSE)

  expect_equal(length(names(fi)), 1)

  })

test_that("subset works with HR", {
  work_dir <- tempdir()

  skip_on_cran()
  get_test_file(work_dir)

  out_gpkg <- file.path(work_dir, "temp.gpkg")

  hr <- get_nhdplushr(work_dir, out_gpkg = out_gpkg,
                      layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody",
                                 "NHDArea", "NHDPlusSink"))

  expect_equal(nhdplusTools:::get_catchment_layer_name(TRUE, out_gpkg), "NHDPlusCatchment")
  expect_equal(nhdplusTools:::get_catchment_layer_name(FALSE, out_gpkg), "NHDPlusCatchment")

  flowlines <- sf::read_sf(out_gpkg, "NHDFlowline")

  up_ids <- get_UT(flowlines, 15000500028335)

  sub <- subset_nhdplus(up_ids, file.path(work_dir, "sub.gpkg"), out_gpkg, return_data = FALSE)

  layers <- sf::st_layers(sub)

  expect_equal(length(layers$name), 4)
  expect_equal(layers$features[which(layers$name == "NHDFlowline")], 1427)
})

test_that("subset by bounding box", {
  sample_data <- system.file("extdata/sample_natseamless.gpkg",
                             package = "nhdplusTools")

  bbox <- st_bbox(c(xmin = -89.4, ymin = 43, xmax = -89.3, ymax = 43.1), crs = st_crs(4326))

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

  file.copy(system.file("extdata/sample_natseamless.gpkg",
                        package = "nhdplusTools"), sample_gpkg)

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
  sample_data <- system.file("extdata/sample_natseamless.gpkg",
                             package = "nhdplusTools")

  nhdplus_path(sample_data)

  staged_nhdplus <- stage_national_data(output_path = tempdir())

  sample_flines <- readRDS(staged_nhdplus$flowline)

  expect(nrow(subset_rpu(sample_flines, rpu = "07b")), 267)
  expect(nrow(subset_rpu(sample_flines, rpu = "07b", run_make_standalone = TRUE)), 267)
})
