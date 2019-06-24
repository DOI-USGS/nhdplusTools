context("subset")


test_that("subset runs as expected", {

  nhdplus_path("borked")

  expect_error(subset_nhdplus(1234567,
                              output_file = "borked.gpkg"),
               "couldn't find nhdplus data")

  unlink("data/temp/*")

  sample_data <- system.file("extdata/sample_natseamless.gpkg",
                             package = "nhdplusTools")

  nhdplus_path(sample_data)

  if (!dir.exists("data/temp")) dir.create("data/temp")

  staged_nhdplus <- stage_national_data(output_path = "data/temp/")

  start_comid <- 13293392

  sample_flines <- readRDS(staged_nhdplus$flowline)

  comids <- get_UM(sample_flines, start_comid)

  out_file <- "./data/temp/demo_subset.gpkg"

  fi <- subset_nhdplus(comids = comids,
                 output_file = out_file,
                 nhdplus_data = sample_data,
                 status = FALSE)

  expect_equal(fi, out_file)

  expect_error(
    subset_nhdplus(comids = comids,
                   output_file = out_file,
                   nhdplus_data = sample_data,
                   overwrite = FALSE,
                   status = FALSE),
    "output_file exists and overwrite is false.")

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

  expect_error(subset_nhdplus(comids = "dud",
                              output_file = "test",
                              "output_file must end in '.gpkg'"))

  unlink("data/temp/*")

  fi <- subset_nhdplus(comids = comids,
                       output_file = out_file,
                       nhdplus_data = sample_data,
                       overwrite = FALSE,
                       status = FALSE)

  check_layers()

  unlink("data/temp/*")

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")

  fi <- subset_nhdplus(comids = comids,
                       output_file = out_file,
                       nhdplus_data = "download",
                       overwrite = FALSE,
                       status = FALSE)

  check_layers()

  })

test_that("prep_nhdplus runs as expected", {
  unlink("data/temp/*")

  temp_dir <- "data/temp"

  if (!dir.exists(temp_dir)) dir.create(temp_dir)

  expect_error(suppressWarnings(stage_national_data()),
               paste("Didn't find NHDPlus national data in default",
                     "location: ../NHDPlusV21_National_Seamless.gdb"))

  sample_gpkg <- file.path(temp_dir, "sample_natseamless.gpkg")

  file.copy(system.file("extdata/sample_natseamless.gpkg",
                        package = "nhdplusTools"), sample_gpkg)

  nhdplus_path(sample_gpkg)

  expect_warning(temp_data <- stage_national_data(),
                 "No output path provided, using: data")

  temp_data <- lapply(temp_data, unlink)

  temp_data <- stage_national_data(output_path = "data/temp")

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

  temp_data <- stage_national_data(output_path = "data/temp")

  expect_equal(
    capture_warnings(
      temp_data <- stage_national_data(output_path = "data/temp")),
    c("attributes file exists", "flowline file exists",
      "catchment already exists."))

  temp_data <- lapply(temp_data, unlink)

  unlink(sample_gpkg)

})

test_that("subset works with HR", {
  work_dir <- tempdir()
  temp_file <- file.path(work_dir, "temp.zip")
  file.copy(system.file("extdata/03_sub.zip", package = "nhdplusTools"),
            temp_file)
  unzip(temp_file, exdir = work_dir)

  hr <- get_nhdplushr(work_dir, out_gpkg = file.path(work_dir, "temp.gpkg"),
                       layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody",
                                  "NHDArea", "NHDPlusSink"))

  flowlines <- read_sf(hr, "NHDFlowline")

  up_ids <- get_UT(flowlines, 15000500028335)

  sub <- subset_nhdplus(up_ids, file.path(work_dir, "sub.gpkg"), hr)

  layers <- st_layers(sub)

  expect_equal(length(layers$name), 4)
  expect_equal(layers$features[which(layers$name == "NHDFlowline")], 1427)
})
