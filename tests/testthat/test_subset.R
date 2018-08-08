context("subset")


test_that("subset runs as expected", {
  unlink("data/temp/*")

  sample_data <- "data/sample_natseamless.gpkg"

  nhdplus_path(sample_data)

  if (!dir.exists("data/temp")) dir.create("data/temp")

  staged_nhdplus <- stage_national_data(output_path = "data/temp/")

  start_comid <- 13293750

  sample_flines <- readRDS(staged_nhdplus$flowline)

  comids <- get_UT(sample_flines, start_comid)

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

  expect_equal(length(messages), 14)

  check_layers <- function() {
    expect_equal(nrow(sf::read_sf(out_file, "CatchmentSP")), 167)
    expect_equal(nrow(sf::read_sf(out_file, "NHDWaterbody")), 90)
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

  if (!dir.exists("data/temp")) dir.create("data/temp")

  expect_error(suppressWarnings(stage_national_data()),
               paste("Didn't find NHDPlus national data in default",
                     "location: ../NHDPlusV21_National_Seamless.gdb"))

  sample_gpkg <- "data/sample_natseamless.gpkg"

  nhdplus_path(sample_gpkg)

  expect_warning(temp_data <- stage_national_data(),
                 "No output path provided, using: data")

  temp_data <- lapply(temp_data, unlink)

  temp_data <- stage_national_data(output_path = "data/temp")

  expect(suppressWarnings(all(lapply(temp_data, file.exists))))

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

})
