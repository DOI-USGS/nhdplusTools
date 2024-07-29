options("rgdal_show_exportToProj4_warnings"="none")

library("sf")
library("dplyr")

nhdplusTools_cache_settings(mode = "memory", timeout = 1)

sf::sf_use_s2(TRUE)

unlink(file.path(tempdir(check = TRUE), "*"), recursive = TRUE)

get_test_file <- function(temp_dir) {
  check_locations <- c("../../docs/data/03_sub.zip", "docs/data/03_sub.zip")
  check_location <- check_locations[file.exists(check_locations)]
  if(length(check_location) == 0) {
    temp_file <- file.path(temp_dir, "temp.zip")
    download.file("https://doi-usgs.github.io/nhdplusTools/data/03_sub.zip",
                  temp_file, quiet = FALSE)
  } else {
    temp_file <- check_location[1]
  }
  unzip(temp_file, exdir = temp_dir)
}


get_test_dir <- function() {
  f <- list.files(pattern = "data$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
  f[grep("testthat\\/data", f)]
}


check_layers <- function(out_file) {
  expect_equal(nrow(sf::read_sf(out_file, "CatchmentSP")), 4)
  expect_equal(nrow(sf::read_sf(out_file, "NHDWaterbody")), 1)
  expect_true(sf::st_crs(sf::read_sf(out_file, "CatchmentSP")) ==
                sf::st_crs(4269))
  expect_true(sf::st_crs(sf::read_sf(out_file, "NHDWaterbody")) ==
                sf::st_crs(4269))
  expect_true(sf::st_crs(sf::read_sf(out_file, "NHDFlowline_Network")) ==
                sf::st_crs(4269))
}

setup_workdir <- function() {
  work_dir <- file.path(tempdir(), "test_hr")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  out_gpkg <- file.path(work_dir, "temp.gpkg")
  list(wd = work_dir, og = out_gpkg)
}

teardown_workdir <- function(work_dir) {
  unlink(work_dir, recursive = TRUE, force = TRUE)
}
