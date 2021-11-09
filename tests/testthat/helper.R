options("rgdal_show_exportToProj4_warnings"="none")

library("sf")
library("dplyr")

sf::sf_use_s2(TRUE)

unlink(file.path(tempdir(check = TRUE), "*"), recursive = TRUE)

get_test_file <- function(temp_dir) {
  check_locations <- c("../../docs/data/03_sub.zip", "docs/data/03_sub.zip")
  check_location <- check_locations[file.exists(check_locations)]
  if(length(check_location) == 0) {
    temp_file <- file.path(temp_dir, "temp.zip")
    download.file("https://usgs-r.github.io/nhdplusTools/data/03_sub.zip",
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
