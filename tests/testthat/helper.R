library("sf")
library("dplyr")

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
