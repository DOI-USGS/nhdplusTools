get_test_file <- function(temp_dir) {
  check_location <- "../../docs/data/03_sub.zip"
  if(!file.exists(check_location)) {
    temp_file <- file.path(temp_dir, "temp.zip")
    download.file("https://usgs-r.github.io/nhdplusTools/data/03_sub.zip",
                  temp_file, quiet = FALSE)
  } else {
    temp_file <- check_location
  }
  unzip(temp_file, exdir = temp_dir)
}
