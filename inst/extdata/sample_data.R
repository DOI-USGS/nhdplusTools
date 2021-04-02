source(system.file("extdata", "utils.R", package = "nhdplusTools"))

data_dir <- file.path(tempdir(check = TRUE), "nhdplusTools")

download_pkg_data("sample_natseamless.gpkg",
                  "https://usgs-r.github.io/nhdplusTools/data/sample_natseamless.gpkg",
                  data_dir)

sample_data <- file.path(data_dir, "sample_natseamless.gpkg")
