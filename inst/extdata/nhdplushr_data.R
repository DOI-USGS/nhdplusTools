# nolint start
work_dir <- file.path(tempdir(check = TRUE), "hr_temp")

dir.create(work_dir, showWarnings = FALSE)

unlink(file.path(work_dir, "*"), recursive = TRUE)

hr_source <- file.path(work_dir, "temp.zip")

project_file <- c("../../docs/data/03_sub.zip", "docs/data/03_sub.zip")
project_file <- project_file[file.exists(project_file)]

if(length(project_file) > 0 &&
          file.exists(project_file[1])) {
  file.copy(project_file, hr_source, overwrite = TRUE)
} else {
  url <- "https://usgs-r.github.io/nhdplusTools/data/03_sub.zip"
  invisible(httr::RETRY("GET", url, httr::write_disk(hr_source, overwrite=TRUE),
                        times = 3, pause_cap = 20))
}

unzip(hr_source, exdir = work_dir)

hr_source <- file.path(work_dir, "03_sub.gpkg")
hr_gpkg <- file.path(work_dir, "hr_data.gpkg")

hr_data <- get_nhdplushr(work_dir, layers = NULL,
                         out_gpkg = hr_gpkg,
                         pattern = "03_sub.gpkg")

# hr_catchment <- sf::read_sf(hr_path, "NHDPlusCatchment")
# hr_catchment <- sf::st_transform(hr_catchment, proj)
#
# hr_flowline <- nhdplusTools:::get_hr_data(hr_path, "NHDFlowline")
# hr_flowline <- sf::st_transform(hr_flowline, proj)
#
# hr_wbd <- sf::read_sf(system.file("extdata/new_hope_wbd.gpkg", package = "nhdplusTools"),
#                       "HUC12")
# nolint end
