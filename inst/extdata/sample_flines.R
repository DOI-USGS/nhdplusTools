source(system.file("extdata", "utils.R", package = "nhdplusTools"))

data_dir <- file.path(rappdirs::user_data_dir(), "nhdplusTools")

download_pkg_data("petapsco_flowlines.gpkg",
                  "https://usgs-r.github.io/nhdplusTools/data/petapsco_flowlines.gpkg",
                  data_dir)

sample_flines <- sf::read_sf(file.path(data_dir, "petapsco_flowlines.gpkg"))
