# ms <- sf::read_sf("https://geoconnex.us/ref/mainstems/377002")
#
# outlet_comid <- as.integer(gsub("https://geoconnex.us/nhdplusv2/comid/", "",
#                                 ms$outlet_nhdpv2_comid))
#
#
# basin <- nhdplusTools::get_nldi_basin(list(featureSource = "comid", featureID = outlet_comid))
#
# threedhp <- nhdplusTools::get_3dhp(basin, type = "flowline", buffer = 100)
#
# threedhp <- nhdplusTools::get_3dhp(ids = unique(threedhp$mainstemid), type = "flowline")
#
# sf::write_sf(threedhp, "docs/data/3dhp_yahara_flowlines.geojson")

source(system.file("extdata", "utils.R", package = "nhdplusTools"))

data_dir <- file.path(tempdir(check = TRUE), "nhdplusTools")

download_pkg_data("3dhp_yahara_flowlines.geojson",
                  "https://doi-usgs.github.io/nhdplusTools/data/3dhp_yahara_flowlines.geojson",
                  data_dir)

sample_3dhp_data <- file.path(data_dir, "3dhp_yahara_flowlines.geojson")
