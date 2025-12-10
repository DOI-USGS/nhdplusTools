ms <- sf::read_sf("https://geoconnex.us/ref/mainstems/377002")

outlet_comid <- as.integer(gsub("https://geoconnex.us/nhdplusv2/comid/", "",
                                ms$outlet_nhdpv2_comid))


basin <- nhdplusTools::get_nldi_basin(list(featureSource = "comid", featureID = outlet_comid))

threedhp <- nhdplusTools::get_3dhp(basin, type = "flowline", buffer = 100)

threedhp <- nhdplusTools::get_3dhp(ids = unique(threedhp$mainstemid), type = "flowline")

sf::write_sf(threedhp, "docs/data/3dhp_yahara_flowlines.geojson")
