library(dplyr)
library(mapview)

flines <- sf::read_sf("closest_ds.gpkg", "flines")

start_comid <- 26294399

UT <- nhdplusTools::get_UT(flines, start_comid)

flines <- filter(flines, comid %in% UT)

# Let's look at NHDPlusHR
hr_data <- nhdplusTools::download_nhdplushr("hr", "0204")

hr_data <- nhdplusTools::get_nhdplushr("hr", layers = "NHDFlowline")

sum(flines$REACHCODE %in% hr_data$NHDFlowline$REACHCODE)

missing <- filter(flines, !REACHCODE %in% hr_data$NHDFlowline$REACHCODE)

mapview::mapview(flines, color = "blue") +
  mapview::mapview(missing, color = "red")

v2_vaa <- nhdplusTools::get_vaa()

sum(flines$REACHCODE %in% v2_vaa$reachcode)

missing_v2 <- dplyr::filter(flines, !REACHCODE %in% v2_vaa$reachcode)

mapview::mapview(flines, color = "blue") +
  mapview::mapview(missing_v2, color = "red")

out_fline <- filter(flines, comid == start_comid)
outlet <- nhdplusTools::get_node(out_fline, position = "end")

hr_outlet <- nhdplusTools::get_flowline_index(
  filter(hr_data$NHDFlowline, REACHCODE == out_fline$REACHCODE), outlet)

mr_outlet <- nhdplusTools::get_flowline_index("download_nhdplusv2", outlet,
                                              max_matches = 10,
                                              search_radius = units::set_units(1000, "m"))

mr_outlet <- filter(mr_outlet, REACHCODE == out_fline$REACHCODE)

mr_outlet$REACHCODE == hr_outlet$REACHCODE

hr_network <- nhdplusTools::get_UT(hr_data$NHDFlowline, hr_outlet$COMID)

mr_network <- nhdplusTools::get_UT(v2_vaa, mr_outlet$COMID)

hr_fline <- filter(hr_data$NHDFlowline, COMID %in% hr_network)

mr_fline <- nhdplusTools::subset_nhdplus(
  mr_network, nhdplus_data = "download",
  flowline_only = FALSE, overwrite = TRUE,
  output_file = "demo_subset.gpkg")

mapview(flines, color = "blue") +
  mapview(mr_fline, color = "brown") +
  mapview(hr_fline, color = "grey")

source_nodes <- nhdplusTools::get_node(flines, position = "end")

match_mr <- nhdplusTools::get_flowline_index(
  sf::st_transform(mr_fline$NHDFlowline_Network,
                   sf::st_crs(source_nodes)),
  source_nodes,
  search_radius = units::set_units(200, "m"),
  max_matches = 5)

match_hr <- nhdplusTools::get_flowline_index(
  sf::st_transform(hr_data$NHDFlowline,
                   sf::st_crs(source_nodes)),
  source_nodes,
  search_radius = units::set_units(200, "m"),
  max_matches = 5)

?nhdplusTools::disambiguate_flowline_indexes

match_hr_d <- nhdplusTools::disambiguate_flowline_indexes(
  match_hr,
  select(hr_data$NHDFlowline, COMID, REACHCODE),
  data.frame(id = seq_len(nrow(source_nodes)), flines$REACHCODE))

match_mr_d <- nhdplusTools::disambiguate_flowline_indexes(
  match_mr,
  select(mr_fline$NHDFlowline_Network, comid, reachcode),
  data.frame(id = seq_len(nrow(source_nodes)), flines$REACHCODE))

mr_fline$NHDFlowline_Network$frommeas <- round(mr_fline$NHDFlowline_Network$frommeas, 3)
mr_fline$NHDFlowline_Network$tomeas <- round(mr_fline$NHDFlowline_Network$tomeas, 3)
match_mr_d$REACH_meas <- round(match_mr_d$REACH_meas, 3)

match_mr_d_points <- nhdplusTools::get_hydro_location(
  match_mr_d,
  select(mr_fline$NHDFlowline_Network, comid, frommeas, tomeas))

lookup <- sf::st_sf(cbind(data.frame(id = seq_len(nrow(source_nodes))),
                          flines))

lookup_mr <- left_join(lookup, select(match_mr_d, id,
                                      match_REACHCODE = REACHCODE,
                                      REACH_meas, offset),
                       by = "id")

lookup_hr <- sf::st_sf(left_join(lookup, select(match_hr_d, id,
                                                match_REACHCODE = REACHCODE,
                                                REACH_meas, offset),
                                 by = "id"))

mapview(lookup_mr) + mapview(mr_fline$NHDFlowline_Network)
# probably better in a GIS

lookup_mr <- sf::st_make_valid(lookup_mr)

sf::write_sf(lookup_mr, "temp.gpkg", "mr")

# now we would go look at the data and see what the groups look like.
# maybe we are good and just need to choose the flowline just upstream?
# what lines of evidence do we have here?

mapview(lookup_mr) + mapview(mr_fline$NHDFlowline_Network)

outlet_nldi <- list(featureSource = "comid",
                    featureID = mr_outlet$COMID)

basin <- nhdplusTools::get_nldi_basin(outlet_nldi, simplify = FALSE)

all_outlet_char <- nhdplusTools::get_nldi_characteristics(
  outlet_nldi,
  type = "total")

char <- nhdplusTools::discover_nldi_characteristics(type = "local")$local

# Average annual runoff (mm) from McCabe and Wolock's Runoff Model
# 1951-2000 per NHDPlus version 2 catchment
characteristic <- "CAT_WB5100_ANN"

char[char$characteristic_id == characteristic, ]

item <- "56fd5bd0e4b0c07cbfa40473"

sbtools::item_list_files(item)

f <- "WB5100_ANN_CONUS.zip"

sbtools::item_file_download(item, names = f, destinations = f)

zip::unzip(f)

WB <- readr::read_csv("WB5100_ANN_CONUS.txt")

wb <- filter(WB, COMID %in% mr_fline$NHDFlowline_Network$comid)

cat <- left_join(mr_fline$CatchmentSP, select(WB, COMID, CAT_WB5100_ANN, TOT_WB5100_ANN),
                 by = c("featureid" = "COMID"))

plot(cat['CAT_WB5100_ANN'])
plot(cat['TOT_WB5100_ANN'])

?nhdplusTools::plot_nhdplus

plot_data <- nhdplusTools::plot_nhdplus(outlet_nldi, streamorder = 2, flowline_only = FALSE,
                                        plot_config = list(flowline = list(lwd = 2)),
                                        gpkg = "demo.gpkg", cache_data = "demo.rds")
plot(sf::st_transform(cat['TOT_WB5100_ANN'], 3857),
     add = TRUE, col = sf::sf.colors(alpha = 0.2))

sf::st_layers("demo.gpkg")

# saved in the rds
names(plot_data)

map <- nhdplusTools::map_nhdplus(outlet_nldi, streamorder = 2,
                          plot_config = list(flowline = list(lwd = 2)),
                          gpkg = "demo.gpkg", cache_data = "demo.rds", return_map = TRUE)

colfun <- leaflet::colorFactor(topo.colors(5), unique(cat$CAT_WB5100_ANN))

leaflet::addPolygons(map, data = sf::st_transform(cat, 4326), weight = 2,
                     fillColor = ~colfun(cat$CAT_WB5100_ANN), color = "grey", stroke = TRUE)
