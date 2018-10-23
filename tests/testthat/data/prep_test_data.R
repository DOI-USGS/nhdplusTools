# data moved to extdata

library(nhdplusTools)

# get path from env
flines <- readRDS("../../../nhdplus_flowline.rds")

# get COMID from search
# https://cida.usgs.gov/nwc/#!waterbudget/huc/070900020902
# https://cida.usgs.gov/nldi/huc12pp/070900020902

start_COMID <- 13296606

# This is coming back non-unique.
UT <- get_UT(flines, start_COMID)

y_flines <- dplyr::filter(flines, COMID %in% UT)

rm(flines)

catchments <- readRDS("../../../nhdplus_catchment.rds")

y_catchments <- dplyr::filter(catchments, FEATUREID %in% UT)

sf::st_write(y_flines, "data/sample_natseamless.gpkg", "NHDFlowline_Network")
sf::st_write(y_catchments, "data/sample_natseamless.gpkg", "CatchmentSP")

rm(catchments)

# sink <- sf::st_read("../NHDPlusV21_National_Seamless.gdb", "Sink")
#
# y_sink <- dplyr::filter(sink, FEATUREID %in% UT)
#
# area <- sf::st_read("../NHDPlusV21_National_Seamless.gdb", "NHDArea")
#
# y_area <- dplyr::filter(area, COMID %in% UT)
#
waterbody <- sf::st_read("../../../NHDPlusV21_National_Seamless.gdb", "NHDWaterbody")

y_waterbody <- sf::st_intersection(sf::st_transform(waterbody, 5070),
                                   sf::st_transform(y_catchments$Shape, 5070))

y_waterbody <- dplyr::filter(waterbody, waterbody$COMID %in% y_waterbody$COMID)

sf::st_write(y_waterbody, "data/sample_natseamless.gpkg", "NHDWaterbody")


