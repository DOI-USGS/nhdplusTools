walker_fac <- raster::raster("data-raw/walker_fac.tif")
walker_fdr <- raster::raster("data-raw/walker_fdr.tif")
proj <- as.character(raster::crs(walker_fdr))
walker_catchment <- sf::read_sf("data-raw/walker.gpkg", "CatchmentSP")
walker_catchment <- sf::st_transform(walker_catchment, proj)
walker_flowline <- sf::read_sf("data-raw/walker.gpkg", "NHDFlowline_Network")
walker_flowline <- sf::st_transform(walker_flowline, proj)

devtools::use_data(walker_catchment, walker_flowline, walker_fdr, walker_fac, pkg = ".")

# This is how the raster data was created.
# r <- fasterize::raster("NHDPlusCA/fdr.tif")
#
# cropper <- catchment %>%
#   st_transform(as.character(raster::crs(r))) %>%
#   st_union() %>%
#   st_buffer(1000) %>%
#   as_Spatial()
#
# fac <- fasterize::raster("NHDPlusCA/fac.tif")
# sub_fac <- raster::crop(fac, cropper)
# sub_r <- raster::crop(r, cropper)
# raster::writeRaster(sub_fac, "data-raw/walker_fac.tif", overwrite = TRUE)
# raster::writeRaster(sub_r, "data-raw/walker_fdr.tif", overwrite = TRUE)
# nolint end
