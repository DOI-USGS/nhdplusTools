# nolint start
extdata <- system.file("extdata", package = "nhdplusTools")
walker_fac <- raster::raster(file.path(extdata, "walker_fac.tif"))
walker_fdr <- raster::raster(file.path(extdata, "walker_fdr.tif"))
proj <- as.character(raster::crs(walker_fdr))
walker_catchment <- sf::read_sf(file.path(extdata, "walker.gpkg"), "CatchmentSP")
walker_catchment <- sf::st_transform(walker_catchment, proj)
walker_flowline <- sf::read_sf(file.path(extdata, "walker.gpkg"), "NHDFlowline_Network")
walker_flowline <- sf::st_transform(walker_flowline, proj)

# walker.gpkg turned into pre-processed sample data.
# run the above then:
# refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
#                              split_flines_meters = 2000,
#                              collapse_flines_meters = 1,
#                              collapse_flines_main_meters = 1,
#                              split_flines_cores = 2,
#                              out_collapsed = "walker_refactor.gpkg",
#                              out_reconciled = "walker_reconcile.gpkg",
#                              three_pass = TRUE,
#                              purge_non_dendritic = FALSE,
#                              warn = FALSE)
# fline_ref <- sf::read_sf("walker_refactor.gpkg")
# fline_rec <- sf::read_sf("walker_reconcile.gpkg")
# cat_rec <- reconcile_catchments(walker_catchment, fline_ref,
#                                 fline_rec, walker_fdr, walker_fac)
# sf::write_sf(cat_rec, "walker_cat_rec.gpkg")
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
walker_fline_ref <- sf::read_sf(file.path(extdata, "walker_refactor.gpkg"))
walker_fline_rec <- sf::read_sf(file.path(extdata, "walker_reconcile.gpkg"))
walker_catchment_rec <- sf::read_sf(file.path(extdata, "walker_cat_rec.gpkg"))
