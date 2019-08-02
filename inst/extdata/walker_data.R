# See hyRefactor package for how this data was created.
# nolint start
extdata <- system.file("extdata", package = "nhdplusTools")
proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
walker_catchment <- sf::read_sf(file.path(extdata, "walker.gpkg"), "CatchmentSP")
walker_catchment <- sf::st_transform(walker_catchment, proj)
walker_flowline <- sf::read_sf(file.path(extdata, "walker.gpkg"), "NHDFlowline_Network")
walker_flowline <- sf::st_transform(walker_flowline, proj)
# nolint end
# walker_fline_ref <- sf::read_sf(file.path(extdata, "walker_refactor.gpkg"))
# walker_fline_rec <- sf::read_sf(file.path(extdata, "walker_reconcile.gpkg"))
# walker_catchment_rec <- sf::read_sf(file.path(extdata, "walker_cat_rec.gpkg"))
