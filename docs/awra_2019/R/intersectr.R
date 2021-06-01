# These functions will end up in intersectr

#' @title Get XY Coodinates
#' @param nc_source Path to NetCDF file, OPeNDAP base URI, or object of class "NetCDF".
#' @param nc_var Data variable of interest. Only required if a one row nc_coord_vars is 
#' not supplied.
#' @param nc_coord_vars As returned by ncmeta::nc_coord_var(nc_source, nc_var). must be one row.
#' @param prj_convert Multiplier to go from coordinate variable units to projection units 
#' (e.g. 1000 m/km to convert km xy coordinates to meters.)
#' @noRd
get_xy_coords <- function(nc_source, nc_var = NULL, nc_coord_vars = NULL, prj_convert = 1) {
  if(!class(nc_source) == "NetCDF") {
    nc_source <- RNetCDF::open.nc(nc_source)
    on.exit(RNetCDF::close.nc(nc_source))
  }
  
  if(is.null(nc_coord_vars)) {
    if(is.null(nc_var)) stop("Missing conditionally required nc_var input.")
    nc_coord_vars <- ncmeta::nc_coord_var(nc_source, nc_var)
  }
  
  x <- RNetCDF::var.get.nc(nc_source, nc_coord_vars$X, unpack = TRUE) * prj_convert
  y <- RNetCDF::var.get.nc(nc_source, nc_coord_vars$Y, unpack = TRUE) * prj_convert
  
  return(list(x = x, y = y))
}

#' @title Wrapper function for intersectr package workflow.
#' @param nc_source Path to NetCDF file, OPeNDAP base URI.
#' @param nc_var variable to intersect with geom.
#' @param geom sf data.frame with POLYGON or MULTIPOLYGON geometry and an "ID" collumn.
#' @param ann_prj projection specification compatible with sf::st_crs if not supplied, 
#' projection from geom will be used for analysis.
#' @param buffer_dist distance to buffer cell geometry in analyisis projection
#' @param status boolean print status if TRUE
#' @param start_datetime,end_datetime character or POSIX character format is strptime default e.g. "2010-10-10 10:10:10"
#' @param return_cell_geometry boolean if TRUE, a "cell_geometry" element is appended to the returned list.
#' @value A list containing requested output.
#' @noRd
run_intersection <- function(nc_source, nc_var, geom, ann_prj = NULL, buffer_dist = 0, 
                             start_datetime, end_datetime, status = FALSE, 
                             return_cell_geometry = FALSE) {
  nc <- RNetCDF::open.nc(nc_source)
  
  if(is.null(ann_prj)) {
    ann_prj <- sf::st_crs(geom)
  } 
  
  if(sf::st_crs(geom) != sf::st_crs(ann_prj)) {
    geom <- st_transform(geom, ann_prj)
  }
  
  nc_coord_vars <- ncmeta::nc_coord_var(nc, nc_var)
  
  coords <- get_xy_coords(nc, nc_coord_vars = nc_coord_vars)
  
  cell_geometry <- intersectr::create_cell_geometry(
    X_coords = coords$x, Y_coords = coords$y, 
    prj = ncmeta::nc_gm_to_prj(ncmeta::nc_grid_mapping_atts(nc)), 
    geom = geom, buffer_dist = buffer_dist)
  
  data_source_cells <- sf::st_sf(dplyr::select(cell_geometry, grid_ids))
  target_polygons <- sf::st_sf(dplyr::select(geom, ID))
  sf::st_agr(data_source_cells) <- "constant"
  sf::st_agr(target_polygons) <- "constant"
  
  area_weights <- 
    intersectr::calculate_area_intersection_weights(data_source_cells, target_polygons)
  
  RNetCDF::close.nc(nc)
  
  out <- intersectr::execute_intersection(nc_file = nc_source,
                                          variable_name = nc_var,
                                          intersection_weights = area_weights,
                                          cell_geometry = cell_geometry,
                                          x_var = nc_coord_vars$X, 
                                          y_var = nc_coord_vars$Y, 
                                          t_var = nc_coord_vars$T,
                                          start_datetime = start_datetime, 
                                          end_datetime = end_datetime, 
                                          status = status)
  
  if(return_cell_geometry) {
    return(list(intersection = out, cell_geometry = cell_geometry))
  } else {
    return(list(intersection = out))
  }
}