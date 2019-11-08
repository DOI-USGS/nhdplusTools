#' @title Plot NHDPlus
#' @description Given a list of outlets, get their basin boundaries and network and return a plot.
#' @param outlets character vector of site ids in the format "USGS-01234567"
#' @param bbox vector of map limits (xmin, ymin, xmax, ymax) that can be coerced into an object of class bbox.
#' @param streamorder integer only streams of order greater than or equal will be returned
#' @param gpkg path and file with .gpkg ending. If NA, no file is written.
#' @export
#' @examples
#' plot_nhdplus("USGS-05428500")
#'
plot_nhdplus <- function(nwissite = NA, outlets = NA, bbox = NA, streamorder = NA, nhdplus_data = NA, gpkg = NA) {

  pd <- get_plot_data(nwissite, outlets, bbox, streamorder, nhdplus_data, gpkg)

  prettymapr::prettymap({
    rosm::osm.plot(pd$plot_bbox, type = "cartolight", quiet = TRUE)
    # plot(gt(catchment), lwd = 0.5, col = NA, border = "grey", add = TRUE)
    graphics::plot(gt(pd$basin), lwd = 1, col = NA, border = "black", add = TRUE)
    graphics::plot(gt(pd$flowline), lwd = 1, col = "blue", add = TRUE)
    graphics::plot(gt(pd$outlets), col = "grey40", pch = 17, add = TRUE)
  },
  drawarrow = TRUE)
}

get_plot_data <- function(nwissite = NA, outlets = NA, bbox = NA, streamorder = NA, nhdplus_data = NA, gpkg = NA) {

  if(!is.na(bbox) | !is.na(gpkg)) {
    # Only outlets and streamorder implemented so far.
  }

  if(!is.na(streamorder) && is.na(nhdplus_data))
    stop("Streamoder not available without specifying nhdplus_data source. Can't filter.")

  if(!is.na(nwissite)) {
    if(!is.na(outlets)) stop("nwissite or outlets supported, not both")
    outlets <- lapply(nwissite, function(x) list("nwissite", x))
  }

  if(!is.list(outlets[[1]])) outlets <- list(outlets)
  outlets <- lapply(outlets, check_nldi_feature)

  if(length(outlets) > 1) stop("only length 1 outlet list supported so far")

  if(!is.na(nhdplus_data)) {
    fline_layer = get_flowline_layer_name()
    catchment_layer <- get_catchment_layer_name(simplified = TRUE, nhdplus_data)

    flowline <- sf::st_set_geometry(sf::read_sf(nhdplus_data, fline_layer), NULL)

    outlets <- lapply(outlets, get_nldi_feature)

    outlet_comids <- lapply(outlets, function(x) x$comid)

    all_comids <- lapply(outlet_comids, get_UT, network = flowline)

    subsets <- lapply(all_comids, subset_nhdplus, nhdplus_data = nhdplus_data, status = FALSE)

    flowline <- sf::st_zm(do.call(rbind, lapply(subsets, function(x) x[[fline_layer]])))
    catchment <- do.call(rbind, lapply(subsets, function(x) x[[catchment_layer]]))
    basin <- do.call(rbind, lapply(subsets, make_basin, catchment_layer = catchment_layer))

    outlets <- do.call(rbind, outlets)
    if(!any(grepl("sfc_POINT", class(sf::st_geometry(outlets)))))
      sf::st_geometry(outlets) <- suppressWarnings(sf::st_centroid(sf::st_geometry(outlets)))

  } else {
    basin <- do.call(rbind, lapply(outlets, get_nldi_basin))

    flowline <- do.call(rbind, lapply(outlets, navigate_nldi,
                                      mode = "UT", data_source = ""))

    outlets <- do.call(rbind, lapply(outlets, get_nldi_feature))

    catchment <- NA
  }

  if(is.na(bbox)) {
    bbox <- sp_bbox(sf::st_transform(basin, 4326))
  }

  if(!is.na(streamorder) && "StreamOrde" %in% names(flowline)) {
    flowline <- flowline[flowline$StreamOrde >= streamorder, ]
  }

  return(list(plot_bbox = bbox, outlets = outlets, flowline = flowline,
              basin = basin, catchment = catchment))
}

gt <- function(x) sf::st_geometry(sf::st_transform(x, 3857))

sp_bbox <- function(g) {
  matrix(as.numeric(sf::st_bbox(g)),
         nrow = 2, dimnames = list(c("x", "y"),
                                   c("min", "max")))
}

make_basin <- function(x, catchment_layer) {
  x <- x[[catchment_layer]]
  sf::st_precision(x) <- 10000 # kills slivers
  sf::st_sf(geom = sf::st_union(sf::st_geometry(x)))
}
