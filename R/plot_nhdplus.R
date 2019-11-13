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

  if(!all(is.na(nwissite))) {
    if(all(is.na(outlets))) outlets <- list()
    outlets <- c(outlets, lapply(nwissite, make_nwis_nldi_feature))
  }

  if(!is.list(outlets)) outlets <- list(outlets)
  outlets <- lapply(outlets, check_nldi_feature)

  if(!is.na(nhdplus_data)) {
    fline_layer = get_flowline_layer_name()
    catchment_layer <- get_catchment_layer_name(simplified = TRUE, nhdplus_data)

    flowline <- sf::st_zm(sf::read_sf(nhdplus_data, fline_layer))

    # For the "COMID" inputs we don't have to go to the NLDI,
    nexus <- lapply(outlets, get_comid_outlets, flowline = flowline)

    flowline <- sf::st_set_geometry(flowline, NULL)

    empty <- sapply(nexus, nrow) == 0
    outlets <- outlets[empty]
    nexus <- nexus[!empty]

    if(length(outlets) > 0) {
      nexus <- c(nexus, lapply(outlets, get_outlet_from_nldi))
    }

    all_comids <- lapply(nexus, function(x) get_UT(flowline, x$comid))

    subsets <- lapply(all_comids, subset_nhdplus, nhdplus_data = nhdplus_data, status = FALSE)

    flowline <- sf::st_zm(do.call(rbind, lapply(subsets, function(x) x[[fline_layer]])))
    catchment <- do.call(rbind, lapply(subsets, function(x) x[[catchment_layer]]))
    basin <- do.call(rbind, lapply(subsets, make_basin, catchment_layer = catchment_layer))

    nexus <- do.call(rbind, nexus)

    if(!any(grepl("sfc_POINT", class(sf::st_geometry(nexus)))))
      sf::st_geometry(nexus) <- suppressWarnings(sf::st_centroid(sf::st_geometry(nexus)))

  } else {
    basin <- do.call(rbind, lapply(outlets, get_nldi_basin))

    flowline <- do.call(rbind, lapply(outlets, navigate_nldi,
                                      mode = "UT", data_source = ""))

    nexus <- do.call(rbind, lapply(outlets, get_outlet_from_nldi))

    catchment <- NA
  }

  if(is.na(bbox)) {
    bbox <- sp_bbox(sf::st_transform(basin, 4326))
  }

  if(!is.na(streamorder) && "StreamOrde" %in% names(flowline)) {
    flowline <- flowline[flowline$StreamOrde >= streamorder, ]
  }

  return(list(plot_bbox = bbox, outlets = nexus, flowline = flowline,
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

get_comid_outlets <- function(o, flowline) {
  if(o$featureSource %in% c("comid", "COMID")) {
    f <- flowline[flowline$COMID == o$featureID, ][c("COMID", attr(flowline, "sf_column"))]
    names(f)[names(f) == "COMID"] <- "comid"
    return(make_point(f))
  }
  return(dplyr::tibble())
}

get_outlet_from_nldi <- function(outlet) {
  make_point(get_nldi_feature(outlet))
}

make_point <- function(x, crs = 4326) {
  sf::st_geometry(x) <-
    suppressWarnings(sf::st_centroid(sf::st_geometry(x)))
  x <- set_geom_name(x)
  x <- sf::st_transform(x, crs)
  return(dplyr::select(x, "comid"))
}

set_geom_name <- function(x, new_name = "geom") {
  g <- attr(x, "sf_column")
  names(x)[names(x) == g] <- new_name
  attr(x, "sf_column") <- new_name
  x
}

make_nwis_nldi_feature <- function(x) {
  if(!grepl("^USGS-.*", x)) x <- paste0("USGS-", x)
  list("nwissite", x)
}
