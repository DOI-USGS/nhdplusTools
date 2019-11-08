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
plot_nhdplus <- function(outlets, bbox = NA, streamorder = NA, nhdplus_data = NA, gpkg = NA) {

  if(!is.na(bbox) | !is.na(gpkg)) {
    # Only outlets and streamorder implemented so far.
  }

  if(!is.na(streamorder) && is.na(nhdplus_data))
    stop("Streamoder not available without specifying nhdplus_data source. Can't filter.")

  pd <- get_plot_data(outlets, bbox, streamorder, nhdplus_data, gpkg)

  if(!is.na(streamorder))
    pd$flowline <- filter(pd$flowline, pd$StreamOrde >= streamorder)

  prettymapr::prettymap({
    rosm::osm.plot(pd$plot_bbox, type = "cartolight", quiet = TRUE)
    # plot(gt(catchment), lwd = 0.5, col = NA, border = "grey", add = TRUE)
    graphics::plot(gt(pd$basin), lwd = 1, col = NA, border = "black", add = TRUE)
    graphics::plot(gt(pd$flowline), lwd = 1, col = "blue", add = TRUE)
    graphics::plot(gt(pd$outlets), col = "grey40", pch = 17, add = TRUE)
  },
  drawarrow = TRUE)
}

get_plot_data <- function(outlets, bbox = NA, streamorder = 3, nhdplus_data = NA, gpkg = NA) {
  basin <- do.call(rbind,
                   lapply(outlets, function(x)
                     get_nldi_basin(list(featureSource = "nwissite",
                                         featureID = x))))

  plot_bbox <- sp_bbox(sf::st_transform(basin, 4326))

  flowline <- do.call(rbind,
                      lapply(outlets, function(x)
                        navigate_nldi(list(featureSource = "nwissite",
                                           featureID = x),
                                      mode = "UT",
                                      data_source = "")))

  outlets <- do.call(rbind,
                   lapply(outlets, function(x)
                     get_nldi_feature(list(featureSource = "nwissite",
                                           featureID = x))))

  return(list(plot_bbox = plot_bbox, outlets = outlets, flowline = flowline, basin = basin))
}

gt <- function(x) sf::st_geometry(sf::st_transform(x, 3857))

sp_bbox <- function(g) {
  matrix(as.numeric(sf::st_bbox(g)),
         nrow = 2, dimnames = list(c("x", "y"),
                                   c("min", "max")))
}
