#' @title Plot NHDPlus
#' @description Given a list of sites, get their basin boundaries and network and return a plot.
#' @param sites character vector of site ids in the format "USGS-01234567"
#' @param bbox vector of map limits (xmin, ymin, xmax, ymax) that can be coerced into an object of class bbox.
#' @param streamorder integer only streams of order greater than or equal will be returned
#' @param gpkg path and file with .gpkg ending. If NA, no file is written.
#' @export
#' @examples
#' plot_nhdplus("USGS-05428500")
#'
plot_nhdplus <- function(sites, bbox = NA, streamorder = NA, gpkg = NA) {

  if(!is.na(bbox) | !is.na(streamorder) | !is.na(gpkg)) {
    # Only sites implemented so far.
  }

  pd <- get_plot_data(sites, bbox, streamorder, gpkg)

  prettymapr::prettymap({
    rosm::osm.plot(pd$plot_bbox, type = "cartolight", quiet = TRUE)
    # plot(gt(catchment), lwd = 0.5, col = NA, border = "grey", add = TRUE)
    graphics::plot(gt(pd$basin), lwd = 1, col = NA, border = "black", add = TRUE)
    graphics::plot(gt(pd$flowline), lwd = 1, col = "blue", add = TRUE)
    graphics::plot(gt(pd$sites), col = "grey40", pch = 17, add = TRUE)
  },
  drawarrow = TRUE)
}

get_plot_data <- function(sites, bbox = NA, streamorder = 3, gpkg = NA) {
  basin <- do.call(rbind,
                   lapply(sites, function(x)
                     get_nldi_basin(list(featureSource = "nwissite",
                                         featureID = x))))

  plot_bbox <- sp_bbox(sf::st_transform(basin, 4326))

  flowline <- do.call(rbind,
                      lapply(sites, function(x)
                        navigate_nldi(list(featureSource = "nwissite",
                                           featureID = x),
                                      mode = "UT",
                                      data_source = "")))

  sites <- do.call(rbind,
                   lapply(sites, function(x)
                     get_nldi_feature(list(featureSource = "nwissite",
                                           featureID = x))))

  return(list(plot_bbox = plot_bbox, sites = sites, flowline = flowline, basin = basin))
}

gt <- function(x) sf::st_geometry(sf::st_transform(x, 3857))

sp_bbox <- function(g) {
  matrix(as.numeric(sf::st_bbox(g)),
         nrow = 2, dimnames = list(c("x", "y"),
                                   c("min", "max")))
}
