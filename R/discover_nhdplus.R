#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#' @param point An sf POINT including crs as created by:
#' sf::st_sfc(sf::st_point(..,..), crs)
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' \link[dataRetrieval]{get_nldi_sources} and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @param raindrop logical if \code{TRUE} will call a raindrop trace web service and
#' return will be a \code{list} containing the COMID and a \code{sfg} linestring trace
#' to the nearest flowline. Ignored if \code{nldi_feature} is supplied rather than
#' \code{point}.
#' @return integer COMID or list containing COMID and raindrop trace.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
#' discover_nhdplus_id(point)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
#' discover_nhdplus_id(nldi_feature = nldi_nwis)
#' }
#'

discover_nhdplus_id <- function(point = NULL, nldi_feature = NULL, raindrop = FALSE) {
  if (!is.null(point)) {
    coords = sf::st_coordinates(point)
    comid  = dataRetrieval::findNLDI(location = c(X = coords[1],
                                                  Y = coords[2]))$identifier
    return(as.integer(comid))
  } else if (!is.null(nldi_feature)) {

    nldi <- get_nldi_feature(nldi_feature)

    return(as.integer(nldi$comid))

  } else {
    stop("Must provide point or nldi_feature input.")
  }
}

get_raindrop_trace <- function(point) {

}

make_json_input <- function(p) {
  jsonlite::toJSON(list(inputs = list(list(id = "lat",
                                           type = "text/plain",
                                           value = p[2]),
                                      list(id = "lng",
                                           type = "text/plain",
                                           value = p[1]))),
                   pretty = TRUE, auto_unbox = TRUE)
}
