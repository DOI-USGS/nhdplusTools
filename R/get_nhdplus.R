#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#' @param point An sf POINT including crs as created by:
#' sf::st_sfc(sf::st_point(..,..), crs)
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' \link[dataRetrieval]{get_nldi_sources} and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @return integer COMID
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
discover_nhdplus_id <- function(point = NULL, nldi_feature = NULL) {
  if (!is.null(point)) {
    return(as.integer(query_usgs_geoserver(AOI = point, type = 'catchment')$featureid))
  } else if (!is.null(nldi_feature)) {

    nldi <- get_nldi_feature(nldi_feature)

    return(as.integer(nldi$comid))

  } else {
    stop("Must provide point or nldi_feature input.")
  }
}

#' @noRd
get_nhdplus_byid <- function(comids, layer, streamorder = NULL) {

 if(layer == "nhdflowline_network"){
   query_usgs_geoserver(ids = comids, type = "nhd", filter = streamorder_filter(streamorder))
 } else if(layer == "catchmentsp"){
   query_usgs_geoserver(ids = comids, type = "catchment")
 } else {
   stop("Layer must be one of catchmentsp, nhdflowline_network")
 }
}

#' @noRd
get_nhdplus_bybox <- function(box, layer, streamorder = NULL) {

  if(!layer %in% c("nhdarea", "nhdwaterbody", "nhdflowline_network", "catchmentsp")) {
    stop("Layer must be one of nhdarea, nhdwaterbody")
  }

  query_usgs_geoserver(AOI = box, type = dplyr::filter(query_usgs_geoserver(),
                                                       geoserver == layer)$user_call)
}
