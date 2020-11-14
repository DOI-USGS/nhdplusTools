#' @title 	Retrieve features from the \href{https://labs.waterdata.usgs.gov/api/nldi/swagger-ui/index.html?configUrl=/api/nldi/v3/api-docs/swagger-config}{NLDI}
#' @description Provides a formal query to the
#' \href{https://labs.waterdata.usgs.gov/about-nldi/index.html}{Network Linked Data Index}.
#' The function is useful for topology and location based feature discovery.
#' A user must supply a starting feature, and can add optional navigation direction(s),
#' and features to identify on the navigated network.
#' Valid starting options can be given by one of the following arguments: comid, nwis, huc12,
#'  wqp, location, and origin.
#' @param comid an NHDPlusV2 COMID
#' @param nwis  a USGS NWIS siteID
#' @param wqp a water quality point ID
#' @param huc12 a HUC12 ID
#' @param location Coordinate pair in WGS84 GCS provided as a numeric vector ordered lng/lat
#' @param origin a named list specifying a feature type and ID (e.g. list("comid" = 101))
#' @param nav where to navigate from the starting point ("UM", "UT", DM", "DD")
#' @param find what resources to find along the navigation path(s) (see get_nldi_sources()$source). Can also include 'basin', which will return the upstream basin of the starting feature
#' @param distance_km how far to look along the navigation path in kilometers (default = 100)
#' @return a list of simple feature objects (points, lines, polygons)
#' @export
#' @importFrom dataRetrieval findNLDI
#' @importFrom sf st_as_sf
#' @examples
#'
#' # Find Features / Define origin features
#'
#' ## Find feature by COMID
#'  find_nldi(comid = 101)
#'
#' ## Find feature by NWIS ID
#'  find_nldi(nwis = '11120000')
#'
#' ## Find feature by WQP ID
#'  find_nldi(wqp = 'USGS-04024315')
#'
#' ## Find feature by LOCATION
#'  find_nldi(location = c(-115,40))
#'
#' ## GENERAL ORIGIN: COMID
#'  find_nldi(origin = list("comid" = 101))
#'
#' ## GENERAL ORIGIN: WaDE
#'  find_nldi(origin = list("wade" = 'CA_45206'))
#'
#' # Navigation
#' # UPPER MAINSTEM of USGS-11120000
#'  str(find_nldi(nwis = '11120000', nav = "UM"), max.level = 1)
#'
#' # MULTI-REQUEST
#' # UPPER MAINSTEM and TRIBUTARY of USGS-11120000
#'  str(find_nldi(nwis = '11120000', nav = c("UT", "UM")), max.level = 1)
#'
#' # Discover Features
#'
#' ## Find feature(s) on the upper tributary of USGS-11120000
#'  str(find_nldi(nwis = '11120000', nav = "UT", find = c("nwis", "wqp")), max.level = 1)
#'
#' ## Find upstream basin boundary of USGS-11120000
#'  str(find_nldi(nwis = '11120000',  find = "basin"), max.level = 1)
#'
#' # Control Distance
#' ## Limit search to 100 km
#'  str(find_nldi(comid = 101, nav = "DM", find = c("nwis", "wqp"), distance_km = 100), max.level = 1)

find_nldi <- function(comid = NULL,
                     nwis = NULL,
                     wqp = NULL,
                     huc12 = NULL,
                     location = NULL,
                     origin = NULL,
                     nav = NULL,
                     find = NULL,
                     distance_km = 100){

  resp <- dataRetrieval::findNLDI(comid = comid, nwis = nwis, wqp = wqp, huc12 = huc12,
                                 location = location, origin = origin, nav = nav,
                                 find = find, distance_km = distance_km, no_sf = FALSE)

  lapply(resp, sf::st_as_sf)
}
