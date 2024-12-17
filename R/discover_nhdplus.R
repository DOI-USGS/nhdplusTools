#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#' @param point sfc POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' \link[dataRetrieval]{get_nldi_sources} and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @param raindrop logical if \code{TRUE} will call a raindrop trace web service and
#' return will be the same as \link{get_raindrop_trace} with direction "none".
#' @return integer COMID or list containing COMID and raindrop trace.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(c(-76.874, 39.482)), crs = 4326)
#' discover_nhdplus_id(point)
#'
#' discover_nhdplus_id(point, raindrop = TRUE)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
#' discover_nhdplus_id(nldi_feature = nldi_nwis)
#' }
#'

discover_nhdplus_id <- function(point = NULL, nldi_feature = NULL, raindrop = FALSE) {
  if (!is.null(point)) {

    point <- check_point(point)

    if(raindrop) {
      out <- get_raindrop_trace(point, "none")

      return(out)
    }
    coords = sf::st_coordinates(point)

    comid <- tryCatch({

      URL <- paste0(get("geoserver_root", envir = nhdplusTools_env),
                    "wmadata",
                    "/ows",
                    "?service=WFS&request=GetFeature&version=1.0.0", # axis order switched in 1.1.0
                    "&typeName=catchmentsp&outputFormat=application/json",
                    "&CQL_FILTER=INTERSECTS(the_geom, POINT (",
                    coords[1], " ", coords[2], "))",
                    "&propertyName=featureid")

      d <- jsonlite::fromJSON(rawToChar(RETRY("GET", utils::URLencode(URL))$content))

      as.integer(d$features$properties$featureid)

    }, error = function(e) NULL)

    return(comid)
  } else if (!is.null(nldi_feature)) {

    nldi <- get_nldi_feature(nldi_feature)

    return(as.integer(nldi$comid))

  } else {
    stop("Must provide point or nldi_feature input.")
  }
}


