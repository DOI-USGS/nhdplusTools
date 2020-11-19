#' @title Get National Hydrography Subsets (Multirealization)
#' @description Subsets NHD features by location (POINT), area (POLYGON),
#' or set of COMIDs. Multi realizations are supported allowing you to query
#' for flowlines, catchments, or outlets.
#' @param AOI sf (MULTI)POINT or (MULTI)POLYGON. An 'area of interest' can
#' be given as either a location (sf POINT) or area (sf POLYGON)
#' in any coordinate reference system.
#' @param comid numeric or character. Search for NHD features by COMID(s)
#' @param nwis  numeric or character. Search for NHD features by
#' colocated NWIS identifiers
#' @param streamorder numeric or character. Only return NHD flowlines with a
#' streamorder greater then or equal to this value
#' for input value and higher.
#' Only usable with AOI and flowline realizations.
#' @param realization character. What realization to return.
#' Default is flowline and options include: outlet, flowline, catchment,
#' and all
#' @return a single, or list, or simple feature objects
#' @examples
#'  point <- sf::st_sfc(sf::st_point(c(-119.845, 34.4146)), crs = 4326)
#'  get_nhdplus(point)
#'  get_nhdplus(point, realization = "catchment")
#'  get_nhdplus(point, realization = "all")

#'  get_nhdplus(comid = 101)
#'  get_nhdplus(nwis  = c(11120000, 11120500))

#'  area <- sf::st_as_sfc(sf::st_bbox(c(xmin = -119.8851, xmax =-119.8361,
#'  ymax = 34.42439, ymin = 34.40473), crs = 4326))

#'  get_nhdplus(area)
#'  get_nhdplus(area, realization = "flowline", streamorder = 3)
#' @importFrom methods is
#' @export

get_nhdplus <- function(AOI = NULL,
                         comid = NULL, nwis = NULL,
                         realization = "flowline",
                         streamorder = NULL){

  if(!is.null(AOI)){
    if(!methods::is(AOI,c("sf", 'sfc'))){
      stop("AOI must be of class sf.",.call = FALSE)
    }

    if(st_geometry_type(AOI) == "POINT"){
      coords = sf::st_coordinates(AOI)
      comid  = dataRetrieval::findNLDI(location = c(X = coords[1], Y = coords[2]))$origin$identifier
      AOI = NULL
    }
  }

  if(!is.null(AOI) & !is.null(c(nwis, comid))){
    stop("Either IDs (comid, nwis) or a spatial AOI can be passed.",.call = FALSE)
  }

  if(is.null(AOI) & is.null(c(nwis, comid))){
    stop("IDs (comid, nwis) or a spatial AOI must be passed.",.call = FALSE)
  }

  good_realizations = c("flowline", "catchment", 'outlet')
  if(realization == "all"){ realization = good_realizations}
  if(any(!realization %in% good_realizations)){
    stop(paste(realization, "not valid.\n Select from", paste(good_realizations, collapse = ", ")))
  }

  geoms = list()

  if(!is.null(nwis)){
    comid = c(unlist(lapply(nwis, extact_comid_nwis)), comid)
  }

  if("catchment" %in% realization){
    geoms$catch   <- query_usgs_geoserver(AOI = AOI, ids = comid, type = "catchment")
  }

  if(any(c("flowline", "outlet") %in% realization)){
    geoms$flowline      <- query_usgs_geoserver(AOI = AOI, ids = comid, type = 'nhd',
                                                filter = streamorder_filter(streamorder))

    if("outlet" %in% realization){
      geoms$outlet  <- geoms$flowline
      geoms$outlet$geometry <- st_geometry(
        get_node(geoms$outlet,
                 position = "end")
      )
    }
  }

  geoms = tc(geoms)
  if(length(geoms) == 1){ geoms = geoms[[1]]}

  return(geoms)

}

#' @title Query USGS Waterlabs Geoserver
#' @description Query the USGS waterlabs geoserver for spatial data by location,
#' area, or ID
#' @inheritParams get_nhdplus
#' @param ids character or numeric. A set of identifier(s) from the data
#' type requested, for example if NHD, then a set of COMID(s).
#' @param type character. Type of feature to return ('huc08','huc12', 'nhd',
#' 'catchment', 'waterbodies', 'gagesII')
#' @param filter character. An XML filter to pass to httr POST query
#' @return a sf object
#' @keywords internal
#' @noRd
#' @importFrom sf st_bbox read_sf st_zm
#' @importFrom httr POST RETRY write_disk
#' @importFrom dplyr filter

query_usgs_geoserver <- function(AOI = NULL, ids = NULL,
                                 type = NULL, filter = NULL){


  df = data.frame(server    =   'wmadata',
                  user_call = c('huc08','huc12',
                                'nhd','catchment', 'nhdarea',
                                'waterbodies',
                                'gagesII'),
                  geoserver = c("huc08","huc12",
                                "nhdflowline_network", "catchmentsp", 'nhdarea',
                                "nhdwaterbody",
                                "gagesii"),
                  ids       = c("huc8", "huc12",
                                "comid", "featureid", "comid",
                                "comid",
                                "staid"))

  if(is.null(type)){ return(df) }


  if(!is.null(AOI) & !is.null(ids)){
    stop("Either IDs or a spatial AOI can be passed.", .call = FALSE)
  }

  if(is.null(AOI) & is.null(ids)){
    stop("IDs or a spatial AOI must be passed.",.call = FALSE)
  }

  if(!(type %in% df$user_call)) {
    stop(paste("Type not available must be one of:",
               paste(df$user_call, collapse = ", ")),
         call. = FALSE)
  }

  here = dplyr::filter(df, .data$user_call == !!type)

  URL      <- paste0("https://labs.waterdata.usgs.gov/geoserver/",
                     here$server,
                     "/ows")

  sp_filter <- spatial_filter(AOI)

  if(!is.null(sp_filter) && grepl("^[?]", sp_filter)) {
    resp <- rawToChar(httr::RETRY("GET",
                                  paste0(URL, sp_filter),
                                  times = 10,
                                  pause_cap = 240)$content)
  } else {

    startXML <- paste0('<?xml version="1.0"?>',
                       '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                       ' xmlns:gml="http://www.opengis.net/gml" service="WFS"
                     version="1.1.0" outputFormat="application/json"',
                       ' xsi:schemaLocation="http://www.opengis.net/wfs
                     http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                       '<wfs:Query xmlns:feature="https://cida.usgs.gov/',
                       here$server,
                       '" typeName="feature:',
                       here$geoserver,
                       '" srsName="EPSG:4269">',
                       '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">')

    filterXML <- ifelse(is.null(AOI),
                        paste0(startXML,
                               id_filter(ids, here$ids)),
                        paste0(startXML,
                               '<ogc:And>',
                               filter, sp_filter,
                               '</ogc:And>',
                               '</ogc:Filter>',
                               '</wfs:Query>',
                               '</wfs:GetFeature>')
    )

    resp <- rawToChar(httr::RETRY("POST",
                                  URL,
                                  body = filterXML,
                                  times = 10, pause_cap = 240)$content)
  }

  out = tryCatch({sf::st_zm(sf::read_sf(resp))},
                 error   = function(e){ return(NULL) },
                 warning = function(w){ return(NULL) }
  )

  if(any(is.null(out), nrow(out) == 0)) {
    out = NULL
    warning(paste("No features found in this AOI from layer,", here$user_call))
  }

  return(out)
}

#' @title Construct a BBOX spatial filter for geoservers
#' @description From an 'area of intferest' object (sf POINT or POLYGON),
#' generate a WMS BBOX filter to pass to a geoserver.
#' @inheritParams get_nhdplus
#' @return a character string XML filter
#' @keywords internal
#' @noRd
#' @importFrom sf st_geometry_type st_buffer st_transform st_bbox

spatial_filter  <- function(AOI){

  if(is.null(AOI)){ return(NULL)}

  AOI <- sf::st_transform(AOI, 4269)

  if (as.character(sf::st_geometry_type(AOI)) == "POINT") {

    AOI <- sf::st_coordinates(AOI)

    return(paste0("?service=WFS",
                  "&version=1.0.0",
                  "&request=GetFeature",
                  "&typeName=wmadata:catchmentsp",
                  "&outputFormat=application%2Fjson",
                  "&srsName=EPSG:4326",
                  "&CQL_FILTER=INTERSECTS%28the_geom,%20POINT%20%28",
                  AOI[1], "%20", AOI[2], "%29%29"))
    }

  bb <- sf::st_bbox(AOI)

  paste0('<ogc:BBOX>',
         '<ogc:PropertyName>the_geom</ogc:PropertyName>',
         '<gml:Envelope srsName="urn:x-ogc:def:crs:EPSG:4269">',
         '<gml:lowerCorner>', bb$ymin, " ", bb$xmin, '</gml:lowerCorner>',
         '<gml:upperCorner>', bb$ymax, " ", bb$xmax, '</gml:upperCorner>',
         '</gml:Envelope>',
         '</ogc:BBOX>')

}

#' @title Construct an attribute filter for geoservers
#' @description From a user provided vector of IDs and an attribute name,
#' generate a WMS filter to pass to a geoserver.
#' @inheritParams query_usgs_geoserver
#' @param name character. The name of the id attribute field in the desired dataset
#' @return a character string
#' @keywords internal
#' @noRd

id_filter  <- function(ids, name = "comid"){

  if(is.null(ids)){ return(NULL)}

  startXML <- paste0('<ogc:Or>',
                     '<ogc:PropertyIsEqualTo>',
                     '<ogc:PropertyName>',
                     name,
                     '</ogc:PropertyName>',
                     '<ogc:Literal>')

  tmp <- paste0('</ogc:Literal>',
                '</ogc:PropertyIsEqualTo>',
                '<ogc:PropertyIsEqualTo>',
                '<ogc:PropertyName>',
                name,
                '</ogc:PropertyName>',
                '<ogc:Literal>')

  endXML <- paste0('</ogc:Literal>',
                   '</ogc:PropertyIsEqualTo>',
                   '</ogc:Or>',
                   '</ogc:Filter>',
                   '</wfs:Query>',
                   '</wfs:GetFeature>')

  paste0(startXML, paste0(ids, collapse = tmp), endXML)

}

#' @title Stream Order Filter
#' @description Generate a WMS filter to pass to a geoserver. That constrains
#' the returned flowlines to only those with a stream order
#' greater then or equal to the provided value.
#' @inheritParams get_nhdplus
#' equal to this value will be returned
#' @return a character string
#' @keywords internal
#' @noRd

streamorder_filter <- function(streamorder){

  if(is.null(streamorder)){ return(NULL)}

  paste0('<ogc:PropertyIsGreaterThan>',
         '<ogc:PropertyName>streamorde</ogc:PropertyName>',
         '<ogc:Literal>', streamorder - 1, '</ogc:Literal>',
         '</ogc:PropertyIsGreaterThan>')
}

#' @title Trim and Cull NULLs
#' @description Remove NULL arguments from a list
#' @param x a list
#' @keywords internal
#' @return a list
#' @noRd

tc <- function(x) {
  Filter(Negate(is.null), x)
}

#' @title Identify NHD features by collocated NWIS ID(s)
#' @description Use the NLDI to identify the COMIDs associated
#' with a given NWIS ID.
#' @param nwis character or numeric. A vector of USGS NWIS id(s)
#' @keywords internal
#' @return a vector of COMIDs
#' @noRd
#' @importFrom httr RETRY GET
#' @importFrom jsonlite fromJSON

extact_comid_nwis <- function(nwis){
  # We could export this from dataRetrieval dataRetrieval:::pkg.env$nldi_base
  #but currently its not...
  baseURL  <- "https://labs.waterdata.usgs.gov/api/nldi/linked-data/"
  url      <-  paste0(baseURL, "nwissite/USGS-", nwis)
  c        <-  rawToChar(httr::RETRY("GET",
                                     url,
                                     times = 10,
                                     pause_cap = 240)$content)
  f.comid  <-  jsonlite::fromJSON(c, simplifyVector = TRUE)
  f.comid$features$properties$comid
}

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
