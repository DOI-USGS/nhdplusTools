#' @title Query USGS Geoserver
#' @description Query the USGS  geoserver for spatial data by location,
#' area, or ID.
#' @details The returned object(s) will have the same
#' Spatial Reference System (SRS) as the input AOI. If a individual or set of
#' IDs are used to query, then the default geoserver CRS of EPSG:4326 is
#' preserved. In all cases, a user-defined SRS can be passed to \code{t_srs}
#' which will override all previous SRS's (either input or default).
#' All buffer and distance operations are handled internally using in
#' EPSG:5070 Albers Equal Area projection
#' @param AOI sf (MULTI)POINT or (MULTI)POLYGON. An 'area of interest' can
#' be provided as either a location (sf POINT) or area (sf POLYGON)
#' in any Spatial Reference System.
#' @param ids character or numeric. A set of identifier(s) from the data
#' type requested, for example if NHD, then a set of COMID(s).
#' @param type character. Type of feature to return
#' ('huc08','huc12', 'nhd', 'catchment', 'waterbodies', 'gagesII').
#' If NULL (default) a data.frame of available resources is returned
#' @param filter character. An XML filter to pass to httr POST query
#' @param t_srs  character (PROJ string or EPSG code) or numeric (EPSG code).
#' A user specified - target -Spatial Reference System (SRS/CRS) for returned objects.
#' Will default to the CRS of the input AOI if provided, and to 4326 for ID requests.
#' @param buffer numeric. The amount (in meters) to buffer a POINT AOI by for an
#' extended search. Default = 0.5
#' @return a simple features (sf) object
#' @keywords internal
#' @importFrom sf st_crs st_geometry_type st_buffer st_transform st_zm read_sf st_bbox st_as_sfc
#' @importFrom httr POST RETRY
#' @importFrom dplyr filter

query_usgs_geoserver <- function(AOI = NULL,  ids = NULL,
                                 type = NULL, filter = NULL,
                                 t_srs = NULL,
                                 buffer = 0.5){

  # If t_src is not provided set to AOI CRS
  if(is.null(t_srs)){ t_srs  <- sf::st_crs(AOI)}
  # If AOI CRS is NA (e.g st_crs(NULL)) then set to 4326
  if(is.na(t_srs))  { t_srs  <- 4326}

  source <- data.frame(server = 'wmadata',
                       user_call  = c('huc08','huc12',
                                      'nhd','catchment', 'nhdarea',
                                      'nonnetwork',
                                      'waterbodies',
                                      'gagesII', "gagesII-basin"),
                       geoserver  = c("huc08","huc12",
                                      "nhdflowline_network", "catchmentsp", 'nhdarea',
                                      "nhdflowline_nonnetwork",
                                      "nhdwaterbody",
                                      "gagesii", "gagesii_basins"),
                       ids        = c("huc8", "huc12",
                                      "comid", "featureid", "comid",
                                      "comid",
                                      "comid",
                                      "staid", "gage_id"))

  if(is.null(type)){ return(source) }

  if(!is.null(AOI) & !is.null(ids)){
    # Check if AOI and IDs are both given
    stop("Either IDs or a spatial AOI can be passed.", .call = FALSE)
  } else if(is.null(AOI) & is.null(ids)){
    # Check if AOI and IDs are both NULL
    stop("IDs or a spatial AOI must be passed.",.call = FALSE)
  } else if(!(type %in% source$user_call)) {
    # Check that "type" is valid
    stop(paste("Type not available must be one of:",
               paste(source$user_call, collapse = ", ")),
         call. = FALSE)
  }

  if(!is.null(AOI)){
    if(sf::st_geometry_type(AOI) == "POINT"){
      # If input is a POINT, buffer by 1/2 meter (in equal area projection)
      AOI = sf::st_buffer(sf::st_transform(AOI, 5070), buffer) %>%
        sf::st_bbox() %>% sf::st_as_sfc() %>% sf::st_make_valid()
    }
  }

  here     <- dplyr::filter(source, .data$user_call == !!type)

  URL      <- paste0(get("geoserver_root", envir = nhdplusTools_env),
                     here$server,
                     "/ows")

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
                             filter,
                             spatial_filter(AOI, type = here$geoserver),
                             '</ogc:And>',
                             '</ogc:Filter>',
                             '</wfs:Query>',
                             '</wfs:GetFeature>')
  )

  tryCatch({
    resp <- rawToChar(httr::RETRY("POST",
                                  URL,
                                  body = filterXML)$content)
  }, error = function(e) {
    warning("Something went wrong trying to access a service.")
    return(NULL)
  })

  out <- tryCatch({check_valid(sf::st_zm(sf::read_sf(resp)), out_prj = t_srs)},
                 error   = function(e){ return(NULL) })

  if(any(is.null(out), nrow(out) == 0)) {

    out = NULL

  } else if(!is.null(AOI)){

    out = sf::st_filter(sf::st_transform(out, t_srs),
                        sf::st_transform(AOI, t_srs))
    if(nrow(out) == 0){
      out = NULL
    }
  }

  if(!is.null(out)) {
    return(out)
  } else {
    warning(paste("No", here$user_call, "features found"), call. = FALSE)
    NULL
  }

}

#' @title Construct a BBOX spatial filter for geoservers
#' @description From an 'area of intferest' object (sf POINT or POLYGON),
#' generate a WMS BBOX filter to pass to a geoserver.
#' @inheritParams get_nhdplus
#' @param type needed if we want to use CQL, not for BBOX. Left for posterity
#' @return a character string XML filter
#' @keywords internal
#' @noRd
#' @importFrom sf st_geometry_type st_buffer st_transform st_bbox

spatial_filter  <- function(AOI, type = 'catchmentsp'){

  # "The current GeoServer implementation will return all the streets whose
  # ENVELOPE overlaps the BBOX provided. This may includes some nearby
  # streets that do not strictly fall within the bounding box.
  # This is a fast response - if a more accurate but slower response is required,
  # use Intersects or Within."

  # if (as.character(sf::st_geometry_type(AOI)) == "POINT") {
  #
  #   AOI <- sf::st_coordinates(AOI)
  #
  #   return(paste0("?service=WFS",
  #                 "&version=1.0.0",
  #                 "&request=GetFeature",
  #                 "&typeName=wmadata:", type,
  #                 "&outputFormat=application%2Fjson",
  #                 "&srsName=EPSG:4326",
  #                 "&CQL_FILTER=INTERSECTS%28the_geom,%20POINT%20%28",
  #                 AOI[1], "%20", AOI[2], "%29%29"))
  #   }


  bb <- sf::st_transform(AOI, 4326) %>%
    sf::st_bbox()

  paste0('<ogc:BBOX>',
         '<ogc:PropertyName>the_geom</ogc:PropertyName>',
         '<gml:Envelope srsName="urn:x-ogc:def:crs:EPSG:4326">',
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
  c        <-  rawToChar(httr::RETRY("GET", url)$content)
  f.comid  <-  jsonlite::fromJSON(c, simplifyVector = TRUE)
  f.comid$features$properties$comid
}
