#' @title Waterlabs Geoserver Query
#' @description Query the USGS waterlabs server
#' @param AOI an 'area of interest' object (sf POINT or POLYGON)
#' @param ids an identifier from the data type requested, for example if NHD, then a COMID.
#' @param type the type of feature to return ('huc08','huc12', 'nhd', 'catchments', 'waterbodies', 'gagesII')
#' @param filter a XML filter to pass to httr POST query
#' @return a sf object
#' @keywords internal
#' @noRd
#' @importFrom sf st_bbox read_sf st_zm
#' @importFrom httr POST write_disk
#' @importFrom dplyr filter
#' @importFrom utils unzip

query_waterlabs <- function(AOI = NULL, ids = NULL, type, filter = NULL){

  if(!is.null(AOI) & !is.null(ids)){
    stop("Either IDs or a spatial AOI can be passed.", .call = FALSE)
  }

  if(is.null(AOI) & is.null(ids)){
    stop("IDs or a spatial AOI must be passed.",.call = FALSE)
  }

  df = data.frame(server = 'wmadata',
                  user_call = c('huc08','huc12','nhd','catchments',
                                'waterbodies',
                                'gagesII'),
                  geoserver =  c("huc08","huc12","nhdflowline_network", "catchmentsp",
                                 "nhdwaterbody",
                                 "gagesii"),
                  ids = c("huc8", "huc12", "comid", "featureid", "comid", "staid"))

  if(!(type %in% df$user_call)) {
    stop(paste("Type not available must be one of:",
               paste(df$user_call, collapse = ", ")),
         call. = FALSE)
  }

  here = dplyr::filter(df, user_call == !!type)

  URL      <- paste0("https://labs.waterdata.usgs.gov/geoserver/", here$server, "/ows")

  startXML <- paste0('<?xml version="1.0"?>',
                     '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                     ' xmlns:gml="http://www.opengis.net/gml" service="WFS"
                     version="1.1.0" outputFormat="shape-zip"',
                     ' xsi:schemaLocation="http://www.opengis.net/wfs
                     http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                     '<wfs:Query xmlns:feature="https://cida.usgs.gov/',
                     here$server, '" typeName="feature:', here$geoserver,
                     '" srsName="EPSG:4269">',
                     '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">')

  filterXML <- ifelse(is.null(AOI),
                      paste0(startXML, id_filter(ids, here$ids)),
                      paste0(startXML,
                             '<ogc:And>',
                             filter, spatial_filter(AOI),
                             '</ogc:And>',
                             '</ogc:Filter>',
                             '</wfs:Query>',
                             '</wfs:GetFeature>')
  )

  tmpdir  <- tempdir()
  tmpfile <- file.path(tmpdir, "spatial_query.zip")
  if(file.exists(tmpfile)){file.remove(tmpfile)}

  file <- httr::POST(URL, body = filterXML, httr::write_disk(tmpfile, overwrite=TRUE))

  utils::unzip(tmpfile, exdir = tmpdir)

  out = tryCatch({sf::st_zm(sf::read_sf(tmpdir, here$geoserver))},
                 error   = function(e){ return(NULL) },
                 warning = function(w){ return(NULL) }
  )

  if(any(is.null(out), nrow(out) == 0)) {
    out = NULL
    warning("No features found in this AOI.")
  }

  return(out)

}

#' @title Spatial Filter for Geoserver
#' @description From an 'area of interest' object (sf POINT or POLYGON), generate a WMS spatial filter
#' @param AOI an 'area of interest' object (sf POINT or POLYGON)
#' @return a character string
#' @keywords internal
#' @noRd
#' @importFrom sf st_geometry_type st_buffer st_transform st_bbox

spatial_filter  <- function(AOI){

  if(is.null(AOI)){ return(NULL)}

  AOI_type <- as.character(sf::st_geometry_type(AOI))

  if (AOI_type == "POINT") { AOI <- sf::st_buffer(
    sf::st_transform(AOI, 5070), 1) }

  bb <- sf::st_bbox(sf::st_transform(AOI, 4269))

  paste0('<ogc:BBOX>',
         '<ogc:PropertyName>the_geom</ogc:PropertyName>',
         '<gml:Envelope srsName="urn:x-ogc:def:crs:EPSG:4269">',
         '<gml:lowerCorner>', bb$ymin, " ", bb$xmin, '</gml:lowerCorner>',
         '<gml:upperCorner>', bb$ymax, " ", bb$xmax, '</gml:upperCorner>',
         '</gml:Envelope>',
         '</ogc:BBOX>')

}

#' @title Attribute Filter for Geoserver
#' @description From a vecotor of IDs and an attribute name, build a WMS filter
#' @param id an identifier from the data type requested, for example if NHD, then a COMID.
#' @param name the name of the id/attribute in the desired dataset
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
#' @description Subset spatial filters with a stream order. The returned results will be any flowline features with a stream order greeted then equal to the provided value.
#' @param streamorder Flowlines with a streamorder greated then or equal to this value will be returned
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

#' @title Identify NHD features via collocated NWIS IDs
#' @description Use the NLDI to identify the COMIDs associated with a given NWIS ID.
#' @param nwis a vector of NWIS id(s)
#' @keywords internal
#' @return a vector of COMIDs
#' @noRd
#' @importFrom httr RETRY GET
#' @importFrom jsonlite fromJSON

extact_comid_nwis <- function(nwis){
  # We could export this from dataRetrieval dataRetrieval:::pkg.env$nldi_base but currently its not...
  baseURL  <- "https://labs.waterdata.usgs.gov/api/nldi/linked-data/"
  url      <-  paste0(baseURL, "nwissite/USGS-", nwis)
  c        <-  rawToChar(httr::RETRY("GET", url, times = 10, pause_cap = 240)$content)
  f.comid  <-  jsonlite::fromJSON(c, simplifyVector = TRUE)
  f.comid$features$properties$comid
}

#' @title Identify NHD outlets from flowlines
#' @description Convert flowline NHD realization to outlet POINT objects.
#' @param flowlines a sf LINESTRING or MULTILINESTRING object
#' @return the input flowlines object with geometry changed to POINT objects representing the oulets
#' @export
#' @importFrom sf st_geometry_type st_cast st_transform st_crs st_line_sample st_zm

find_outlets <- function(flowlines){
  AOI_type = unique(sf::st_geometry_type(flowlines))
  if(!AOI_type %in% c("MULTILINESTRING", "LINESTRING")){
    stop("outlets can only be extracted from flowlines", call. = FALSE)
  } else {
    flowlines$geometry = suppressWarnings(
      flowlines$geometry %>%
        sf::st_cast("LINESTRING") %>%
        sf::st_transform(5070) %>%
        sf::st_line_sample(sample = 1) %>%
        sf::st_cast("POINT") %>%
        sf::st_transform(st_crs(flowlines)) %>%
        sf::st_zm()
    )

    return(flowlines)

  }
}

#' @title Find National Hydrography Subsets (Multirealization)
#' @description Subsets NHD features by location (POINT), area (POLYGON), or set of COMIDs. Mutli realizations are supported allowing you to query for flowlines, catchments, or outlets.
#' @param AOI an 'area of interest' object (sf POINT or POLYGON)
#' @param comid Search for NHD features by COMID
#' @param nwis Search for NHD features by colocated NWIS identifiers
#' @param streamorder filter returned NHD by streamorder. Returns are for given value and higher. Only usable with AOI, not with comid or nwis inputs
#' @param realization what realization to return. Default is flowline, options include: outlet, flowline, catchment, and all
#' @return a single, or list, or simple feature objects
#' @examples
#'  point <- sf::st_sfc(sf::st_point(c(-119.845, 34.4146)), crs = 4326)
#'  discover_nhd(point)
#'  discover_nhd(point, realization = "catchment")
#'  discover_nhd(point, realization = "all")

#'  discover_nhd(comid = 101)
#'  discover_nhd(nwis  = c(11120000, 11120500))

#'  area <- sf::st_as_sfc(sf::st_bbox(
#'    c(xmin = -119.8851, xmax =-119.8361, ymax = 34.42439, ymin = 34.40473), crs = 4326))

#'  discover_nhd(area)
#'  discover_nhd(area, realization = "flowline", streamorder = 3)
#' @importFrom methods is
#' @export

discover_nhd <- function(AOI = NULL, comid = NULL, nwis = NULL,
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
    geoms$catch   <- query_waterlabs(AOI = AOI, ids = comid, type = "catchments")
  }

  if(any(c("flowline", "outlet") %in% realization)){
    geoms$fl      <- query_waterlabs(AOI = AOI, ids = comid, type = 'nhd',
                               filter = streamorder_filter(streamorder))

    if("outlet" %in% realization){ geoms$outlet = find_outlets(geoms$fl) }
  }

  geoms = tc(geoms)
  if(length(geoms) == 1){ geoms = geoms[[1]]}

  return(geoms)

}

#' @title Find WBD HUC 12 unit subsets
#' @description Subsets the WBD level 08 features by location (POINT), area (POLYGON), or set of IDs.
#' @param AOI an 'area of interest' object (sf POINT or POLYGON)
#' @param id Search for WBD HUC08 features by ID
#' @return simple feature polygon object
#' @export

find_huc8 <-  function(AOI = NULL, id = NULL){
  query_waterlabs(AOI = AOI, ids = id, type = "huc08")
}

#' @title Find WBD HUC 12 unit subsets
#' @description Subsets the WBD level 12 features by location (POINT), area (POLYGON), or set of IDs.
#' @param AOI an 'area of interest' object (sf POINT or POLYGON)
#' @param id Search for WBD HUC12 features by ID
#' @return simple feature polygon object
#' @export

find_huc12 <- function(AOI = NULL, id = NULL){
  query_waterlabs(AOI = AOI, ids = id, type = "huc12")
}

#' @title Find WBD HUC08 unit Subsets
#' @description Subsets NHD waterbody features by location (POINT), area (POLYGON), or set of IDs.
#' @param AOI an 'area of interest' object (sf POINT or POLYGON)
#' @param id Search for WBD HUC08 features by ID
#' @return simple feature polygon object
#' @export

find_waterbodies <- function(AOI = NULL, id = NULL){
  query_waterlabs(AOI = AOI, ids = id, type = "waterbodies")
}

#' @title Discover USGS NWIS Stream Gages
#' @description Returns a POINT feature class of active, ST, NWIS gages for an Area of Interest.
#' If a POINT is given as the AOI, a 20km2 bounding box is searched and the nearest
#' feature (euclidian) is returned. Data is accessed through the NWIS web portal.
#' @param AOI an 'area of interest' object (sf POINT or POLYGON)
#' @return sf object
#' @importFrom xml2 xml_root xml_children xml_attr read_xml
#' @importFrom sf st_geometry_type  st_transform st_buffer st_as_sf st_bbox st_nearest_feature
#' @importFrom dplyr filter
#' @export

discover_nwis = function(AOI = NULL){

  AOI_type <- st_geometry_type(AOI)

  if(AOI_type == "POINT"){
    pt  <-  AOI
    AOI <-  sf::st_transform(AOI, 5070) %>%
      sf::st_buffer(20000)
  }

  bb <-  sf::st_transform(AOI, 4269)
  bb <-  round(sf::st_bbox(bb), 7)

  url <- paste0("https://waterservices.usgs.gov/nwis/site/?format=mapper&bBox=",
               bb$xmin, ",", bb$ymin, ",",
               bb$xmax, ",", bb$ymax,
               "&siteType=ST&siteStatus=active")

  resp <- tryCatch(
    {read_xml(url) },
    warning = function(w) { NULL },
    error = function(e)   { NULL })

  if(is.null(resp)){
    if(AOI_type == "POINT"){
      stop("No gages with 20km of this location", call. = FALSE)
    } else {}
      stop("No gages found in this AOI.", call. = FALSE)
  } else {
    doc        <- xml2::xml_root(resp)
    sc         <- xml2::xml_children(doc)
    sites      <- xml2::xml_children(sc)

    sites_sf <- data.frame(agency_cd  = xml2::xml_attr(sites, "agc"),
                           site_no    = xml2::xml_attr(sites, "sno"),
                           station_nm = xml2::xml_attr(sites, "sna"),
                           site_type  = xml2::xml_attr(sites, "cat"),
                           lat = as.numeric(xml2::xml_attr(sites, "lat")),
                           lon = as.numeric(xml2::xml_attr(sites, "lng"))) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4269)

    if(AOI_type == "POINT"){
      sites_sf <- suppressMessages(
        sites_sf[sf::st_nearest_feature(pt, sites_sf),]
      )
    }

    rturn(sites_sf)

  }
}




