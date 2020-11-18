#' @title Find WBD HUC 08 unit subsets
#' @description Subsets the WBD level 08 features by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inheritParams discover_nhd
#' @param id WBD HUC08 ID(s)
#' @return simple feature polygon object
#' @export

discover_huc8 <- function(AOI = NULL, id = NULL){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "huc08")
}

#' @title Find WBD HUC 12 unit subsets
#' @description Subsets the WBD level 12 features by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inheritParams discover_nhd
#' @param id WBD HUC12 ID(s)
#' @return simple feature polygon object
#' @export

discover_huc12 <- function(AOI = NULL, id = NULL){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "huc12")
}

#' @title Find NHD Water Bodies
#' @description Subsets NHD waterbody features by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inheritParams discover_nhd
#' @param id Search for NHD Water Bodies by COMID
#' @return simple feature polygon object
#' @export

discover_waterbodies <- function(AOI = NULL, id = NULL){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "waterbodies")
}

#' @title Find NHD Areas
#' @description Subsets NHD Area features by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inheritParams discover_nhd
#' @param id Search for NHD Areas Bodies by COMID
#' @return simple feature polygon object
#' @export

discover_nhdarea <- function(AOI = NULL, id = NULL){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "nhdarea")
}


#' @title Find GAGESII Features
#' @description Subsets the gagesII dataset by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inheritParams discover_nhd
#' @param id Search for gagesII locations by NWIS siteID
#' @return simple feature polygon object
#' @export

discover_gagesII <- function(AOI = NULL, id = NULL){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "gagesII")
}

#' @title Discover USGS NWIS Stream Gages
#' @description Returns a POINT feature class of active, stream network,
#' NWIS gages for an Area of Interest. If a POINT is given as the AOI,
#' a 20km2 bounding box is searched and the nearest feature (euclidian)
#' is returned. Data is accessed through the NWIS web portal.
#' @inheritParams discover_nhd
#' @return sf object
#' @importFrom xml2 xml_root xml_children xml_attr read_xml
#' @importFrom sf st_geometry_type  st_transform st_buffer st_as_sf
#' st_bbox st_nearest_feature
#' @importFrom dplyr filter
#' @export

discover_nwis = function(AOI = NULL){

  AOI_type <- st_geometry_type(AOI)

  if(AOI_type == "POINT"){
    pt  <-  AOI
    AOI <-  sf::st_transform(AOI, 5070) %>%
      sf::st_buffer(20000) # meters due to 5070
  }

  bb <-  sf::st_transform(AOI, 4269)
  bb <-  round(sf::st_bbox(bb), 7)

  url <- paste0("https://waterservices.usgs.gov/nwis/site/?format=mapper&bBox=",
                bb$xmin, ",", bb$ymin, ",",
                bb$xmax, ",", bb$ymax,
                "&siteType=ST&siteStatus=active")

  resp <- tryCatch({read_xml(url) },
    warning = function(w) { NULL },
    error = function(e)   { NULL }
  )

  if(is.null(resp)){
    if(AOI_type == "POINT"){
      stop("No gages with 20km of this location", call. = FALSE)
    } else {
      stop("No gages found in this AOI.", call. = FALSE)
    }
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

    return(sites_sf)
  }
}
