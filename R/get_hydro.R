#' @title Find WBD HUC 08 unit subsets
#' @description Subsets the WBD level 08 features by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inherit query_usgs_geoserver details return
#' @inheritParams query_usgs_geoserver
#' @param id WBD HUC08 ID(s)
#' @export

get_huc8 <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "huc08",
                       t_srs = t_srs, buffer = buffer)
}

#' @title Find WBD HUC 12 unit subsets
#' @description Subsets the WBD level 12 features by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inherit query_usgs_geoserver details return
#' @inheritParams query_usgs_geoserver
#' @param id WBD HUC12 ID(s)
#' @export

get_huc12 <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "huc12",
                       t_srs = t_srs, buffer = buffer)
}

#' @title Find NHD Water Bodies
#' @description Subsets NHD waterbody features by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inherit query_usgs_geoserver details return
#' @inheritParams query_usgs_geoserver
#' @param id NHD Waterbody COMID(s)
#' @export

get_waterbodies <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5){
  query_usgs_geoserver(AOI = AOI, ids = id,
                       type = "waterbodies",
                       t_srs = t_srs,
                       buffer = buffer)
}

#' @title Find NHD Areas
#' @description Subsets NHD Area features by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inherit query_usgs_geoserver details return
#' @inheritParams query_usgs_geoserver
#' @param id NHD Area COMID(s)
#' @export

get_nhdarea <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "nhdarea",
                       t_srs = t_srs, buffer = buffer)
}


#' @title Find GAGESII Features
#' @description Subsets the gagesII dataset by location (POINT),
#' area (POLYGON), or set of IDs.
#' @inherit query_usgs_geoserver details return
#' @inheritParams query_usgs_geoserver
#' @param id character NWIS Gage ID(s)
#' @param basin logical should the gagesII basin also be returned? If True,
#' return value will be a list with "site" and "basin" elements.
#' @export

get_gagesII <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5,
                        basin = FALSE){

  out <- query_usgs_geoserver(AOI = AOI, ids = id, type = "gagesII",
                              t_srs = t_srs, buffer = buffer)

  if(basin) {
    return(list(site = out,
                basin = query_usgs_geoserver(
                  ids = out[["staid"]], type = "gagesII-basin",
                  t_srs = t_srs, buffer = buffer)))
  }

  out
}

#' @title Discover USGS NWIS Stream Gages
#' @description Returns a POINT feature class of active, stream network,
#' NWIS gages for an Area of Interest. If a POINT feature is used as an AOI,
#' then the returned sites within the requested buffer, are sorted by distance (in meters) from that POINT.
#' @inherit query_usgs_geoserver details return
#' @inheritParams query_usgs_geoserver
#' @param buffer numeric. The amount (in meters) to buffer a POINT AOI by
#' for an extended search. Default = 20,000. Returned results are arrange
#' by distance from POINT AOI
#' @importFrom xml2 xml_root xml_children xml_attr read_xml
#' @importFrom sf st_geometry_type st_transform st_buffer st_as_sf
#' st_bbox st_nearest_feature st_distance
#' @importFrom dplyr filter
#' @export

get_nwis <- function(AOI = NULL, t_srs = NULL, buffer = 20000){

  # If t_src is not provided set to AOI CRS
  if(is.null(t_srs)){ t_srs  <- sf::st_crs(AOI)}

  AOI_type = st_geometry_type(AOI)

  if(AOI_type == "POINT"){
    pt  <-  AOI
    AOI <-  sf::st_buffer(sf::st_transform(AOI, 5070), buffer) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc()
  }

  bb <-  sf::st_transform(AOI, 4326)
  bb <-  round(sf::st_bbox(bb), 7)

  dX = bb$xmax - bb$xmin
  dY = bb$ymax - bb$ymin

  if(dX > 4.599 | dY > 7.599){
    stop(paste0("Bounding Box too large [", round(dX,1),"x", round(dY,1), " degrees].
                Your requested width must be less than or equal to
                7.6 degrees at latitude 44.4
                with requested height of 4.6 degrees."))
  }

  u <- paste0("https://waterservices.usgs.gov/nwis/site/?format=mapper&bBox=",
                bb$xmin, ",", bb$ymin, ",",
                bb$xmax, ",", bb$ymax,
                "&siteType=ST&siteStatus=active")

  get_xml <- function(u) {
    u <- suppressWarnings(url(u, "rb"))
    out <- read_xml(u)
    close(u)
    out
  }

  resp <- tryCatch(get_xml(u), error = function(e) NULL)

  if(is.null(resp)){
    if(AOI_type == "POINT"){
      warning("No gages with defined buffer of this location")
      return(NULL)
    } else {
      warning("No gages found in this AOI.")
      return(NULL)
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
      st_as_sf(coords = c("lon", "lat"), crs = 4326)

    if(AOI_type == "POINT"){
      sites_sf <- sites_sf %>%
        mutate(distance_m = st_distance(st_transform(., 5070),
                                        st_transform(pt, 5070))) %>%
        arrange(.data$distance_m)
    }

    return(st_transform(sites_sf, t_srs))
  }
}
