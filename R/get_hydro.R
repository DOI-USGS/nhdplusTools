#' @title Find WBD HUC unit subsets
#' @description Subsets WBD features by location (POINT),
#' area (POLYGON), or set of HUC IDs.
#'
#' @inherit query_usgs_geoserver details return params
#' @param id WBD HUC ID(s)
#' @param type character. Type of feature to return
#' ('huc02', 'huc04', 'huc06', 'huc08', 'huc10', 'huc12', 'huc12_nhdplusv2').
#'
#' Pulls `huc02`-`huc12` from a web service that hosts a snapshot of the
#' Watershed Boundary Dataset from October, 2020.
#'
#' See <doi:10.5066/P92U7ZUT> for full source data.
#'
#' See https://labs.waterdata.usgs.gov/geoserver/web/ for the web service.
#'
#' `huc12_nhdplusv2` derives from a snapshot of the WBD available from the nhdplusv2.
#' See \link{download_nhdplusv2} for source data documentation.
#'
#'
#'
#' @export
#'
get_huc <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5, type = "huc12") {

  allow_types <- c('huc02', 'huc04', 'huc06', 'huc08', 'huc10', 'huc12',
                   'huc12_nhdplusv2')

  if(!type %in% allow_types) {
    stop("type must be one of ", paste(allow_types, collapse = " "))
  }

  query_usgs_geoserver(AOI = AOI, ids = id, type = type,
                       t_srs = t_srs, buffer = buffer)

}

#' @title Find NHDPlusV2 Water Bodies
#' @description Subsets NHDPlusV2 waterbody features by location (POINT),
#' area (POLYGON), or set of IDs. See \link{download_nhdplusv2} for source data documentation.
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

#' @title Find NHDPlusV2 Areas
#' @description Subsets NHDPlusV2 Area features by location (POINT),
#' area (POLYGON), or set of IDs. See \link{download_nhdplusv2} for source data documentation.
#' @inherit query_usgs_geoserver details return
#' @inheritParams query_usgs_geoserver
#' @param id NHD Area COMID(s)
#' @export

get_nhdarea <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5){
  query_usgs_geoserver(AOI = AOI, ids = id, type = "nhdarea",
                       t_srs = t_srs, buffer = buffer)
}


#' @title Find gagesII Features
#' @description Subsets the gagesII dataset by location (POINT),
#' area (POLYGON), or set of IDs. See <doi:10.5066/P96CPHOT> for documentation of source data.
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
#' @importFrom dplyr filter mutate
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

#' Get 3DHP Data
#' @description
#' Calls the 3DHP_all web service and returns sf data.frames for the selected
#' layers. See https://hydro.nationalmap.gov/arcgis/rest/services/3DHP_all/MapServer
#' for source data documentation.
#'
#' @inherit query_usgs_arcrest details return params
#' @param ids character vector of id3dhp ids or mainstem uris
#' @param universalreferenceid character vector of hydrolocation universal
#' reference ids such as reachcodes
#' @export
#' @examples
#' \donttest{
#' AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
#'                                    xmax = -89.24681, ymax = 43.17192),
#'                                  crs = "+proj=longlat +datum=WGS84 +no_defs"))
#'
#' # get flowlines and hydrolocations
#' flowlines <- get_3dhp(AOI = AOI, type = "flowline")
#' hydrolocation <- get_3dhp(AOI = AOI, type = "hydrolocation")
#' waterbody <- get_3dhp(AOI = AOI, type = "waterbody")
#'
#' if(!is.null(waterbody) & !is.null(flowlines) & !is.null(hydrolocation)) {
#' plot(sf::st_geometry(waterbody), col = "lightblue", border = "lightgrey")
#' plot(sf::st_geometry(flowlines), col = "blue", add = TRUE)
#' plot(sf::st_geometry(hydrolocation), col = "grey", pch = "+", add = TRUE) }
#'
#' # given mainstem ids from any source, can query for them in ids.
#'
#' CO <- get_3dhp(ids = "https://geoconnex.us/ref/mainstems/29559",
#'                type = "flowline")
#'
#' if(!is.null(CO))
#'   plot(sf::st_geometry(CO), col = "blue")
#'
#' # get all the waterbodies along the CO river
#' CO_wb <- get_3dhp(ids = unique(CO$waterbodyid3dhp), type = "waterbody")
#'
#' if(!is.null(CO_wb)) {
#' plot(sf::st_geometry(CO_wb[grepl("Powell", CO_wb$gnisidlabel),]),
#'      col = "blue", border = "NA") }
#'
#' # given universalreferenceid (reachcodes), can query for them but only
#' # for hydrolocations. This is useful for looking up mainstem ids.
#'
#' get_3dhp(universalreferenceid = unique(hydrolocation$universalreferenceid),
#'          type = "hydrolocation")
#'}
get_3dhp <- function(AOI = NULL, ids = NULL, type = NULL,
                     universalreferenceid = NULL,
                     t_srs = NULL, buffer = 0.5) {

  if(!is.null(universalreferenceid) & !grepl("outlet|reach|hydrolocation", type)) {
    stop("universalereferenceid can only be specified for hydrolocation features")
  }

  where <- NULL
  if(!is.null(universalreferenceid)) {
    where <- paste(paste0("universalreferenceid IN ('",
                          paste(universalreferenceid, collapse = "', '"), "')"))
    if(!is.null(ids)) stop("can not specify both universalreferenceid and other ids")
  }

  if(!is.null(ids) && grepl("^https://", ids[1])) {
    where <- paste(paste0("mainstemid IN ('",
                          paste(ids, collapse = "', '"), "')"))
    ids <- NULL
  }

  query_usgs_arcrest(AOI, ids, type, where, t_srs, buffer)

}

