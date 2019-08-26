#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#' @param point An sf POINT including crs as created by:
#' sf::st_sfc(sf::st_point(..,..), crs)
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' discover_nldi_sources() and the `featureSource` is a known identifier
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
discover_nhdplus_id <- function(point = NULL, nldi_feature = NULL) {

  if (!is.null(point)) {

    url_base <- paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows",
                       "?service=WFS",
                       "&version=1.0.0",
                       "&request=GetFeature",
                       "&typeName=nhdplus:catchmentsp",
                       "&outputFormat=application%2Fjson",
                       "&srsName=EPSG:4269")
    # "&bbox=40,-90.001,40.001,-90,urn:ogc:def:crs:EPSG:4269",

    p_crd <- sf::st_coordinates(sf::st_transform(point, 4269))

    url <- paste0(url_base, "&bbox=",
                  paste(p_crd[2], p_crd[1],
                        p_crd[2] + 0.00001, p_crd[1] + 0.00001,
                        "urn:ogc:def:crs:EPSG:4269", sep = ","))

    req_data <- httr::RETRY("GET", url, times = 10, pause_cap = 240)

    catchment <- make_web_sf(req_data)

    if (nrow(catchment) > 1) {
      warning("point too close to edge of catchment found multiple.")
    }

    return(as.integer(catchment$featureid))

  } else if (!is.null(nldi_feature)) {

    check_nldi_feature(nldi_feature)

    if (is.null(nldi_feature[["tier"]])) nldi_feature[["tier"]] <- "prod"

    nldi <- get_nldi_feature(nldi_feature[["featureSource"]],
                             nldi_feature[["featureID"]],
                             nldi_feature[["tier"]])

    return(as.integer(nldi$features$properties$comid))

  } else {

    stop("Must provide point or nldi_feature input.")

  }
}

#' @noRd
get_nhdplus_byid <- function(comids, layer) {

  id_name <- list(catchmentsp = "featureid", nhdflowline_network = "comid")

  if (!any(names(id_name) %in% layer)) {
    stop(paste("Layer must be one of",
               paste(names(id_name),
                     collapse = ", ")))
  }

  post_url <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"

  # nolint start

  filter_1 <- paste0('<?xml version="1.0"?>',
                     '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="application/json" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                     '<wfs:Query xmlns:feature="http://gov.usgs.cida/nhdplus" typeName="feature:',
                     layer, '" srsName="EPSG:4326">',
                     '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                     '<ogc:Or>',
                     '<ogc:PropertyIsEqualTo>',
                     '<ogc:PropertyName>',
                     id_name[[layer]],
                     '</ogc:PropertyName>',
                     '<ogc:Literal>')

  filter_2 <- paste0('</ogc:Literal>',
                     '</ogc:PropertyIsEqualTo>',
                     '<ogc:PropertyIsEqualTo>',
                     '<ogc:PropertyName>',
                     id_name[[layer]],
                     '</ogc:PropertyName>',
                     '<ogc:Literal>')

  filter_3 <- paste0('</ogc:Literal>',
                     '</ogc:PropertyIsEqualTo>',
                     '</ogc:Or>',
                     '</ogc:Filter>',
                     '</wfs:Query>',
                     '</wfs:GetFeature>')

  filter_xml <- paste0(filter_1, paste0(comids, collapse = filter_2), filter_3)

  # nolint end

  req_data <- httr::RETRY("POST", post_url, body = filter_xml, times = 10, pause_cap = 240)

  return(make_web_sf(req_data))
}

#' @noRd
get_nhdplus_bybox <- function(box, layer) {

  valid_layers <- c("nhdarea", "nhdwaterbody")

  if (!layer %in% valid_layers) {
    stop(paste("Layer must be one of",
               paste(valid_layers,
                     collapse = ", ")))
  }

  bbox <- sf::st_bbox(sf::st_transform(box, 4326))

  post_url <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"

  # nolint start

  filter_xml <- paste0('<?xml version="1.0"?>',
                       '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="application/json" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                       '<wfs:Query xmlns:feature="http://gov.usgs.cida/nhdplus" typeName="feature:',
                       layer, '" srsName="EPSG:4326">',
                       '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                       '<ogc:BBOX>',
                       '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                       '<gml:Envelope>',
                       '<gml:lowerCorner>',bbox[2]," ",bbox[1],'</gml:lowerCorner>',
                       '<gml:upperCorner>',bbox[4]," ",bbox[3],'</gml:upperCorner>',
                       '</gml:Envelope>',
                       '</ogc:BBOX>',
                       '</ogc:Filter>',
                       '</wfs:Query>',
                       '</wfs:GetFeature>')

  # nolint end

  req_data <- httr::RETRY("POST", post_url, body = filter_xml, times = 10, pause_cap = 240)

  return(make_web_sf(req_data))

}

#' Download NHDPlus HiRes
#' @param nhd_dir character directory to save output into
#' @param hu_list character vector of hydrologic region(s) to download
#' @param download_files boolean if FALSE, only URLs to files will be returned
#' can be hu02s and/or hu04s
#'
#' @return Paths to geodatabases created.
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_text
#' @importFrom utils download.file unzip
#' @export
#' @examples
#' \donttest{
#' download_nhdplushr(tempdir(), c("01", "0203"), download_files = FALSE)
#' }
download_nhdplushr <- function(nhd_dir, hu_list, download_files = TRUE) {

  nhdhr_bucket <- get("nhdhr_bucket", envir = nhdplusTools_env)
  nhdhr_file_list <- get("nhdhr_file_list", envir = nhdplusTools_env)

  hu02_list <- unique(substr(hu_list, 1, 2))
  hu04_list <- hu_list[which(nchar(hu_list) == 4)]
  subset_hu02 <- sapply(hu02_list, function(x)
    sapply(x, function(y) any(grepl(y, hu04_list))))

  out <- c()

  for(h in 1:length(hu02_list)) {
    hu02 <- hu02_list[h]

    if(download_files) {
      out <- c(out, file.path(nhd_dir, hu02))
    }

    if(download_files) {
      dir.create(out[length(out)], recursive = TRUE, showWarnings = FALSE)
    }

    file_list <- read_xml(paste0(nhdhr_bucket, nhdhr_file_list,
                                 "NHDPLUS_H_", hu02)) %>%
      xml_ns_strip() %>%
      xml_find_all(xpath = "//Key") %>%
      xml_text()

    file_list <- file_list[grepl("_GDB.zip", file_list)]

    if(subset_hu02[h]) {
      file_list <- file_list[sapply(file_list, function(f)
        any(sapply(hu04_list, grepl, x = f)))]
    }

    for(key in file_list) {
      out_file <- paste0(out[length(out)], "/", tail(strsplit(key, "/")[[1]], 1))
      url <- paste0(nhdhr_bucket, key)

      if(download_files & !dir.exists(gsub(".zip", ".gdb", out_file))) {
        download.file(url, out_file)
        unzip(out_file, exdir = out[length(out)])
        unlink(out_file)
      } else if(!download_files) {
        out <- c(out, url)
      }
    }
  }
  return(out)
}

#' Get NHDPlus HiRes as single geopackage
#' @param hr_dir character directory with geodatabases (gdb search is recursive)
#' @param out_gpkg character path to write output geopackage
#' @param layers character vector with desired layers to return.
#' c("NHDFlowline", "NHDPlusCatchment") is default.
#' Choose from:
#' c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody", "NHDArea", "NHDLine",
#' "NHDPlusSink", "NHDPlusWall", "NHDPoint", "NHDPlusBurnWaterbody",
#' "NHDPlusBurnLineEvent", "HYDRO_NET_Junctions",
#' "WBDHU2", "WBDHU4","WBDHU6", "WBDHU8" "WBDHU10", "WBDHU12", "WBDLine")
#' @param pattern character optional regex to select certain files in hr_dir
#'
#' @details
#' NHDFlowline is joined to value added attributes prior to being
#' returned.
#' Names are not modified from the NHDPlusHR geodatabase.
#' Set layers to "NULL" to get all layers.
#'
#' @importFrom sf st_layers read_sf st_sf write_sf
#' @export
#' @examples
#' \donttest{
#' # Note this will download a lot of data to a temp directory.
#' # Change 'tempdir()' to your directory of choice.
#' download_dir <- download_nhdplushr(tempdir(), c("0302", "0303"))
#' get_nhdplushr(download_dir, file.path(download_dir, "nhdplus_0302-03.gpkg"))
#' }
get_nhdplushr <- function(hr_dir, out_gpkg = NULL,
                          layers = c("NHDFlowline", "NHDPlusCatchment"),
                          pattern = ".*GDB.gdb$") {
  gdb_files <- list.files(hr_dir, pattern = pattern,
                          full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

  if(length(gdb_files) == 0) {
    # For testing.
    gdb_files <- list.files(hr_dir, pattern = "sub.gpkg", full.names = TRUE)
  }
  if(is.null(layers)) {
    layers <- st_layers(gdb_files[1])

    layers <- layers$name[!is.na(layers$geomtype) & layers$features > 0]
  }

  out_list <- list()

  for(layer in layers) {
    layer_set <- lapply(gdb_files, get_hr_data, layer = layer)

    out <- do.call(rbind, layer_set)

    try(out <- st_sf(out))

    if(!is.null(out_gpkg)) {
      write_sf(out, layer = layer, dsn = out_gpkg)
    } else {
      out_list[layer] <- list(out)
    }
  }
  if(!is.null(out_gpkg)) {
    return(out_gpkg)
  } else {
    return(out_list)
  }
}

get_hr_data <- function(gdb, layer = NULL) {
  if(layer == "NHDFlowline") {
    vaa <- suppressWarnings(read_sf(gdb, "NHDPlusFlowlineVAA"))
    vaa <- select(vaa, -ReachCode, -VPUID)
    left_join( read_sf(gdb, layer), vaa, by = "NHDPlusID")
  } else {
    read_sf(gdb, layer)
  }
}

make_web_sf <- function(content) {
  if(content$status_code == 200) {
    tryCatch(sf::read_sf(rawToChar(content$content)),
             error = function(e) {
               message(paste("Something went wrong with a web request.\n", e))
             }, warning = function(w) {
               message(paste("Something went wrong with a web request.\n", w))
             })
  } else {
    message(paste("Something went wrong with a web request.\n", content$url,
                  "\n", "returned",
                  content$status_code))
    data.frame()
  }
}
