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

  c <- httr::RETRY("POST", post_url, body = filter_xml, times = 10, pause_cap = 240)

  return(sf::read_sf(rawToChar(c$content)))
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

  c <- httr::RETRY("POST", post_url, body = filter_xml, times = 10, pause_cap = 240)

  return(sf::read_sf(rawToChar(c$content)))

}

#' download NHDPlus HiRes
#' @param nhd_dir character directory to save output into
#' @param hu_list character vector of hydrologic region(s) to download
#' can be hu02s and/or hu04s
#'
#' @return Paths to geodatabases created.
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_text
#' @export
#' @examples
#' download_nhdhr("", c("01", "0203"), download_files = FALSE)
download_nhdhr <- function(nhd_dir, hu_list, download_files = TRUE) {

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

    if(!download_files || !dir.exists(out[length(out)])) {

      if(download_files) {
        dir.create(out, recursive = TRUE, showWarnings = FALSE)
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
        out_file <- paste0(out, "/", tail(strsplit(key, "/")[[1]], 1))
        url <- paste0(nhdhr_bucket, key)

        if(download_files) {
          download.file(url, out_file)
          unzip(out_file, exdir = out)
          unlink(out_file)
        } else {
          out <- c(out, url)
        }
      }
    }
  }
  return(out)
}

#' Get NHDPlus HiRes as single geopackage
#' @param hr_dir character directory with geodatabases
#' @param out_gpkg character path to write output geopackage
#' @importFrom sf st_layers read_sf st_sf
get_nhdplushr <- function(hr_dir, out_gpkg) {
  gdb_files <- list.files(hr_dir, pattern = "GDB.gdb", full.names = TRUE)

  layer_names <- st_layers(gdb_files[1])

  layer_names <- layer_names$name[!is.na(layer_names$geomtype) & layer_names$features > 0]

  for(layer in layer_names) {
    layer_set <- lapply(gdb_files, read_sf, layer = layer)

    out <- do.call(rbind, layer_set)

    out <- st_sf(out)

    write_sf(out, layer = layer, dsn = out_gpkg)

  }

  return(out_gpkg)
}

get_hr_fline <- function(gdb) {
  fline <- "NHDFlowline"
  vaa <- "NHDPlusFlowlineVAA"
  left_join(
    read_sf(gdb, fline),
    suppressWarnings(read_sf(gdb, vaa)),
    by = "NHDPlusID")
}
