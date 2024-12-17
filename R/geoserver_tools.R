#' @title Query USGS Geoserver
#' @description Query the USGS  geoserver for spatial data by location,
#' area, or ID.
#' @details The returned object(s) will have the same
#' Spatial Reference System (SRS) as the input AOI. If a individual or set of
#' IDs are used to query, then the default geoserver CRS of EPSG:4326 is
#' preserved. In all cases, a user-defined SRS can be passed to \code{t_srs}
#' which will override all previous SRS (either input or default).
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
#' @importFrom sf sf_use_s2
#' @importFrom httr POST RETRY
#' @importFrom dplyr filter
#' @importFrom methods as
#' @importFrom xml2 read_xml

query_usgs_geoserver <- function(AOI = NULL,  ids = NULL,
                                 type = NULL, filter = NULL,
                                 t_srs = NULL,
                                 buffer = 0.5) {

  source <- data.frame(server = 'wmadata',
                       user_call  = c('huc02', 'huc04', 'huc06',
                                      'huc08', 'huc10', 'huc12',
                                      'huc08_legacy', "huc12_nhdplusv2",
                                      'nhd','catchment', 'nhdarea',
                                      'nonnetwork',
                                      'waterbodies',
                                      'gagesII', "gagesII-basin"),
                       geoserver  = c("wbd02_20201006", "wbd04_20201006",
                                      "wbd06_20201006", "wbd08_20201006",
                                      "wbd10_20201006", "wbd12_20201006",
                                      "huc08", "huc12",
                                      "nhdflowline_network", "catchmentsp", 'nhdarea',
                                      "nhdflowline_nonnetwork",
                                      "nhdwaterbody",
                                      "gagesii", "gagesii_basins"),
                       geom_name = c("SHAPE", "SHAPE", "SHAPE",
                                     "SHAPE","SHAPE","SHAPE",
                                     "the_geom", "the_geom",
                                     "the_geom", "the_geom", "the_geom",
                                     "the_geom",
                                     "the_geom",
                                     "the_geom", "the_geom"),
                       ids        = c("huc2", "huc4", "huc6",
                                      "huc8", "huc10", "huc12",
                                      "huc8", "huc12",
                                      "comid", "featureid", "comid",
                                      "comid",
                                      "comid",
                                      "staid", "gage_id"),
                       page       = c(FALSE, FALSE,
                                      FALSE, FALSE,
                                      FALSE, FALSE,
                                      FALSE, FALSE,
                                      TRUE, TRUE, TRUE,
                                      TRUE,
                                      TRUE,
                                      FALSE, TRUE))

  if(is.null(type)){ return(source) }

  AOI <- check_query_params(AOI, ids, type, NULL, source, t_srs, buffer)
  t_srs <- AOI$t_srs
  AOI <- AOI$AOI

  here <- filter(source, .data$user_call == !!type)

  URL <- paste0(get("geoserver_root", envir = nhdplusTools_env),
                here$server,
                "/ows")

  use_s2 <- sf_use_s2()
  sf_use_s2(FALSE)

  on.exit(sf_use_s2(use_s2), add = TRUE)

  out <- spatial_filter(AOI, tile = here$page, geom_name = here$geom_name)

  for(i in 1:length(out)) {

    if(i > 1) {
      message("Getting tile ", i, " of ", length(out))
    }

    startXML <- paste0('<?xml version="1.0"?>',
                       '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs"
                       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                       ' xmlns:gml="http://www.opengis.net/gml" service="WFS"
                        version="1.1.0" outputFormat="application/json"',
                       ' xsi:schemaLocation="http://www.opengis.net/wfs
                        http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                       '<wfs:Query xmlns:feature="https://api.water.usgs.gov/',
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
                               out[[i]],
                               '</ogc:And>',
                               '</ogc:Filter>',
                               '</wfs:Query>',
                               '</wfs:GetFeature>')
    )

    tryCatch({
      if(nhdplus_debug()) {
        message(paste(URL, "\n"))
        message(as.character(read_xml(filterXML)))
      }

      out[[i]] <- rawToChar(RETRY("POST",
                                  URL,
                                  body = filterXML)$content)
    }, error = function(e) {
      warning("Something went wrong trying to access a service.")
      return(NULL)
    })

    out[[i]] <- tryCatch({
      st_zm(read_sf(out[[i]]))},
      error = function(e) return(NULL))
  }

  if(inherits(out[[1]], "data.frame")) {
    out <- bind_rows(unify_types(out))

    out <- check_valid(out[!duplicated(out[[here$ids]]), ],
                       out_prj = t_srs)

  } else {
    out <- NULL
  }

  if(any(is.null(out), nrow(out) == 0)) {

    out = NULL

  } else if(!is.null(AOI)){

    out = st_filter(st_transform(out, t_srs),
                    st_transform(AOI, t_srs))
    if(nrow(out) == 0){
      out = NULL
    }
  }

  if(!is.null(out)) {
    return(select(out, -any_of("id")))
  } else {
    warning(paste("No", here$user_call, "features found"), call. = FALSE)
    NULL
  }

}

unify_types <- function(out) {
  all_class <- bind_rows(lapply(out, function(x) {
    vapply(x, function(x) class(x)[1], character(1))
  }))

  set_type <- function(out, n, type) {
    lapply(out, function(x) {
      x[[n]] <- as(x[[n]], type)
      x
    })
  }

  for(n in names(all_class)) {
    if(length(unique(all_class[[n]])) > 1) {
      if("numeric" %in% all_class[[n]]) { # prefer numeric
        out <- set_type(out, n, "numeric")
      } else if("integer" %in% all_class[[n]]) { # then integer
        out <- set_type(out, n, "integer")
      } else if("cheracter" %in% all_class[[n]]) {
        out <- set_type(out, n, "character")
      }
    }
  }

  rows <- sapply(out, nrow)

  if(any(rows > 0))
    out <- out[rows > 0]

  out
}

assign("bb_break_size", value = 2, nhdplusTools_env)

#' @title Construct a BBOX spatial filter for geoservers
#' @description From an 'area of intferest' object (sf POINT or POLYGON),
#' generate a WMS BBOX filter to pass to a geoserver.
#' @inheritParams get_nhdplus
#' @param type needed if we want to use CQL, not for BBOX. Left for posterity
#' @param break_size desired size of bbox tiles
#' @param tile should the response be a tiled list or not?
#' @return a character string XML filter
#' @keywords internal
#' @noRd
#' @importFrom sf st_geometry_type st_buffer st_transform st_bbox

spatial_filter  <- function(AOI,
                            break_size = get("bb_break_size", nhdplusTools_env),
                            tile = TRUE,
                            geom_name = "the_geom", format = 'ogc') {

  if(is.null(AOI)) return(list(list()))


  bb <- st_transform(AOI, 4326) %>%
    st_bbox()

  if(tile) {
    x_breaks <- seq(bb$xmin, bb$xmax, length.out = (ceiling((bb$xmax - bb$xmin) / break_size) + 1))
    y_breaks <- seq(bb$ymin, bb$ymax, length.out = (ceiling((bb$ymax - bb$ymin) / break_size) + 1))

    bb_list <- unlist(lapply(seq_len(length(x_breaks) - 1), function(x) {
      lapply(seq_len(length(y_breaks) - 1), function(y) {
        list(xmin = x_breaks[x], ymin = y_breaks[y], xmax = x_breaks[x + 1], ymax = y_breaks[y + 1])
      })
    }), recursive = FALSE)
  } else {
    bb_list <- list(bb)
  }

  if(format == "ogc") {
    lapply(bb_list, function(bb) {
      paste0('<ogc:BBOX>',
             '<ogc:PropertyName>', geom_name, '</ogc:PropertyName>',
             '<gml:Envelope srsName="urn:x-ogc:def:crs:EPSG:4326">',
             '<gml:lowerCorner>', bb$ymin, " ", bb$xmin, '</gml:lowerCorner>',
             '<gml:upperCorner>', bb$ymax, " ", bb$xmax, '</gml:upperCorner>',
             '</gml:Envelope>',
             '</ogc:BBOX>')
    })
  } else if(format == "esri") {

    # {
    #   "xmin": -109.55,
    #   "ymin": 25.76,
    #   "xmax": -86.39,
    #   "ymax": 49.94,
    #   "spatialReference": {
    #     "wkid": 4326
    #   }
    # }

    lapply(bb_list, function(bb) {
      jsonlite::toJSON(
        c(bb, list(spatialReference = list(wkid = 4326))),
        auto_unbox = TRUE)
    })
  } else {
    stop("format must be ogc or esri")
  }
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

extact_comid_nwis <- memoise::memoise(function(nwis){
  # We could export this from dataRetrieval dataRetrieval:::pkg.env$nldi_base
  #but currently its not...
  baseURL  <- paste0(get_nldi_url(), "/linked-data/")
  url      <-  paste0(baseURL, "nwissite/USGS-", nwis)
  c        <-  rawToChar(httr::RETRY("GET", url)$content)
  f.comid  <-  jsonlite::fromJSON(c, simplifyVector = TRUE)
  f.comid$features$properties$comid
})

#' @importFrom sf st_make_valid st_as_sfc st_bbox st_buffer st_transform
check_query_params <- function(AOI, ids, type, where, source, t_srs, buffer) {
  # If t_src is not provided set to AOI CRS
  if(is.null(t_srs)){ t_srs  <- st_crs(AOI) }
  # If AOI CRS is NA (e.g st_crs(NULL)) then set to 4326
  if(is.na(t_srs))  { t_srs  <- 4326 }

  if(!is.null(AOI) & !is.null(ids)) {
    # Check if AOI and IDs are both given
    stop("Either IDs or a spatial AOI can be passed.", .call = FALSE)
  } else if(is.null(AOI) & is.null(ids) & !(!is.null(where) && grepl("IN", where))) {
    # Check if AOI and IDs are both NULL
    stop("IDs or a spatial AOI must be passed.", .call = FALSE)
  } else if(!(type %in% source$user_call)) {
    # Check that "type" is valid
    stop(paste("Type not available must be one of:",
               paste(source$user_call, collapse = ", ")),
         call. = FALSE)
  }

  if(!is.null(AOI)){

    if(length(st_geometry(AOI)) > 1) {
      stop("AOI must be one an only one feature.")
    }

    if(st_geometry_type(AOI) == "POINT"){
      # If input is a POINT, buffer by 1/2 meter (in equal area projection)
      AOI = st_buffer(st_transform(AOI, 5070), buffer) %>%
        st_bbox() %>% st_as_sfc() %>% st_make_valid()
    }
  }

  return(list(AOI = AOI, t_srs = t_srs))
}
