#' @title Get National Hydrography Dataset V2 Subsets (Multirealization)
#' @description Subsets NHDPlusV2 features by location (POINT), area (POLYGON),
#' or set of COMIDs. Multi realizations are supported allowing you to query
#' for flowlines, catchments, or outlets.
#' @inherit query_usgs_geoserver details
#' @inheritParams query_usgs_geoserver
#' @param comid numeric or character. Search for NHD features by COMID(s)
#' @param nwis  numeric or character. Search for NHD features by
#' collocated NWIS identifiers
#' @param streamorder numeric or character. Only return NHD flowlines with a
#' streamorder greater then or equal to this value
#' for input value and higher.
#' Only usable with AOI and flowline realizations.
#' @param realization character. What realization to return.
#' Default is flowline and options include: outlet, flowline, catchment,
#' and all
#' @return sfc a single, or list, of simple feature objects
#' @examples
#' \donttest{
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
#'  }
#' @importFrom methods is
#' @importFrom sf st_filter st_crs st_transform
#' @export

get_nhdplus <- function(AOI = NULL,
                        comid = NULL, nwis = NULL,
                        realization = "flowline",
                        streamorder = NULL,
                        t_srs = NULL){


  if(!is.null(AOI)){

    if(all(!methods::is(AOI,"sf"), !methods::is(AOI,"sfc"))){
      stop("AOI must be of class sf.", .call = FALSE)
    }

    if(st_geometry_type(AOI) == "POINT"){
      # This is here is here so that if a POINT location is requested,
      # a COMID is used to extract the NHD data/realization
      # This overrides the default behavior of query_usgs_geoserver
      # which buffers a POINT by 1/2 meter
      comid  <- discover_nhdplus_id(AOI)
      AOI    <- NULL
    }
  }

  if(!is.null(AOI) & !is.null(c(nwis, comid))){
    stop("Either IDs (comid, nwis) or a spatial AOI can be passed.",.call = FALSE)
  } else if(is.null(AOI) & is.null(c(nwis, comid))){
    stop("IDs (comid, nwis) or a spatial AOI must be passed.",.call = FALSE)
  }

  hy_realizations = c("flowline", "catchment", 'outlet')

  if("all" %in% realization){ realization = hy_realizations}

  if(any(!realization %in% hy_realizations)){
    stop(paste(realization, "not valid.\n Select from", paste(hy_realizations, collapse = ", ")))
  }

  geoms = list()

  if(!is.null(nwis)){
    comid = c(unlist(lapply(nwis, extact_comid_nwis)), comid)
  }

  if("catchment" %in% realization){
    geoms$catchment   <- query_usgs_geoserver(AOI = AOI, ids = comid,
                                              type = "catchment",
                                              t_srs = t_srs)
  }

  if(any(c("flowline", "outlet") %in% realization)){
    geoms$flowline    <- query_usgs_geoserver(AOI = AOI, ids = comid, type = 'nhd',
                                                filter = streamorder_filter(streamorder),
                                                t_srs = t_srs)

    if("outlet" %in% realization){
      geoms$outlet          <- geoms$flowline
      geoms$outlet$geometry <- st_geometry(
        get_node(geoms$outlet,
                 position = "end")
      )
    }
  }

  geoms = tc(geoms)

  geoms = geoms[names(geoms) %in% realization]

  if(length(geoms) == 1){ geoms = geoms[[1]]}

  return(geoms)
}

