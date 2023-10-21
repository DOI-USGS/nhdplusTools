# this function is intended to be a very general internal utility. It's initial
# implementation is quite specific but this should become more general over time.
#' @title Query USGS Hydro ESRI Rest Server
#' @description Query the USGS Hydro ESRI Rest Server for spatial data by location,
#' area, or ID.
#' @details The returned object(s) will have the same
#' Spatial Reference System (SRS) as the input AOI. If a individual or set of
#' IDs are used to query, then the default CRS of EPSG:4269 is
#' preserved. In all cases, a user-defined SRS can be passed to \code{t_srs}
#' which will override all previous SRS (either input or default).
#' All buffer and distance operations are handled internally using in
#' EPSG:5070 Albers Equal Area projection
#' @param AOI sf (MULTI)POINT or (MULTI)POLYGON. An 'area of interest' can
#' be provided as either a location (sf POINT) or area (sf POLYGON)
#' in any Spatial Reference System.
#' @param ids character. A set of identifier(s) from the data
#' type requested, for 3dhp, this is id3dhp.
#' @param type character. Type of feature to return
#' ("hydrolocation", "flowline", "waterbody", "drainage area", "catchment").
#' If NULL (default) a data.frame of available resources is returned
#' @param where character. An where clause to pass to the server.
#' @param t_srs  character (PROJ string or EPSG code) or numeric (EPSG code).
#' A user specified - target -Spatial Reference System (SRS/CRS) for returned objects.
#' Will default to the CRS of the input AOI if provided, and to 4326 for ID requests.
#' @param buffer numeric. The amount (in meters) to buffer a POINT AOI by for an
#' extended search. Default = 0.5
#' @return a simple features (sf) object or valid types if no type supplied
#' @keywords internal
#' @importFrom sf st_crs st_geometry_type st_buffer st_transform st_zm read_sf st_bbox st_as_sfc
#' @importFrom httr RETRY content
#' @importFrom dplyr filter
#' @importFrom methods as
query_usgs_arcrest <- function(AOI = NULL,  ids = NULL,
                               type = NULL, where = NULL,
                               t_srs = NULL,
                               buffer = 0.5){

  # TODO: can generalize this to support other layers
  source <- data.frame(server = "3DHP_all",
                       user_call  = c("hydrolocation", "flowline", "waterbody",
                                      "drainage area", "catchment"),
                       layer  = c(0, 1, 2, 3, 4))

  if(is.null(type)) return(source)

  AOI <- check_query_params(AOI, ids, type, where, source, t_srs)
  t_srs <- AOI$t_srs
  AOI <- AOI$AOI

  here <- filter(source, .data$user_call == !!type)

  URL <- paste0(get("arcrest_root", envir = nhdplusTools_env),
                here$server,
                "/MapServer/", here$layer, "/query")

  spat_filter <- spatial_filter(AOI, tile = FALSE, format = "esri")

  if(!is.null(ids)) {

    if(!is.null(where)) stop("can't specify ids and where")

    where <- paste0("id3dhp IN ('",
                    paste(ids, collapse = "', '"), "')")
  }

  post_body <- list(where = where,
                    f = "json",
                    returnIdsOnly = "true")

  if(length(spat_filter[[1]]) > 0) {
    post_body <- c(list(geometry = spat_filter[[1]]),
                   post_body)
  }

  tryCatch({
    if(nhdplus_debug()) {
      message(paste(URL, "\n"))
      message(post_body)
    }

    ids <- content(RETRY("POST",
                         URL,
                         body = post_body,
                         encode = "form"))
    ids <- unlist(ids$objectIds)

  }, error = function(e) {
    warning("Something went wrong trying to access a service.")
    return(NULL)
  })

  length_ids <- length(ids)

  if(is.null(ids) | length(ids) == 0) {
    "nothing found for filter or web service failed"
    return(NULL)
  }

  chunk_size <- 2000
  ids <- split(ids, ceiling(seq_along(ids)/chunk_size))

  out <- rep(list(list()), length(ids))

  for(i in seq_along(ids)) {

    if(length_ids > chunk_size) {
      top <- i * chunk_size
      if(top > length_ids) top <- length_ids
      message("Getting features ", (i - 1) * chunk_size, " to ", top,  " of ", length_ids)
    }

    post_body <- list(objectIds = paste(ids[[i]], collapse = ","),
                      outFields = "*",
                      f = "geojson")

    tryCatch({
      if(nhdplus_debug()) {
        message(paste(URL, "\n"))
        message(post_body)
      }

      out[[i]] <- rawToChar(httr::RETRY("POST",
                                        URL,
                                        body = post_body,
                                        encode = "form")$content)
    }, error = function(e) {
      warning("Something went wrong trying to access a service.")
      return(NULL)
    })

    out[[i]] <- tryCatch({
      sf::st_zm(sf::read_sf(out[[i]]))},
      error = function(e) return(NULL))
  }

  if(inherits(out[[1]], "data.frame")) {
    out <- bind_rows(unify_types(out))

    out <- check_valid(out[!duplicated(out[["id3dhp"]]), ],
                       out_prj = t_srs)

  } else {

    out <- NULL

  }

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
    return(select(out, -any_of("OBJECTID")))
  } else {
    warning(paste("No", here$user_call, "features found"), call. = FALSE)
    NULL
  }

}
