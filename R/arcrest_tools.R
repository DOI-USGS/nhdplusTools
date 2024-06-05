get_3dhp_service_info <- memoise::memoise(function() {

  # TODO: support more services?
  server <- "3DHP_all"

  url_base <- paste0(get("arcrest_root", envir = nhdplusTools_env),
                     server,
                     "/MapServer/")

  all_layers <- jsonlite::read_json(paste0(url_base, "?f=json"))

  list(url_base = url_base, all_layers = all_layers)

})

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

  si <- get_3dhp_service_info()

  source <- data.frame(user_call = sapply(si$all_layers$layers, \(x) tolower(x$name)),
                       layer = sapply(si$all_layers$layers, \(x) x$id))

  group_layers <- si$all_layers$layers[sapply(si$all_layers$layers,
                                              \(x) grepl("Group Layer", x$type))]

  if(is.null(type)) {
    message("`type` input must be one of: \n\t\"",
            paste(source$user_call, collapse = "\"\n\t\""), "\"")
    return(source)
  }

  if(!type %in% source$user_call) {
    warning("\"", type, "\" not in `type` input. Must be one of: \n\t\"",
            paste(source$user_call, collapse = "\"\n\t\""), "\"")
    return(NULL)
  }

  if(grepl(paste(sapply(group_layers, \(x) x$name),
                 collapse = "|"),
           type, ignore.case = TRUE)) {
    layer_id <- filter(source, .data$user_call == !!type)$layer

    group_layer <- group_layers[[sapply(group_layers, \(x) x$id == layer_id)]]

    need_layers <- as.integer(group_layer$subLayerIds)
  } else {
    need_layers <- as.integer(filter(source, .data$user_call == !!type)$layer)
  }

  all_out <- rep(list(list()), length(need_layers))

  for(l in seq_len(length(all_out))) {

    layer <- need_layers[l]

    type <- filter(source, .data$layer == !!layer)$user_call

    AOI <- check_query_params(AOI, ids, type, where, source, t_srs, buffer)
    t_srs <- AOI$t_srs
    AOI <- AOI$AOI

    here <- filter(source, .data$user_call == !!type)

    URL <- paste0(si$url_base, here$layer, "/query")

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

      all_ids <- content(RETRY("POST",
                               URL,
                               body = post_body,
                               encode = "form"))
      all_ids <- unlist(all_ids$objectIds)

    }, error = function(e) {
      warning("Something went wrong trying to access a service.")
      out <- NULL
    })

    length_ids <- length(all_ids)

    if(is.null(all_ids) | length(all_ids) == 0) {
      warning(paste("No", here$user_call, "features found in area of interest."), call. = FALSE)
      out <- NULL
    } else {

      chunk_size <- 2000
      all_ids <- split(all_ids, ceiling(seq_along(all_ids)/chunk_size))

      out <- rep(list(list()), length(all_ids))

      for(i in seq_along(all_ids)) {

        if(length_ids > chunk_size) {
          top <- i * chunk_size
          if(top > length_ids) top <- length_ids
          message("Getting features ", (i - 1) * chunk_size, " to ", top,  " of ", length_ids)
        }

        post_body <- list(objectIds = paste(all_ids[[i]], collapse = ","),
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
          out <- NULL
        })

        out[[i]] <- tryCatch({
          sf::st_zm(sf::read_sf(out[[i]]))},
          error = function(e) NULL)
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
          warning(paste(here$user_call, "features found but were outside area of interest polygon."))
          out = NULL

        }
      }

      if(!is.null(out)) {
        out <- select(out, -any_of("OBJECTID"))
      } else {
        warning(paste("No", here$user_call, "features found"), call. = FALSE)
        out <- NULL
      }
    }
    all_out[[l]] <- out
  }
  tryCatch(sf::st_sf(data.table::rbindlist(all_out)), error = function(e) NULL)
}
