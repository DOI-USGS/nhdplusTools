#' @title Query USGS Water OGC API Features
#' @description Query the USGS Water OGC API for spatial data by location,
#' area, or ID.
#' @details The returned object(s) will have the same
#' Spatial Reference System (SRS) as the input AOI. If a individual or set of
#' IDs are used to query, then the default server CRS of EPSG:4326 is
#' preserved. In all cases, a user-defined SRS can be passed to \code{t_srs}
#' which will override all previous SRS (either input or default).
#' All buffer and distance operations are handled internally using an
#' EPSG:5070 Albers Equal Area projection
#' @param AOI sf (MULTI)POINT or (MULTI)POLYGON. An 'area of interest' can
#' be provided as either a location (sf POINT) or area (sf POLYGON)
#' in any Spatial Reference System.
#' @param ids character or numeric. A set of identifier(s) from the data
#' type requested, for example if NHDPlusV2, then a set of COMID(s).
#' @param type character. Type of feature to return
#' ('huc08','huc12', 'nhd', 'catchment', 'waterbodies', 'gagesII').
#' If NULL (default) a data.frame of available resources is returned
#' @param filter character. An filter to pass to the query
#' @param t_srs  character (PROJ string or EPSG code) or numeric (EPSG code).
#' A user specified - target -Spatial Reference System (SRS/CRS) for returned objects.
#' Will default to the CRS of the input AOI if provided, and to 4326 for ID requests.
#' @param buffer numeric. The amount (in meters) to buffer a POINT AOI by for an
#' extended search. Default = 0.5
#' @return a simple features (sf) object
#' @keywords internal
query_usgs_oafeat <- function(AOI = NULL,  ids = NULL,
                              type = NULL, filter = NULL,
                              t_srs = NULL,
                              buffer = 0.5) {

  base <- get("usgs_water_root", envir = nhdplusTools_env)

  source <- data.frame(server = 'usgs_oafeat',
                       user_call  = c('huc02_2020', 'huc04_2020', 'huc06_2020',
                                      'huc08_2020', 'huc10_2020', 'huc12_2020',
                                      'huc08_nhdplusv2', "huc12_nhdplusv2",
                                      'nhd','catchment', 'nhdarea',
                                      'nonnetwork',
                                      'waterbodies',
                                      'gagesII', "gagesII-basin"),
                       layer_name  = c("wbd02_20201006", "wbd04_20201006", "wbd06_20201006",
                                       "wbd08_20201006", "wbd10_20201006", "wbd12_20201006",
                                       "nhdplusv2-huc08", "nhdplusv2-huc12",
                                       "nhdflowline_network", "catchmentsp", 'nhdarea',
                                       "nhdflowline_nonnetwork",
                                       "nhdwaterbody",
                                       "gagesii", "gagesii-basins"),
                       geom_name = c("SHAPE", "SHAPE", "SHAPE",
                                     "SHAPE", "SHAPE", "SHAPE",
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
                       page       = c(FALSE, FALSE, FALSE,
                                      FALSE, FALSE, FALSE,
                                      FALSE, FALSE,
                                      TRUE, TRUE, TRUE,
                                      TRUE,
                                      TRUE,
                                      FALSE, TRUE))

  if(is.null(type)) {
    return(source)
  }

  AOI <- check_query_params(AOI, ids, type, NULL, source, t_srs, buffer)
  t_srs <- AOI$t_srs
  AOI <- AOI$AOI

  here <- filter(source, .data$user_call == !!type)

  type <- here$layer_name

  if(!is.null(ids)) {
    ids <- stats::setNames(list(ids), here$ids)
  }

  out <- get_oafeat(base, AOI = AOI, type = type, ids = ids, filter = filter, t_srs = t_srs, buffer = buffer, status = FALSE)

  out <- check_valid(out)

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
    return(out)
  } else {
    warning(paste("No", here$user_call, "features found"), call. = FALSE)
    NULL
  }

}

#' discover OGC API feature layers
#' @description
#' Queries an OGC API feature server for available layers and
#' attributes.
#'
#' @return data.frame containing layers available and fields that are available to query.
#' @param landing_url url of landing page of OGC API
#' @noRd
#'
discover_oafeat <- function(landing_url) {

  landing <- mem_get_json(landing_url)

  collections <- landing$links |>
    filter_list_kvp("rel", "data", n = 1) |>
    extract("href") |>
    mem_get_json()

  collections_meta <- dplyr::bind_rows(
    lapply(collections$collections,
           \(x) c(x[c("id", "title", "description", "itemType")],
                  list(url = filter_list_kvp(x$links,
                                             "rel", "self", n = 1)$href)))) |>
    dplyr::filter(.data$itemType == "feature") |>
    dplyr::select(-"itemType")


  q_ables <- dplyr::bind_rows(lapply(collections$collections, \(x) {
    if(x$itemType != "feature") return(NULL)

    q <- filter_list_kvp(x$links, "rel", "http://www.opengis.net/def/rel/ogc/1.0/queryables",
                         type = "application/schema+json", n = 1)$href |>
      mem_get_json() |>
      (\(y) if(is.null(y)) y else list(id = x$id, qs = y$properties))()

    q$qs <- q$qs[vapply(q$qs, \(x) all(c("title", "type") %in% names(x)), TRUE)]

    data.frame(id = rep(q$id, length(q$qs)),
               attribute = vapply(q$qs, \(x) x$title, ""),
               type = vapply(q$qs, \(x) x$type, ""), row.names = NULL)
  }))

  dplyr::left_join(collections_meta, q_ables, by = "id")

}

#' get geoconnex reference feature layers
#' @description
#' Queries the geoconnex reference feature server for features of interest.
#'
#' @param AOI  bbox, sf polygon or point, or a URL that will return an sf object when passed to
#' \link[sf]{read_sf}
#' @param type character the feature type desired
#' @param filter character CQL filter string
#' @inheritParams query_usgs_oafeat
#' @param status boolean print status or not
#' @return sf data.frame containing requested features
#' @noRd
get_oafeat <- function(base,
                       AOI,
                       ids = NULL,
                       type = NULL,
                       filter = NULL,
                       t_srs = NULL,
                       buffer = 0.5,
                       status = TRUE) {

  avail <- discover_oafeat(base)

  if(is.null(type)) {
    warning("type is required, returning choices.")
    return(avail)
  }

  if(!type %in% avail$id) stop("Type must be in available ids: ", paste(unique(avail$id), collapse = ", "), ".")

  avail <- filter(avail, .data$id == !!type)

  base_call <- paste0(base, "collections/", type, "/items")
  post_body <- list()

  limit <- 1000

  if(!is.null(AOI)) {
    if(is.character(AOI)) {

      AOI <- try(sf::read_sf(AOI))

      if(!inherits(AOI, "sf")) {
        stop("AOI did not return an sf object when read")
      }

    }

    if(!inherits(AOI, "bbox")) {
      AOI <- st_bbox(AOI)
    } else if(!inherits(AOI, "bbox") &&
              grepl("point", sf::st_geometry_type(AOI), ignore.case = TRUE)) {
      AOI <- sf::st_buffer(AOI, units::as_units(buffer, "m"))
    }

    # pull features with paging if necessary

    bbox <- paste(AOI, collapse = ",")

    base_call <- paste0(base_call, "?bbox=", bbox)

  } else if(!is.null(ids)) {

    if(!is.null(filter)) stop("filter not supported with ids")

    id_attribute <- names(ids)

    ids <- ids[[1]]

    if(length(ids) == 1) {
      base_call <- paste0(base_call, paste0("?", id_attribute, "=", ids))
    } else {
      post_body <- list(ids = ids, id_attribute = id_attribute)
      limit <- 500
    }

  }

  if(!is.null(filter)) {
    base_call <- paste0(add_sep(base_call), "filter=", filter)
  }

  out <- get_features_paging(base_call, post_body, limit = limit, status = status)

  if(!is.null(t_srs)) out <- st_transform(out, t_srs)

  out
}

add_sep <- function(bc) {
  if(!grepl("\\?", bc)) {
    bc <- paste0(bc, "?")
  } else {
    bc <- paste0(bc, "&")
  }
  bc
}

make_request <- function(req, body = "") {
  tryCatch({
    if(nhdplus_debug()) {
      message(paste(req, "\n"))
      message(body)
    }

    if(body != "") {
      out <- rawToChar(RETRY("POST",
                             req,
                             body = body,
                             httr::content_type("application/query-cql-json"))$content)
    } else {

      out <- rawToChar(RETRY("GET", req)$content)

    }


  }, error = function(e) {
    warning("Something went wrong trying to access a service.")
    return(NULL)
  })

  out <- tryCatch({
    st_zm(read_sf(out))},
    error = function(e) return(NULL))

  out
}

get_features_paging <- function(base_call, ids_list = list(), limit = 1000, status = TRUE) {

  if(!identical(ids_list, list())) {
    # we will page through ids
    ids <- split_equal_size(ids_list$ids, limit)
  }

  base_call <- add_sep(base_call)


  post_body <- ""

  offset <- 0

  keep_going <- TRUE

  if(status) message("Starting download of first set of features.")

  out <- rep(list(list()), 1e6)
  i <- 1

  while(keep_going) {

    if(!identical(ids_list, list())) {

      req <- base_call
      post_body <- id_filter_cql(ids[[i]], ids_list$id_attribute)
      req <- paste0(base_call, "limit=", limit)

    } else {

      req <- paste0(base_call, "limit=", limit, "&offset=", offset)

    }

    out[[i]] <- make_request(req, post_body)

    if(!is.null(out[[i]]) && inherits(out[[i]], "sf") & nrow(out[[i]]) == limit) {
      offset <- offset + limit
    }

    if(nrow(out[[i]]) < limit) keep_going <- FALSE

    if(!inherits(out[[i]], "sf")) {
      warning("Something went wrong requesting data.")
      keep_going <- FALSE
    }

    if(status & keep_going) {
      if(identical(ids_list, list())) {
        message("Starting next download from ", offset, ".")
      } else {
        message("starting next download from ", i * limit, ".")
      }
    }

    i <- i + 1
  }

  out <- out[1:(i - 1)]

  sf::st_sf(dplyr::bind_rows(unify_types(out)))
}

id_filter_cql  <- function(ids, name = "comid"){

  jsonlite::toJSON(list(op = "in", args = list(list(property = name), c(ids))),
                   auto_unbox = TRUE)

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
