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
                       user_call  = c('huc08', "huc12_nhdplusv2",
                                      'nhd','catchment', 'nhdarea',
                                      'nonnetwork',
                                      'waterbodies',
                                      'gagesII', "gagesII-basin"),
                       layer_name  = c("nhdplusv2-huc08", "nhdplusv2-huc12",
                                       "nhdflowline_network", "catchmentsp", 'nhdarea',
                                       "nhdflowline_nonnetwork",
                                       "nhdwaterbody",
                                       "gagesii", "gagesii-basins"),
                       geom_name = c("the_geom", "the_geom",
                                     "the_geom", "the_geom", "the_geom",
                                     "the_geom",
                                     "the_geom",
                                     "the_geom", "the_geom"),
                       ids        = c("huc8", "huc12",
                                      "comid", "featureid", "comid",
                                      "comid",
                                      "comid",
                                      "staid", "gage_id"),
                       page       = c(FALSE, FALSE,
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

  get_oafeat(base, AOI = AOI, type = type, ids = ids, t_srs = t_srs, buffer = buffer, status = FALSE)

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
#' @inheritParams query_usgs_geoserver
#' @param status boolean print status or not
#' @return sf data.frame containing requested features
#' @noRd
get_oafeat <- function(base,
                       AOI, ids = NULL,
                       type = NULL,
                       t_srs = NULL,
                       buffer = 0.5,
                       status = TRUE) {

  avail <- discover_oafeat(base)

  if(is.null(type)) {
    warning("type is required, returning choices.")
    return(avail)
  }

  if(!type %in% avail$id) stop("Type must be in available ids: ", paste(unique(avail$id), collapse = ", "), ".")

  avail <- filter(avail, id == !!type)

  base_call <- paste0(base, "collections/", type, "/items")

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
  } else {

    id_attribute <- names(ids)

    ids <- ids[[1]]

    base_call <- paste0(base_call, "?", id_attribute, "=", paste(ids, collapse = ","))

  }

  out <- get_features_paging(base_call, status = status)

  if(!is.null(t_srs)) out <- st_transform(out, t_srs)

  out
}

get_features_paging <- function(base_call, limit = 1000, status = TRUE) {

  if(!grepl("\\?", base_call)) {
    base_call <- paste0(base_call, "?")
  } else {
    base_call <- paste0(base_call, "&")
  }

  offset <- 0

  keep_going <- TRUE

  if(status) message("Starting download of first set of features.")

  out <- rep(list(list()), 1e6)
  i <- 1

  while(keep_going) {
    req <- paste0(base_call, "limit=", limit, "&offset=", offset)

    out[[i]] <- try(read_sf(req))

    if(inherits(out[[i]], "sf") & nrow(out[[i]]) == limit) {
      offset <- offset + limit
    }

    if(nrow(out[[i]]) < limit) keep_going <- FALSE

    if(!inherits(out[[i]], "sf")) {
      warning("Something went wrong requesting data.")
      keep_going <- FALSE
    }

    if(status & keep_going) message("Starting next download from ", offset, ".")

    i <- i + 1
  }

  out <- out[1:(i - 1)]

  sf::st_sf(dplyr::bind_rows(unify_types(out)))
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
