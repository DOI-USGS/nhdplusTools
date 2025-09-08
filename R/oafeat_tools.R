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
           \(x) c(x[c("id", "title", "description")],
                  list(url = filter_list_kvp(x$links,
                                             "rel", "self", n = 1)$href))))


  q_ables <- dplyr::bind_rows(lapply(collections$collections, \(x) {
    q <- filter_list_kvp(x$links, "rel", "http://www.opengis.net/def/rel/ogc/1.0/queryables",
                         type = "application/schema+json", n = 1)$href |>
      mem_get_json() |>
      (\(y) list(id = x$id, qs = y$properties))()

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
                       AOI,
                       type = NULL,
                       t_srs = NULL,
                       buffer = 0.5,
                       status = TRUE) {

  avail <- discover_oafeat(base)

  if(is.null(type)) {
    warning("type is required, returning choices.")
    return(avail)
  }

  if(!type %in% avail$id) stop("Type must be in available ids: ", paste(avail$id, collapse = ", "), ".")

  base_call <- paste0(base, "collections/", type, "/items")

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
