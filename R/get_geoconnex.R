#' memoise get json
#' @description
#' attempts to get a url as JSON and return the content.
#'
#' Will return NULL if anything fails
#'
#' @param url character url to get
#' @return list containing parsed json on success, NULL otherwise
#' @noRd
mem_get_json <- memoise::memoise(\(url) {
  tryCatch({
    retn <- httr::RETRY("GET", url, httr::accept_json())

    if(retn$status_code == 200 & grepl("json", retn$headers$`content-type`)) {
      return(httr::content(retn, simplifyVector = FALSE, type = "application/json"))
    } else {
      warning("Can't access json from ", url)
      return(NULL)
    }
  }, error = function(e) {
    warning("Error accessing ", url, "\n\n", e)
    return(NULL)
  })
})

filter_list_kvp <- \(l, key, val, type = NULL, n = NULL) {
  ret <- l[vapply(l, \(x) x[[key]] == val, TRUE)]


  if(!is.null(type)) {
    ret <- ret[vapply(ret, \(x) x[["type"]] == type, TRUE)]
  }

  if(!is.null(n)) {
    ret <- ret[[n]]
  }

  ret
}

extract <- `[[`

#' discover geoconnex reference feature layers
#' @description
#' Queries the geoconnex.us reference feature server for available layers and
#' attributes.
#'
#' @return data.frame containing layers available and fields that are available to query.
#' @export
#' @examples
#' \donttest{
#'   discover_geoconnex_reference()
#' }
#'
discover_geoconnex_reference <- function() {

  landing <- mem_get_json(get("gocnx_ref_base_url", envir = nhdplusTools_env))

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

#' get geoconnex reference feature layers
#' @description
#' Queries the geoconnex reference feature server for features of interest.
#'
#' @param AOI  bbox, sf polygon or point, or a URL that will return an sf object when passed to
#' \link[sf]{read_sf}
#' @param type character the feature type chosen from \link{discover_geoconnex_reference}
#' @inheritParams query_usgs_geoserver
#' @param status boolean print status or not
#' @return sf data.frame containing requested reference features
#' @export
#' @examples
#' \donttest{
#'
#' dplyr::distinct(discover_geoconnex_reference()[c("id", "title")])
#'
#' AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
#'                                    xmax = -89.24681, ymax = 43.17192),
#'                                  crs = "+proj=longlat +datum=WGS84 +no_defs"))
#'
#' get_geoconnex_reference(AOI, type = "hu04")
#'
#' get_geoconnex_reference("https://geoconnex.us/ref/mainstems/315626", type = "hu04", )
#'
#' AOI <- sf::st_sfc(sf::st_point(c(-89.56684, 42.99816)),
#'                   crs = "+proj=longlat +datum=WGS84 +no_defs")
#'
#' get_geoconnex_reference(AOI, type = "hu04", buffer = 100000)
#'
#' }
get_geoconnex_reference <- function(AOI,
                                    type = NULL,
                                    t_srs = NULL,
                                    buffer = 0.5,
                                    status = TRUE) {

  avail <- discover_geoconnex_reference()

  if(is.null(type)) {
    warning("type is required, returning choices.")
    return(avail)
  }

  base <- get("gocnx_ref_base_url", envir = nhdplusTools_env)

  if(!type %in% avail$id) stop("Type must be in available ids. ",
                               "Check discover_geoconnex_reference()")

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

