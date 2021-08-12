#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#' @param point sfc POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' \link[dataRetrieval]{get_nldi_sources} and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @param raindrop logical if \code{TRUE} will call a raindrop trace web service and
#' return will be the same as \link{get_raindrop_trace} with direction "none".
#' @return integer COMID or list containing COMID and raindrop trace.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(c(-76.874, 39.482)), crs = 4326)
#' discover_nhdplus_id(point)
#'
#' discover_nhdplus_id(point, raindrop = TRUE)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
#' discover_nhdplus_id(nldi_feature = nldi_nwis)
#' }
#'

discover_nhdplus_id <- function(point = NULL, nldi_feature = NULL, raindrop = FALSE) {
  if (!is.null(point)) {

    point <- check_point(point)

    if(raindrop) {
      out <- get_raindrop_trace(point, "none")

      return(out)
    }
    coords = sf::st_coordinates(point)
    comid  = dataRetrieval::findNLDI(location = c(X = coords[1],
                                                  Y = coords[2]))$identifier
    return(as.integer(comid))
  } else if (!is.null(nldi_feature)) {

    nldi <- get_nldi_feature(nldi_feature)

    return(as.integer(nldi$comid))

  } else {
    stop("Must provide point or nldi_feature input.")
  }
}

#' Get Raindrop Trace
#' @description Uses a raindrop trace web service to trace the
#' nhdplus digital elevation model to the nearest downslop flowline.
#' @param point sfc POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}
#' @param direction character \code{"up"}, \code{"down"}, or \code{"none"}.
#' Controls the portion of the split flowline that is returned along with
#' the raindrop trace line.
#' @return sf data.frame containing raindrop trace and requested
#' portion of flowline.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)
#'
#' (trace <- get_raindrop_trace(point))
#'
#' bbox <- sf::st_bbox(trace) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' nhdplusTools::plot_nhdplus(bbox = bbox)
#'
#' plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE)
#' plot(sf::st_transform(sf::st_geometry(trace)[1], 3857), add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_geometry(trace)[2], 3857), add = TRUE, col = "black")
#' }
#'
get_raindrop_trace <- function(point, direction = "down") {

  point <- check_point(point)[[1]]

  url_base <- paste0(get_nldi_url(tier = "prod"), "/pygeoapi/processes/")

  url <- paste0(url_base, "nldi-flowtrace/jobs?response=document")

  allowed_direction <- c("up", "down", "none")

  if(!direction %in% allowed_direction)
    stop(paste("direction must be in",
               paste(allowed_direction, collapse = ", ")))

  return(sf_post(url, make_json_input_trace(point, direction = direction)))

}

#' Get split catchment
#' @description Uses catchment splitting web service to retrieve
#' the portion of a catchment upstream of the point provided.
#' @param point scf POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}
#' @param upstream logical If TRUE, the entire drainage basin upstream
#' of the point provided is returned in addition to the local catchment.
#' @return sf data.frame containing the local catchment, the split portion
#' and optionally the total dranage basin.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)
#'
#' trace <- get_raindrop_trace(point)
#'
#' (snap_point <- sf::st_sfc(sf::st_point(trace$intersectionPoint[[1]][2:1]),
#'                           crs = 4326))
#'
#' (catchment <- get_split_catchment(snap_point))
#'
#' bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' nhdplusTools::plot_nhdplus(bbox = bbox)
#'
#' plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
#' plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE, col = "white")
#'
#' (catchment <- get_split_catchment(snap_point, upstream = FALSE))
#'
#' bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' nhdplusTools::plot_nhdplus(bbox = bbox)
#'
#' plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
#' plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE, col = "white")
#'
#' pour_point <- sf::st_sfc(sf::st_point(x = c(-89.25619, 42.98646)), crs = 4326)
#'
#' (catchment <- get_split_catchment(pour_point, upstream = FALSE))
#'
#' bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' nhdplusTools::plot_nhdplus(bbox = bbox)
#'
#' plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
#' plot(sf::st_transform(sf::st_sfc(pour_point, crs = 4326), 3857), add = TRUE, col = "white")
#'}
#'
get_split_catchment <- function(point, upstream = TRUE) {

  point <- check_point(point)[[1]]

  url_base <- paste0(get_nldi_url(tier = "prod"), "/pygeoapi/processes/")

  url <- paste0(url_base, "nldi-splitcatchment/jobs?response=document")

  return(sf_post(url, make_json_input_split(point, upstream)))
}

sf_post <- function(url, json) {
  tryCatch({

    out <- httr::RETRY("POST", url, httr::accept_json(),
                       httr::content_type_json(),
                       body = json)

    if(out$status_code == 200) {
      sf::read_sf(rawToChar(out$content))
    } else {
      stop(rawToChar(out$content))
    }

  }, error = function(e) {
    message("Error calling processing service. \n Original error: \n", e)
    NULL
  })
}


check_point <- function(p) {
  mess <- "Point must be of type sfc and have a CRS declared."

  if(!inherits(p, "sfc")) stop(mess)

  tryCatch({
    sf::st_transform(p, 4326)
  }, error = function(e) {
    stop(paste(mess, "Original error was: \n", e))
  })
}

make_json_input_trace <- function(p, raindrop = TRUE, direction = "down") {

  jsonlite::toJSON(list(inputs = list(list(id = "lat",
                                           type = "text/plain",
                                           value = p[2]),
                                      list(id = "lon",
                                           type = "text/plain",
                                           value = p[1]),
                                      list(id = "raindroptrace",
                                           type = "text/plain",
                                           value = ifelse(raindrop,
                                                          "true", "false")),
                                      list(id = "direction",
                                           type = "text/plain",
                                           value = direction))),
                   pretty = TRUE, auto_unbox = TRUE)
}

make_json_input_split <- function(p, upstream = TRUE) {

  jsonlite::toJSON(list(inputs = list(list(id = "lat",
                                           type = "text/plain",
                                           value = p[2]),
                                      list(id = "lon",
                                           type = "text/plain",
                                           value = p[1]),
                                      list(id = "upstream",
                                           type = "text/plain",
                                           value = ifelse(upstream,
                                                          "true", "false")))),
                   pretty = TRUE, auto_unbox = TRUE)

}
