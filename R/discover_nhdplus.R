#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#' @param point An sf POINT including crs as created by:
#' sf::st_sfc(sf::st_point(..,..), crs)
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' \link[dataRetrieval]{get_nldi_sources} and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @param raindrop logical if \code{TRUE} will call a raindrop trace web service and
#' return will be a \code{list} containing the COMID and a \code{sfg} linestring trace
#' to the nearest flowline. Ignored if \code{nldi_feature} is supplied rather than
#' \code{point}.
#' @return integer COMID or list containing COMID and raindrop trace.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
#' discover_nhdplus_id(point)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
#' discover_nhdplus_id(nldi_feature = nldi_nwis)
#' }
#'

discover_nhdplus_id <- function(point = NULL, nldi_feature = NULL, raindrop = FALSE) {
  if (!is.null(point)) {
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
#' @param point sfg point to trace from
#' @return sf data.frame containing raindrop trace and requested
#' portion of flowline.
#' @export
#' @examples
#' point <- sf::st_point(x = c(-89.2158, 42.9561), dim = "XY")
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

get_raindrop_trace <- function(point, direction = "down") {

  url_base <- "https://labs.dev-wma.chs.usgs.gov/api/nldi/pygeoapi/processes/"

  url <- paste0(url_base, "nldi-flowtrace/jobs?response=document")

  allowed_direction <- c("up", "down", "none")

  if(!direction %in% allowed_direction)
    stop(paste("direction must be in",
               allowed_direction))

  tryCatch({

    out <- httr::RETRY("POST", url, httr::accept_json(),
                      httr::content_type_json(),
                      body = make_json_input_trace(point, direction = direction))

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
