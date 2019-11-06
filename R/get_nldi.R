#' @title Discover NLDI Sources
#' @description Function to retrieve available feature
#' and data sources from the Network Linked Data Index.
#' @param tier character optional "prod" or "test"
#' @return data.frame with three columns "source", "sourceName"
#' and "features"
#' @export
#' @examples
#' \donttest{
#' discover_nldi_sources()
#' }
discover_nldi_sources <- function(tier = "prod") {
  return(query_nldi(query = "", tier))
}

#' @title Discover NLDI Navigation Options
#' @description Discover available navigation options for a
#' given feature source and id.
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' discover_nldi_sources() and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @param tier character optional "prod" or "test"
#' @return data.frame with three columns "source", "sourceName"
#' and "features"
#' @export
#' @examples
#' \donttest{
#' discover_nldi_sources()
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
#'
#' discover_nldi_navigation(nldi_nwis)
#' }
discover_nldi_navigation <- function(nldi_feature, tier = "prod") {
  check_nldi_feature(nldi_feature)
  query <- paste(nldi_feature[["featureSource"]],
                 nldi_feature[["featureID"]],
                 "navigate", sep = "/")

  query_nldi(query, tier)
}

#' @title Navigate NLDI
#' @description Navigate the Network Linked Data Index network.
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' discover_nldi_sources() and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @param mode character chosen from names, URLs, or url parameters
#' returned by discover_nldi_navigation(nldi_feature). See examples.
#' @param data_source character chosen from "source" column of the response
#' of discover_nldi_sources() or empty string for flowline geometry.
#' @param distance_km numeric distance in km to stop navigating.
#' @param tier character optional "prod" or "test"
#' @return sf data.frame with result
#' @export
#' @importFrom utils tail
#' @examples
#' \donttest{
#' library(sf)
#' library(dplyr)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05428500")
#'
#' navigate_nldi(nldi_feature = nldi_nwis,
#'               mode = "upstreamTributaries",
#'               data_source = "") %>%
#'   st_geometry() %>%
#'   plot()
#'
#' navigate_nldi(nldi_feature = nldi_nwis,
#'               mode = "UM",
#'               data_source = "") %>%
#'   st_geometry() %>%
#'   plot(col = "blue", add = TRUE)
#'
#'
#'
#' nwissite <- navigate_nldi(nldi_feature = nldi_nwis,
#'                           mode = "UT",
#'                           data_source = "nwissite")
#'
#' st_geometry(nwissite) %>%
#'   plot(col = "green", add = TRUE)
#'
#' nwissite
#' }
#'
navigate_nldi <- function(nldi_feature, mode = "upstreamMain",
                          data_source = "comid", distance_km = NULL,
                          tier = "prod") {

  nav_lookup <- list(upstreamMain = "UM",
                     upstreamTributaries = "UT",
                     downstreamMain = "DM",
                     downstreamDiversions = "DD")

  if (nchar(mode) > 2) {
    if (nchar(mode) < 30) {
      mode <- nav_lookup[[mode]]
    } else {
      mode <- tail(unlist(strsplit(mode, "/")), n = 1)
    }
  }

  query <- paste(nldi_feature[["featureSource"]],
                 nldi_feature[["featureID"]],
                 "navigate", mode, data_source,
                 sep = "/")

  if (!is.null(distance_km)) {
    query <- paste0(query, "?distance=", distance_km)
  }

  out <- query_nldi(query, tier = tier, parse_json = FALSE)

  if(!is.null(out)) {
    return(sf::read_sf(out))
  }

  return(dplyr::tibble())

}

#' @title Get NLDI Basin Boundary
#' @description Get a basin boundary for a given NLDI feature.
#' @details Only resolves to the nearest NHDPlus catchment divide. See:
#' https://owi.usgs.gov/blog/nldi-intro/ for more info on the nldi.
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' discover_nldi_sources() and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @param tier character optional "prod" or "test"
#' @return sf data.frame with result basin boundary
#' @export
#' @examples
#' \donttest{
#' library(sf)
#' library(dplyr)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05428500")
#'
#' basin <- get_nldi_basin(nldi_feature = nldi_nwis)
#'
#' basin %>%
#'  st_geometry() %>%
#'  plot()
#'
#' basin
#' }
get_nldi_basin <- function(nldi_feature,
                          tier = "prod") {

  query <- paste(nldi_feature[["featureSource"]],
                 nldi_feature[["featureID"]],
                 "basin",
                 sep = "/")

  return(sf::read_sf(query_nldi(query, tier = tier, parse_json = FALSE)))

}



#' @noRd
get_nldi_feature <- function(f_source, f_id, tier = "prod") {
  return(query_nldi(paste(f_source, f_id,
                          sep = "/"),
                    tier))
}

#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @noRd
query_nldi <- function(query, tier = "prod", parse_json = TRUE) {
  nldi_base_url <- get_nldi_url(tier)

  url <- paste(nldi_base_url, query,
               sep = "/")

  req_data <- rawToChar(httr::RETRY("GET", url, times = 10, pause_cap = 240)$content)

  if (nchar(req_data) == 0) {
    NULL
  } else {
    if (parse_json) {
      tryCatch(jsonlite::fromJSON(req_data, simplifyVector = TRUE),
               error = function(e) {
                 message("Something went wrong accessing the NLDI.\n", e)
               }, warning = function(w) {
                 message("Something went wrong accessing the NLDI.\n", w)
               })
    } else {
      req_data
    }
  }
}

#' @noRd
get_nldi_url <- function(tier = "prod") {
  if (tier == "prod") {
    "https://cida.usgs.gov/nldi"
  } else if (tier == "test") {
    "https://cida-test.er.usgs.gov/nldi"
  } else {
    stop("only prod or test allowed.")
  }
}

#' @noRd
check_nldi_feature <- function(nldi_feature) {
  expect_names <- c("featureSource", "featureID")
  if (!all(expect_names %in%
           names(nldi_feature))) {
    stop(paste0("Missing some required input for NLDI. ",
                "Expected: ",
                paste(expect_names[which(!(expect_names %in%
                                             names(nldi_feature)))],
                      collapse = ", ")))
  }
}
