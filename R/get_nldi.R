#' @title Discover Characteristics Metadata
#' @description Provides access to metadata for characteristics that are returned by `get_nldi_characteristics()`.
#' @param type character "all", "local", "total", or "divergence_routed".
#' @export
#' @return data.frame containing available characteristics
#' @examples
#' chars <- discover_nldi_characteristics()
#' names(chars)
#' head(chars$local, 10)
discover_nldi_characteristics <- function(type="all") {

  tc <- type_check(type)

  out <- lapply(tc$type_options[[type]], function(x) {
    o <- query_nldi(paste0(x, "/characteristics"),
                    base_path = "/lookups")
    o$characteristicMetadata$characteristic
  })

  names(out) <- tc$char_names

  out
}

type_check <- function(type) {
  type_options <- list("all" = c("local", "tot", "div"),
                       "local" = "local",
                       "total" = "tot",
                       "divergence_routed" = "div")

  if(!type %in% names(type_options)) stop(paste("Type must be one of", paste(names(type_options), collapse = ", ")))

  char_names <- type

  if(type == "all") char_names <- names(type_options)[2:4]

  return(list(type_options = type_options, char_names = char_names))
}

#' @title Navigate NLDI
#' @description Navigate the Network Linked Data Index network.
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' dataRetrieval::get_nldi_sources() and the `featureID` is a known identifier
#' from the specified `featureSource`.
#' @param mode character chosen from ("UM", "UT", DM", "DD"). See examples.
#' @param data_source character chosen from "source" column of the response
#' of dataRetrieval::get_nldi_sources() or empty string for flowline geometry.
#' @param distance_km numeric distance in km to stop navigating.
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
#'               mode = "upstreamTributaries")$UT %>%
#'   st_geometry() %>%
#'   plot()
#'
#' navigate_nldi(nldi_feature = nldi_nwis,
#'               mode = "UM")$UM %>%
#'   st_geometry() %>%
#'   plot(col = "blue", add = TRUE)
#'
#'
#'
#' nwissite <- navigate_nldi(nldi_feature = nldi_nwis,
#'                           mode = "UT",
#'                           data_source = "nwissite")$UT_nwissite
#'
#' st_geometry(nwissite) %>%
#'   plot(col = "green", add = TRUE)
#'
#' nwissite
#' }
#'
navigate_nldi <- function(nldi_feature, mode = "upstreamMain",
                          data_source = "flowlines", distance_km = 10) {

  nldi_feature <- check_nldi_feature(nldi_feature)

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

  # For backward compatibility
  if(data_source == "flowline" | data_source == "") {
    data_source <- "flowlines"
    warning("data source specified as flowline or '' is deprecated")
  }

  dataRetrieval::findNLDI(origin = nldi_feature,
                          nav = mode,
                          find = data_source,
                          distance_km = distance_km,
                          no_sf = FALSE)
}

#' @title Get NLDI Basin Boundary
#' @description Get a basin boundary for a given NLDI feature.
#' @details Only resolves to the nearest NHDPlus catchment divide. See:
#' https://waterdata.usgs.gov/blog/nldi-intro/ for more info on the nldi.
#' @inheritParams navigate_nldi
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
get_nldi_basin <- function(nldi_feature) {

  nldi_feature <- check_nldi_feature(nldi_feature)

  dataRetrieval::findNLDI(origin = nldi_feature, find = "basin")$basin

}


#' @title Get NLDI Feature
#' @description Get a single feature from the NLDI
#' @inheritParams navigate_nldi
#' @return sf data.frame with one feature
#' @examples
#' \donttest{
#' get_nldi_feature(list("featureSource" = "nwissite", featureID = "USGS-05428500"))
#' }
#' @export
get_nldi_feature <- function(nldi_feature) {
  nldi_feature <- check_nldi_feature(nldi_feature)

  return(dataRetrieval::findNLDI(origin = nldi_feature))
}

#' @title Get Catchment Characteristics
#' @description Retrieves catchment characteristics from the Network Linked Data Index.
#' Metadata for these characteristics can be found using `discover_nldi_characteristics()`.
#' @inheritParams navigate_nldi
#' @inheritParams discover_nldi_characteristics
#' @return data.frame contianing requested characteristics
#' @export
#' @examples
#' chars <- get_nldi_characteristics(list(featureSource = "nwissite", featureID = "USGS-05429700"))
#' names(chars)
#' head(chars$local, 10)
get_nldi_characteristics <- function(nldi_feature, type="local") {

  tc <- type_check(type)

  nldi_feature <- check_nldi_feature(nldi_feature, convert = FALSE)

  out <- lapply(tc$type_options[[type]], function(x) {
    o <- query_nldi(paste(nldi_feature[["featureSource"]],
                          nldi_feature[["featureID"]],
                          x,
                          sep = "/"))
    o$characteristics
  })

  names(out) <- tc$char_names

  out

}

#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @noRd
query_nldi <- function(query, tier = "prod", base_path = "/linked-data", parse_json = TRUE) {
  nldi_base_url <- paste0(get_nldi_url(tier), base_path)

  url <- paste(nldi_base_url, query,
               sep = "/")

  req_data <- rawToChar(httr::RETRY("GET", url, times = 3, pause_cap = 60)$content)

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
check_nldi_feature <- function(nldi_feature, convert = TRUE) {
  expect_names <- c("featureSource", "featureID")
  if (!all(expect_names %in% names(nldi_feature))) {
    if(length(nldi_feature) != 2 | !all(sapply(nldi_feature, is.character)))
      stop(paste0("Missing some required input for NLDI. ",
                  "Expected length 2 character vector or list with optional names: ",
                  paste(expect_names[which(!(expect_names %in%
                                               names(nldi_feature)))],
                        collapse = ", ")))
  }

  nldi_feature <- as.list(nldi_feature)
  names(nldi_feature) <- expect_names

  if(nldi_feature$featureSource == "nwissite" && convert) {
    if(grepl("^USGS-", nldi_feature$featureID)) {
      nldi_feature$featureID <- gsub("USGS-", "", nldi_feature$featureID)
      nldi_feature$featureSource <- "nwis"
    }
  }

  names(nldi_feature) <- expect_names

  if(convert) {
    stats::setNames(list(nldi_feature[["featureID"]]), nldi_feature[["featureSource"]])
  } else {
    nldi_feature
  }
}
