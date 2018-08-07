#' @title Discover NLDI Sources
#' @description Function to retrieve available feature
#' and data sources from the Network Linked Data Index.
#' @param tier character optional "prod" or "test"
#' @return data.frame with three columns "source", "sourceName"
#' and "features"
#' @export
#' @examples
#' discover_nldi_sources()
#'
discover_nldi_sources <- function(tier = "prod") {
  return(query_nldi(query = "", tier))
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
query_nldi <- function(query, tier = "prod", simplifyvector = TRUE) {
  nldi_base_url <- get_nldi_url(tier)

  url <- paste(nldi_base_url, query,
               sep = "/")

  c <- rawToChar(httr::GET(url)$content)

  if (nchar(c) == 0) {
    NULL
  } else {
    try(jsonlite::fromJSON(c, simplifyVector = simplifyvector), silent = FALSE)
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
