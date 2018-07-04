COMID <- COMID.y <- Divergence <- DnHydroseq <-
  DnMinorHyd <- FTYPE <- FromNode <-
  Hydroseq <- ID <- LENGTHKM <- LevelPathI <-
  Pathlength <- StartFlag <- StreamCalc <-
  StreamOrde <- TerminalFl <- TerminalPa <-
  ToNode <- TotDASqKM <- becomes <- dsLENGTHKM <-
  ds_joined_fromCOMID <- ds_num_upstream <- fID <-
  fromCOMID <- fromLENGTHKM <- fromTotDASqKM <- geom_len <-
  geometry <- join_category <- joined_fromCOMID <-
  joined_fromCOMID_new <- joined_toCOMID <- member_COMID <-
  new_joined_fromCOMID <- new_joined_toCOMID <- new_toCOMID <-
  num_upstream <- part <- piece <- pieces <- removed_COMID <-
  split_fID <- toCOMID <- toID <- usLENGTHKM <- usTotDASqKM <-
  . <- L1 <- X <- Y <- breaks <- dist_ratio <- ideal_len <-
  len <- nID <- new_index <- piece_len <- setNames <- start <-
  FromMeas <- REACHCODE <- REACH_meas <- ToMeas <-
  index <- measure <- nn.idx <- precision_index <- NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    "USGS Research Package:
    https://owi.usgs.gov/R/packages.html#research"),
    collapse = "\n"))
}

#' Sample flowlines from the Petapsco River.
#'
#' A sample set of flowlines.
#'
#' @format An sf data.frame
#'
#' @source \url{https://www.epa.gov/waterdata/nhdplus-national-data}
"sample_flines"

#' @title Discover NHDPlus Catchment/Flowline ID
#' @description Mutlipurpose function to find a COMID of interest.
#' @param point An sf POINT including crs as created by:
#' sf::st_sf(sf::st_point(..,..), crs)
#' @param  nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the
#' \href{https://cida.usgs.gov/nldi/}{list here.} and the `featureSource` is
#' a known identifier from the specified `featureSource`.
#' @return integer of catchment identifier COMID
#' @export
#' @examples
#' point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
#' discover_nhdplus_id(point)
#'
#' nldi_huc12 <- list(featureSource = "huc12pp", featureID = "070700051701")
#' discover_nhdplus_id(nldi_feature = nldi_huc12)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
#' discover_nhdplus_id(nldi_feature = nldi_nwis)
#'
discover_nhdplus_id <- function(point = NULL, nldi_feature = NULL) {

  if (!is.null(point)) {

    url_base <- paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows",
                       "?service=WFS",
                       "&version=1.0.0",
                       "&request=GetFeature",
                       "&typeName=nhdplus:catchmentsp",
                       "&outputFormat=application%2Fjson",
                       "&srsName=EPSG:4269")
    # "&bbox=40,-90.001,40.001,-90,urn:ogc:def:crs:EPSG:4269",

    p_crd <- sf::st_coordinates(sf::st_transform(point, 4269))

    url <- paste0(url_base, "&bbox=",
                  paste(p_crd[2], p_crd[1],
                        p_crd[2] + 0.00001, p_crd[1] + 0.00001,
                        "urn:ogc:def:crs:EPSG:4269", sep = ","))

    catchment <- sf::read_sf(url)

    if (nrow(catchment) > 1) {
      warning("point too close to edge of catchment found multiple.")
    }

    return(as.integer(catchment$featureid))

  } else if (!is.null(nldi_feature)) {

    nldi <- query_nldi(nldi_feature[["featureSource"]],
                       nldi_feature[["featureID"]])

    return(as.integer(nldi$features$properties$comid))

  } else {

    stop("Must provide point or nldi_feature input.")

  }
}

get_dsLENGTHKM <- function(flines) {
  # This gets all the next-downstream flowlines and finds the
  # length of the next downstream
  flines$dsLENGTHKM <-
    flines[["LENGTHKM"]][match(flines$toCOMID, flines$COMID)]
  # already removed comids get NA dsLength -- ok to set them to 0.
  flines[["dsLENGTHKM"]][is.na(flines$dsLENGTHKM)] <- 0
  flines[["dsLENGTHKM"]]
}

get_upstream <- function(flines) {
  left_join(select(flines, COMID), select(flines, COMID, toCOMID),
            by = c("COMID" = "toCOMID")) %>%
    rename(fromCOMID = COMID.y)
}

get_num_upstream <- function(flines) {
  left_join(select(flines, COMID, toCOMID),
            get_upstream(flines) %>%
              group_by(COMID) %>%
              summarise(num_upstream = n()),
            by = "COMID")[["num_upstream"]]
}

get_ds_num_upstream <- function(flines) {
  flines <- mutate(flines, num_upstream = get_num_upstream(flines))
  flines[["num_upstream"]][match(flines$toCOMID, flines$COMID)]
}

get_ds_joined_fromCOMID <- function(flines) {
  flines <- mutate(flines, ds_joined_fromCOMID = joined_fromCOMID)
  flines[["ds_joined_fromCOMID"]][match(flines$toCOMID, flines$COMID)]
}

#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @noRd
query_nldi <- function(f_source, f_id, tier = "prod") {
  nldi_base_url <- get_nldi_url(tier)

  url <- paste(nldi_base_url, f_source, f_id,
               sep = "/")

  c <- rawToChar(httr::GET(url)$content)

  if (nchar(c) == 0) {
    NULL
  } else {
    try(jsonlite::fromJSON(c), silent = FALSE)
  }
}

#' @noRd
get_nldi_url <- function(tier = "prod") {
  if (tier == "prod") {
    "https://cida.usgs.gov/nldi"
  }
}
