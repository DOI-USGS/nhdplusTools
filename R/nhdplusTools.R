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

nhdplusTools_env <- new.env()

default_nhdplus_path <- "../NHDPlusV21_National_Seamless.gdb"

assign("default_nhdplus_path", default_nhdplus_path, envir = nhdplusTools_env)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    "This information is preliminary or provisional
    and is subject to revision. It is being provided
    to meet the need for timely best science. The
    information has not received final approval by the
    U.S. Geological Survey (USGS) and is provided on the
    condition that neither the USGS nor the U.S. Government
    shall be held liable for any damages resulting from the
    authorized or unauthorized use of the information.

    USGS Research Package:
    https://owi.usgs.gov/R/packages.html#research"),
    collapse = "\n"))
  nhdplus_path(default_nhdplus_path, warn = FALSE)
}

#' @title NHDPlus Data Path
#' @description Allows specification of a custom path to a source dataset.
#' Typically this will be the national seamless dataset in geodatabase or geopackage format.
#' @param path character path ending in .gdb or .gpkg
#' @param warn boolean controls whether warning an status messages are printed
#' @return 1 if set successfully, the path if no input.
#' @export
#' @examples
#' nhdplus_path("/data/NHDPlusV21_National_Seamless.gdb")
#'
#' nhdplus_path("/data/NHDPlusV21_National_Seamless.gdb", warn=FALSE)
#'
#' nhdplus_path()
#'
nhdplus_path <- function(path = NULL, warn = FALSE) {
  if (!is.null(path)) {

    assign("nhdplus_data_path", path, envir = nhdplusTools_env)

    if (warn) {
      warning("Path does not exist.")
    }

    if (nhdplus_path() == path) {
      return(1)
    } else {
      stop("Path not set successfully.")
    }
  } else {
      return(get("nhdplus_data_path", envir = nhdplusTools_env))
  }
}


#' @title Prepare NHDPlus National Data
#' @description Breaks down the national geo database into a collection
#' of quick to access R binary files.
#' @param include character vector containing one or more of:
#' "attributes", "flowline", "catchment".
#' @param output_path character path to save the output to defaults
#' to the directory of the nhdplus_data_path.
#' @param nhdplus_data_path character path to the .gpkg or .gdb containing the
#' national seamless dataset. Not required if \code{\link{nhdplus_path}} has been set.
#' @details "attributes" will save `NHDFlowline_Network` attributes as a seperate
#' data.frame without the geometry. The others will save the `NHDFlowline_Network`
#' and `CatchmentSP` as sf data.frames with superfluous Z information dropped.
#'
#' The returned list of paths is also added to the nhdplusTools_env as "national_data".
#'
#' @return list containing paths to the .rds files.
#' @export
#' @examples
#' \dontrun{
#' prep_national_data()
#'
#' prep_national_data(include = c("attributes", "flowlines","catchments"))
#' }
#'
prep_national_data <- function(include = c("attribute", "flowline", "catchment"),
                               output_path = NULL, nhdplus_data_path = NULL) {

  if (is.null(output_path)) {
    output_path <- dirname(nhdplus_path())
    warning(paste("No output path provided, using:", output_path))
  }

  if (is.null(nhdplus_data_path)) {
    nhdplus_data_path <- nhdplus_path()

    if (nhdplus_data_path == get("default_nhdplus_path", envir = nhdplusTools_env) &
        !file.exists(nhdplus_data_path)) {
          stop(paste("Didn't find NHDPlus national data in default location:",
                     nhdplus_data_path))
    } else if (!file.exists(nhdplus_data_path)) {
      stop(paste("Didn't find NHDPlus national data in user specified location:",
                 nhdplus_data_path))
    }
  }

  allow_include <- c("attribute", "flowline", "catchment")

  if (!all(include %in% allow_include)) {
    stop(paste0("Got invalid include entries. Expect one or more of: ",
               paste(allow_include, collapse = ", "), "."))
  }

  outlist <- list()

  if (any(c("attribute", "flowline") %in% include)) {

    out_path_attributes <- file.path(output_path, "nhdplus_flowline_attributes.rds")
    out_path_flines <- file.path(output_path, "nhdplus_flowline.rds")

    if (!(file.exists(out_path_flines) | file.exists(out_path_attributes))) {
      fline <- sf::st_zm(sf::read_sf(nhdplus_data_path, "NHDFlowline_Network"))
    }

    if ("attribute" %in% include) {
      if (file.exists(out_path_attributes)) {
        warning("attributes file exists")
      } else {
        saveRDS(sf::st_set_geometry(fline, NULL), out_path_attributes)
      }
      outlist["attributes"] <- out_path_attributes
    }

    if ("flowline" %in% include) {
      if (file.exists(out_path_flines)) {
        warning("flowline file exists")
      } else {
        saveRDS(fline, out_path_flines)
      }
      outlist["flowline"] <- out_path_flines
    }
  }

  if (exists("fline")) rm(fline)

  if ("catchment" %in% include) {
    out_path_catchments <- file.path(output_path, "nhdplus_catchment.rds")
    if (file.exists(out_path_catchments)) {
      warning("catchment already exists.")
    } else {
      saveRDS(sf::st_zm(sf::read_sf(nhdplus_data_path, "CatchmentSP")),
              out_path_catchments)
    }
    outlist["catchment"] <- out_path_catchments
  }
  assign("national_data", outlist, envir = nhdplusTools_env)

  return(outlist)
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
#' @description Multipurpose function to find a COMID of interest.
#' @param point An sf POINT including crs as created by:
#' sf::st_sf(sf::st_point(..,..), crs)
#' @param nldi_feature list with names `featureSource` and `featureID` where
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
