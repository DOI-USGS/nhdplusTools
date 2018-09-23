# NHDPlus Attributes
COMID <- FEATUREID <-
  Hydroseq <- DnHydroseq <- DnMinorHyd <- LevelPathI <- DnLevelPat <-
  ToNode <- FromNode <-
  TotDASqKM <- LENGTHKM <-
  Pathlength <- StreamCalc <- StreamOrde <- TerminalFl <-
  Divergence <- TerminalPa <- StartFlag <- FTYPE <-
  FromMeas <- ToMeas <- REACHCODE <- REACH_meas <- NULL

  # Package Attribute Names
COMID.y <- ID <- becomes <- ds_num_upstream <- fID <-
  dsLENGTHKM <- ds_joined_fromCOMID <- fromCOMID <-
  fromLENGTHKM <- fromTotDASqKM <- geom_len <-
  geometry <- join_category <- joined_fromCOMID <-
  joined_fromCOMID_new <- joined_toCOMID <- member_COMID <-
  new_joined_fromCOMID <- new_joined_toCOMID <- new_toCOMID <-
  num_upstream <- part <- piece <- pieces <- removed_COMID <-
  split_fID <- toCOMID <- toID <- usLENGTHKM <- usTotDASqKM <-
  . <- L1 <- X <- Y <- breaks <- dist_ratio <- ideal_len <-
  len <- nID <- new_index <- piece_len <- setNames <- start <-
  index <- measure <- nn.idx <- precision_index <- max_Hydroseq <-
  nn.dists <- offset <- NULL

nhdplusTools_env <- new.env()

default_nhdplus_path <- "../NHDPlusV21_National_Seamless.gdb"

assign("prepare_nhdplus_attributes",
       c("COMID", "LENGTHKM", "FTYPE", "TerminalFl",
         "FromNode", "ToNode", "TotDASqKM",
         "StartFlag", "StreamOrde", "StreamCalc",
         "TerminalPa", "Pathlength", "Divergence", "Hydroseq"),
       envir = nhdplusTools_env)

assign("split_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "TotDASqKM"),
       envir = nhdplusTools_env)

assign("collapse_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "TotDASqKM"),
       envir = nhdplusTools_env)

assign("reconcile_collapsed_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "TotDASqKM"),
       envir = nhdplusTools_env)

assign("get_UT_attributes",
       c("COMID", "Pathlength", "LENGTHKM", "Hydroseq",
         "LevelPathI", "DnHydroseq"),
       envir = nhdplusTools_env)

assign("get_UM_attributes",
       c("COMID", "Pathlength", "LevelPathI",
         "UpHydroseq", "Hydroseq"),
       envir = nhdplusTools_env)

assign("get_DM_attributes",
       c("COMID", "Pathlength", "LENGTHKM",
         "LevelPathI", "DnLevelPat",
         "DnHydroseq", "Hydroseq"),
       envir = nhdplusTools_env)

assign("get_DD_attributes",
       c("COMID", "Pathlength", "LENGTHKM",
         "LevelPathI", "DnLevelPat",
         "DnHydroseq", "Hydroseq", "DnMinorHyd"),
       envir = nhdplusTools_env)

assign("get_flowline_index_attributes",
       c("COMID", "REACHCODE", "ToMeas", "FromMeas"),
       envir = nhdplusTools_env)


check_names <- function(names_flines, function_name) {
  expect_names <- get(paste0(function_name, "_attributes"),
                      envir = nhdplusTools_env)
  if ( !all(expect_names %in% names_flines)) {
    stop(paste0("Missing some required attributes in call to: ",
                function_name, ". Expected: ",
                paste(expect_names[which(!(expect_names %in%
                                             names_flines))],
                      collapse = ", "), "."))
  }
}

assign("default_nhdplus_path", default_nhdplus_path, envir = nhdplusTools_env)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    "USGS Support Package:
    https://owi.usgs.gov/R/packages.html#support"),
    collapse = "\n"))
  nhdplus_path(default_nhdplus_path, warn = FALSE)
}

#' @title NHDPlus Data Path
#' @description Allows specification of a custom path to a source dataset.
#' Typically this will be the national seamless dataset in
#' geodatabase or geopackage format.
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

    assign("nhdplus_data", path, envir = nhdplusTools_env)

    if (warn) {
      warning("Path does not exist.")
    }

    if (nhdplus_path() == path) {
      invisible(0)
    } else {
      stop("Path not set successfully.")
    }
  } else {
      return(get("nhdplus_data", envir = nhdplusTools_env))
  }
}

#' Sample Flowlines from the Petapsco River.
#'
#' A sample set of flowlines.
#'
#' @format An sf data.frame
#'
#' @source \url{https://www.epa.gov/waterdata/nhdplus-national-data}
"sample_flines"

#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#' @param point An sf POINT including crs as created by:
#' sf::st_sfc(sf::st_point(..,..), crs)
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' discover_nldi_sources() and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @return integer of catchment identifier COMID
#' @export
#' @examples
#' point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
#' discover_nhdplus_id(point)
#'
#' discover_nldi_sources()
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

    check_nldi_feature(nldi_feature)

    if (is.null(nldi_feature[["tier"]])) nldi_feature[["tier"]] <- "prod"

    nldi <- get_nldi_feature(nldi_feature[["featureSource"]],
                             nldi_feature[["featureID"]],
                             nldi_feature[["tier"]])

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
