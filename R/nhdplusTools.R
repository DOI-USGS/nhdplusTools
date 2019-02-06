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
  fromTotDASqKM <- geom_len <-
  geometry <- join_category <- joined_fromCOMID <-
  joined_fromCOMID_new <- joined_toCOMID <- member_COMID <-
  new_joined_fromCOMID <- new_joined_toCOMID <- new_toCOMID <-
  num_upstream <- part <- piece <- pieces <- removed_COMID <-
  split_fID <- toCOMID <- toID <- usTotDASqKM <-
  . <- L1 <- X <- Y <- breaks <- dist_ratio <- ideal_len <-
  len <- nID <- new_index <- piece_len <- setNames <- start <-
  index <- measure <- nn.idx <- precision_index <- max_Hydroseq <-
  nn.dists <- offset <- area <- member_FEATUREID <- geom <-
  fromID <- nexID <- cat_ID <- type <- LevelPathID <- orig_COMID <-
  tail_ID <- toID_hydroseq <- toID_tail_ID <- toID_fromID <-
  toID_LevelpathID <- set <- set_toID <- usLevelPathI <- fromLevelPathI <-
  ID_Hydroseq <- ID_LevelPath <- ID_LevelPathID <- toID_fromID_TotDASqKM <-
  toID_fromID_lp <- NULL

nhdplusTools_env <- new.env()

default_nhdplus_path <- "../NHDPlusV21_National_Seamless.gdb"

assign("prepare_nhdplus_attributes",
       c("COMID", "LENGTHKM", "FTYPE", "TerminalFl",
         "FromNode", "ToNode", "TotDASqKM",
         "StartFlag", "StreamOrde", "StreamCalc",
         "TerminalPa", "Pathlength", "Divergence", "Hydroseq",
         "LevelPathI"),
       envir = nhdplusTools_env)

assign("split_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM"),
       envir = nhdplusTools_env)

assign("collapse_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "LevelPathI", "Hydroseq"),
       envir = nhdplusTools_env)

assign("reconcile_collapsed_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "LevelPathI", "Hydroseq"),
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

assign("calculate_levelpaths_attributes",
       c("ID", "toID", "nameID", "weight"),
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
    }
  } else {
      return(get("nhdplus_data", envir = nhdplusTools_env))
  }
}

#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#' @param point An sf POINT including crs as created by:
#' sf::st_sfc(sf::st_point(..,..), crs)
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' discover_nldi_sources() and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @return integer COMID
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

#' Total Drainage Area
#' @description Calculates total drainage area given a dendritic
#' network and incremental areas.
#' @param catchment_area data.frame with ID, toID, and area columns.
#' @return numeric with total area.
#' @importFrom igraph graph_from_data_frame topo_sort
#' @importFrom dplyr select left_join
#' @export
#' @examples
#' library(dplyr)
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' catchment_area <- prepare_nhdplus(walker_flowline, 0, 0,
#'                              purge_non_dendritic = FALSE, warn = FALSE) %>%
#'   left_join(select(walker_flowline, COMID, AreaSqKM), by = "COMID") %>%
#'   select(ID = COMID, toID = toCOMID, area = AreaSqKM)
#'
#' new_da <- calculate_total_drainage_area(catchment_area)
#'
#' catchment_area$totda <- new_da
#' catchment_area$nhdptotda <- walker_flowline$TotDASqKM
#'
#' mean(abs(catchment_area$totda - catchment_area$nhdptotda))
#' max(abs(catchment_area$totda - catchment_area$nhdptotda))
#'

calculate_total_drainage_area <- function(catchment_area) {

  return(accumulate_downstream(catchment_area, "area"))

}

#' Calculate Arbolate Sum
#' @description Calculates arbolate sum given a dendritic
#' network and incremental lengths. Arbolate sum is the total length
#' of all upstream flowlines.
#' @param catchment_area data.frame with ID, toID, and length columns.
#' @return numeric with arbolate sum.
#' @export
#' @examples
#' library(dplyr)
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' catchment_length <- prepare_nhdplus(walker_flowline, 0, 0,
#'                              purge_non_dendritic = FALSE, warn = FALSE) %>%
#'   left_join(select(walker_flowline, COMID), by = "COMID") %>%
#'   select(ID = COMID, toID = toCOMID, length = LENGTHKM)
#'
#' arb_sum <- calculate_arbolate_sum(catchment_length)
#'
#' catchment_length$arb_sum <- arb_sum
#' catchment_length$nhd_arb_sum <- walker_flowline$ArbolateSu
#'
#' mean(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum))
#' max(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum))
#'

calculate_arbolate_sum <- function(catchment_area) {

  return(accumulate_downstream(catchment_area, "length"))

}

#' @importFrom igraph graph_from_data_frame topo_sort
#' @importFrom dplyr select left_join
#' @noRd
#'
accumulate_downstream <- function(dat_fram, var) {

  cat_order <- select(dat_fram, ID)

  dat_fram[["toID"]][which(is.na(dat_fram[["toID"]]))] <- 0

  sorted <- names(topo_sort(graph_from_data_frame(dat_fram,
                                                  directed = TRUE),
                            mode = "out"))

  sorted <- sorted[sorted != "0" & sorted %in% as.character(cat_order$ID)]

  dat_fram <- left_join(data.frame(ID = as.integer(sorted[!sorted == "NA"])),
                              dat_fram, by = "ID")

  dat_fram[["toID_row"]] <- match(dat_fram[["toID"]], dat_fram[["ID"]])

  var_out <- dat_fram[[var]]
  toid_row <- dat_fram[["toID_row"]]

  for(cat in 1:length(var_out)) {
    var_out[toid_row[cat]] <- var_out[toid_row[cat]] + var_out[cat]
  }

  dat_fram[[var]] <- var_out

  dat_fram <- left_join(cat_order, dat_fram, by = "ID")

  return(dat_fram[[var]])
}

#' Calculate Level Paths
#' @description Calculates level paths using the stream-leveling approach of
#' NHD and NHDPlus. In addition to a levelpath identifier, a topological sort and
#' levelpath outlet identifier is provided in output. If arbolate sum is provided in
#' the weight column, this will match the behavior of NHDPlus. Any numeric value can be
#' included in this column and the largest value will be followed when no nameID is available.
#' @param flowline data.frame with ID, toID, nameID, and weight columns.
#' @return data.frame with ID, outletID, topo_sort, and levelpath collumns.
#' See details for more info.
#' @details
#' \enumerate{
#'   \item levelpath provides an identifier for the collection of flowlines
#'   that make up the single mainstem flowpath of a total upstream aggregate catchment.
#'   \item outletID is the catchment ID (COMID in the case of NHDPlus) for the catchment
#'   at the outlet of the levelpath the catchment is part of.
#'   \item topo_sort is similar to Hydroseq in NHDPlus in that large topo_sort values
#'   are upstream of small topo_sort values. Note that there are many valid topological
#'   sort orders of a directed graph. The sort order output by this function is generated
#'   using \code{\link{igraph::topo_sort}}.
#' }
#' @export
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE)
#'
#' test_flowline <- data.frame(
#'   ID = test_flowline$COMID,
#'   toID = test_flowline$toCOMID,
#'   nameID = walker_flowline$GNIS_ID,
#'   weight = walker_flowline$ArbolateSu,
#'   stringsAsFactors = FALSE)
#'
#' calculate_levelpaths(test_flowline)
#'
#'
calculate_levelpaths <- function(flowline) {

  check_names(names(flowline), "calculate_levelpaths")

  flowline[["toID"]][which(is.na(flowline[["toID"]]))] <- 0

  sorted <- names(topo_sort(graph_from_data_frame(flowline,
                                                  directed = TRUE),
                            mode = "out"))

  sorted <- sorted[sorted != 0]

  flowline <- left_join(data.frame(ID = as.integer(sorted[!sorted == "NA"])),
                        flowline, by = "ID")

  flowline[["topo_sort"]] <- seq(nrow(flowline), 1)
  flowline[["levelpath"]] <- rep(0, nrow(flowline))

  get_path <- function(flowline, tailID) {
    from_inds <- which(flowline$toID == tailID)
    if(length(from_inds) > 1) {
      ind <- which(flowline$ID == tailID)
      next_step <- dplyr::filter(flowline[from_inds, ],
                                 (nameID == flowline$nameID[ind] & nameID != " ") |
                                   weight == max(weight))$ID
      c(tailID, get_path(flowline, next_step))
    } else if(length(from_inds) == 1) {
      c(tailID, get_path(flowline, flowline$ID[from_inds]))
    } else {
      return(tailID)
    }
  }

  flc <- flowline
  diff = 1

  while(nrow(flc) > 0) {
    tail_ind <- which(flc$topo_sort == min(flc$topo_sort))
    tailID <- flc$ID[tail_ind]
    sortID <- flowline$topo_sort[tail_ind]

    pathIDs <- get_path(flc, tailID)

    flowline <- mutate(flowline, levelpath = ifelse(flowline$ID %in% pathIDs, sortID, levelpath))
    flc <- filter(flc, !ID %in% pathIDs)
  }

  outlets <- flowline %>%
    group_by(levelpath) %>%
    filter(topo_sort == min(topo_sort)) %>%
    ungroup() %>%
    select(outletID = ID, levelpath)

  flowline <- left_join(flowline, outlets, by = "levelpath")

  return(select(flowline, ID, outletID, topo_sort, levelpath))
}
