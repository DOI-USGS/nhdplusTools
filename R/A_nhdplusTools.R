# Primary HydroShare Data Resource
vaa_hydroshare <-
  'https://www.hydroshare.org/resource/6092c8a62fac45be97a09bfd0b0bf726/data/contents/nhdplusVAA.fst'

vaa_sciencebase <-
  'https://www.sciencebase.gov/catalog/file/get/60c92503d34e86b9389df1c9?name=enhd_nhdplusatts.fst'

nhdplusTools_env <- new.env()

# NHDPlus Attributes
COMID <- "COMID"
FEATUREID <- "FEATUREID"
Hydroseq <- "Hydroseq"
UpHydroseq <- "UpHydroseq"
DnHydroseq <- "DnHydroseq"
DnMinorHyd <- "DnMinorHyd"
UpLevelPat <- "UpLevelPat"
LevelPathI <- "LevelPathI"
LevelPathID <- "LevelPathID"
DnLevelPat <- "DnLevelPat"
DnLevel <- "DnLevel"
ToNode <- "ToNode"
FromNode <- "FromNode"
TotDASqKM <- "TotDASqKM"
AreaSqKM <- "AreaSqKM"
LENGTHKM <- "LENGTHKM"
Pathlength <- "Pathlength"
ArbolateSu <- "ArbolateSu"
StreamCalc <- "StreamCalc"
StreamOrde <- "StreamOrde"
TerminalFl <- "TerminalFl"
Divergence <- "Divergence"
TerminalPa <- "TerminalPa"
StartFlag <- "StartFlag"
FTYPE <- "FTYPE"
FCODE <- "FCODE"
FromMeas <- "FromMeas"
ToMeas <- "ToMeas"
REACHCODE <- "REACHCODE"
REACH_meas <- "REACH_meas"
HUC12 <- "HUC12"
TOHUC <- "TOHUC"
ReachCode <- "ReachCode"
VPUID <- "VPUID"
RPUID <- "RPUID"
toCOMID <- "toCOMID"
WBAREACOMI <- "WBAREACOMI"


# List of input names that should be changed to replacement names
nhdplus_attributes <- list(
  COMID = COMID, NHDPlusID = COMID,
  RPUID = RPUID,
  VPUID = VPUID,
  FEATUREID = FEATUREID,
  WBAREACOMI = WBAREACOMI,
  Hydroseq = Hydroseq, HydroSeq = Hydroseq,
  UpHydroseq = UpHydroseq, UpHydroSeq = UpHydroseq,
  DnHydroseq = DnHydroseq, DnHydroSeq = DnHydroseq,
  DnMinorHyd = DnMinorHyd,
  LevelPathI = LevelPathI,
  LevelPathID = LevelPathID,
  UpLevelPat = UpLevelPat,
  DnLevelPat = DnLevelPat,
  DnLevel = DnLevel,
  ToNode = ToNode,
  FromNode = FromNode,
  TotDASqKM = TotDASqKM, TotDASqKm = TotDASqKM,
  AreaSqKM = AreaSqKM, AreaSqKm = AreaSqKM,
  LENGTHKM = LENGTHKM, LengthKM = LENGTHKM,
  ArbolateSu = ArbolateSu,
  Pathlength = Pathlength, PathLength = Pathlength,
  StreamCalc = StreamCalc,
  StreamOrde = StreamOrde,
  TerminalFl = TerminalFl,
  Divergence = Divergence,
  TerminalPa = TerminalPa,
  StartFlag = StartFlag,
  FTYPE = FTYPE, FType = FTYPE,
  FCODE = FCODE, FCode = FCODE,
  FromMeas = FromMeas,
  ToMeas = ToMeas,
  REACHCODE = REACHCODE, ReachCode = REACHCODE,
  REACH_meas = REACH_meas,
  HUC12 = HUC12,
  TOHUC = TOHUC,
  toCOMID = toCOMID)

.data <- . <- NULL

assign("nhdplus_attributes", nhdplus_attributes, envir = nhdplusTools_env)

assign("geoserver_root", "https://labs.waterdata.usgs.gov/geoserver/",
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
         "Hydroseq"),
       envir = nhdplusTools_env)

assign("get_DM_attributes",
       c("COMID", "Pathlength", "LENGTHKM",
         "LevelPathI", "DnLevelPat",
         "DnHydroseq", "Hydroseq"),
       envir = nhdplusTools_env)

assign("get_DM_nolength_attributes",
       c("COMID",
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

assign("get_levelpaths_attributes",
       c("ID", "toID", "nameID", "weight"),
       envir = nhdplusTools_env)

assign("get_streamorder_attributes",
       c("ID", "toID"),
       envir = nhdplusTools_env)

assign("get_streamlevel_attributes",
       c("LevelPathI", "DnLevelPat"),
       envir = nhdplusTools_env)

assign("get_pfaf_attributes",
       c("ID", "toID", "totda", "outletID", "topo_sort", "levelpath"),
       envir = nhdplusTools_env)

assign("make_standalone_tonode_attributes",
       c("COMID", "ToNode", "FromNode", "TerminalFl", "Hydroseq", "TerminalPa",
         "LevelPathI", "FCODE"),
       envir = nhdplusTools_env)

assign("make_standalone_tocomid_attributes",
       c("COMID", "toCOMID", "Hydroseq", "TerminalPa",
         "LevelPathI", "FCODE"), envir = nhdplusTools_env)

assign("get_waterbody_index_waterbodies_attributes",
       c("COMID"), envir = nhdplusTools_env)

assign("get_waterbody_index_flines_attributes",
       c("COMID", "WBAREACOMI", "Hydroseq"),
       envir = nhdplusTools_env)

assign("disambiguate_flowline_indexes_attributes",
       c("id", "COMID", "REACHCODE", "REACH_meas", "offset"),
       envir = nhdplusTools_env)

assign("add_plus_network_attributes_attributes",
       c("comid", "tocomid", "nameID", "lengthkm"),
       envir = nhdplusTools_env)

assign("subset_rpu_attributes",
       c("COMID", "Pathlength", "LENGTHKM",
         "Hydroseq", "LevelPathI", "DnLevelPat",
         "RPUID", "ArbolateSu", "TerminalPa"),
       envir = nhdplusTools_env)

assign("subset_vpu_attributes",
       c(get("subset_rpu_attributes",
             envir = nhdplusTools_env),
         "VPUID"),
       envir = nhdplusTools_env)

assign("fix_flowdir_attributes",
       c("COMID", "toCOMID"),
       envir = nhdplusTools_env)

assign("get_hydro_location_attributes",
       c("COMID", "ToMeas", "FromMeas"),
       envir = nhdplusTools_env)

assign("get_wb_outlet_mres_attributes",
       c("COMID", "Hydroseq", "WBAREACOMI"),
       envir = nhdplusTools_env)

assign("get_wb_outlet_hires_attributes",
       c("WBArea_Permanent_Identifier", "Hydroseq"),
       envir = nhdplusTools_env)

assign("on_off_network_attributes",
       c("COMID", "WBAREACOMI"),
       envir = nhdplusTools_env)

assign("get_tocomid_attributes",
       c("COMID", "ToNode", "FromNode"),
       envir = nhdplusTools_env)

assign("get_partial_length_attributes",
       c("REACHCODE", "FromNode", "ToNode", "LENGTHKM"),
       envir = nhdplusTools_env)

# assigned here for record keeping. Used as a status counter in apply functions.
assign("cur_count", 0, envir = nhdplusTools_env)

check_names <- function(x, function_name, align = TRUE, tolower = FALSE) {
  if(align) {
    x <- align_nhdplus_names(x)
  }
  names_x <- names(x)
  expect_names <- get(paste0(function_name, "_attributes"),
                      envir = nhdplusTools_env)
  if ( !all(expect_names %in% names_x)) {
    stop(paste0("Missing some required attributes in call to: ",
                function_name, ". Expected: ",
                paste(expect_names[which(!(expect_names %in%
                                             names_x))],
                      collapse = ", "), " or NHDPlusHR equivalents."))
  }

  if(tolower) {
    names(x) <- tolower(names(x))
    if(inherits(x, "sf")) {
      attr(x, "sf_column") <- tolower(attr(x, "sf_column"))
    }
  }

  return(x)
}

default_nhdplus_path <- "../NHDPlusV21_National_Seamless.gdb"

assign("default_nhdplus_path", default_nhdplus_path, envir = nhdplusTools_env)

nhdhr_bucket <- "https://prd-tnm.s3.amazonaws.com/"
nhdhr_file_list <- "?prefix=StagedProducts/Hydrography/NHDPlusHR/Beta/GDB/"

assign("nhdhr_bucket", nhdhr_bucket, envir = nhdplusTools_env)
assign("nhdhr_file_list", nhdhr_file_list, envir = nhdplusTools_env)

assign("nhdpt_dat_dir",
       tools::R_user_dir("nhdplusTools"),
       envir = nhdplusTools_env)

assign("nldi_tier", "prod",
       envir = nhdplusTools_env)

nhdplus_debug <- function() {
  Sys.getenv("debug_nhdplusTools") == "true"
}

#' @noRd
get_nldi_url <- function() {

  tier <- get("nldi_tier", envir = nhdplusTools_env)

  if (tier == "prod") {
    "https://labs.waterdata.usgs.gov/api/nldi"
  } else if (tier == "test") {
    "https://labs-beta.waterdata.usgs.gov/api/nldi"
  } else {
    stop("only prod or test allowed.")
  }
}

#' Get or set nhdplusTools data directory
#' @description if left unset, will return the user data dir
#' as returned by `tools::R_user_dir` for this package.
#' @param dir path of desired data directory
#' @return character path of data directory (silent when setting)
#' @importFrom tools R_user_dir
#' @export
#' @examples
#' nhdplusTools_data_dir()
#'
#' nhdplusTools_data_dir("demo")
#'
#' nhdplusTools_data_dir(tools::R_user_dir("nhdplusTools"))
#'
nhdplusTools_data_dir <- function(dir = NULL) {

  if(is.null(dir)) {
    return(get("nhdpt_dat_dir", envir = nhdplusTools_env))
  } else {
    assign("nhdpt_dat_dir",
           dir,
           envir = nhdplusTools_env)
    return(invisible(get("nhdpt_dat_dir", envir = nhdplusTools_env)))
  }
}

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
#' @return 0 (invisibly) if set successfully, character path if no input.
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


#' @title Align NHD Dataset Names
#' @description this function takes any NHDPlus dataset and aligns the attribute names with those used in nhdplusTools.
#' @param x a \code{sf} object of nhdplus flowlines
#' @return data.frame renamed \code{sf} object
#' @export
#' @examples
#' source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#'
#' names(new_hope_flowline)
#'
#' names(new_hope_flowline) <- tolower(names(new_hope_flowline))
#'
#' new_hope_flowline <- align_nhdplus_names(new_hope_flowline)
#'
#' names(new_hope_flowline)
#'
align_nhdplus_names <- function(x){

  attribute_names <- get("nhdplus_attributes", envir = nhdplusTools_env)

  # get into correct case
  good_names <- unique(unlist(do.call(rbind, attribute_names))[,1])

  new_names <- old_names <- names(x)

  matched <- match(toupper(names(x)), toupper(good_names))
  replacement_names <- as.character(good_names[matched[which(!is.na(matched))]])

  new_names[which(toupper(old_names) %in% toupper(good_names))] <- replacement_names
  names(x) <- new_names

  # rename to match package
  new_names <- old_names <- names(x)

  matched <- match(names(x), names(attribute_names))
  replacement_names <- as.character(attribute_names[matched[which(!is.na(matched))]])

  new_names[which(old_names %in% names(attribute_names))] <- replacement_names

  names(x) <- new_names

  if("GridCode" %in% names(x) & !"FeatureID" %in% names(x) & !"FEATUREID" %in% names(x))
    names(x)[which(names(x) == "COMID")] <- "FEATUREID"

  return(x)

}


drop_geometry <- function(x) {
  if("sf" %in% class(x)) {
    sf::st_drop_geometry(x)
  } else {
    x
  }
}

#' make spatial inputs compatible
#' @description makes sf1 compatible with sf2 by projecting into
#' the projection of 2 and ensuring that the geometry columns are the
#' same name.
#' @param sf1 sf data.frame
#' @param sf2 sf data.frame
#' @export
#' @examples
#'
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' (one <- dplyr::select(sample_flines))
#' (two <- sf::st_transform(one, 5070))
#'
#' attr(one, "sf_column") <- "geotest"
#' names(one)[names(one) == "geom"] <- "geotest"
#'
#' st_compatibalize(one, two)
#'
st_compatibalize <- function(sf1, sf2) {

  sf1 <- st_transform(sf1, st_crs(sf2))

  rename_geometry(sf1, attr(sf2, "sf_column"))

}

#' rename_geometry
#' @description correctly renames the geometry column
#' of a sf object.
#' @param g sf data.table
#' @param name character name to be used for geometry
#' @export
#' @examples
#'
#' (g <- sf::st_sf(a=3, geo = sf::st_sfc(sf::st_point(1:2))))
#' rename_geometry(g, "geometry")
#'
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")

  names(g)[names(g)==current] = name

  attr(g, "sf_column") <- name

  g
}

get_cl <- function(cl) {
  if(!is.null(cl)) {
    if(!requireNamespace("parallel", quietly = TRUE)) {
      stop("parallel required if using cores input")
    }
    if(is.numeric(cl)) {
      cl <- parallel::makeCluster(cl)
    } else {
      if(!"cluster" %in% class(cl)) {
        stop("cores must be numeric or a cluster object")
      }
    }
  }
  return(cl)
}
