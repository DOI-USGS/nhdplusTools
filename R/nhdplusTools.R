nhdplusTools_env <- new.env()

# NHDPlus Attributes
COMID <- "COMID"
FEATUREID <- "FEATUREID"
Hydroseq <- "Hydroseq"
DnHydroseq <- "DnHydroseq"
DnMinorHyd <- "DnMinorHyd"
LevelPathI <- "LevelPathI"
DnLevelPat <- "DnLevelPat"
ToNode <- "ToNode"
FromNode <- "FromNode"
TotDASqKM <- "TotDASqKM"
AreaSqKM <- "AreaSqKM"
LENGTHKM <- "LENGTHKM"
Pathlength <- "Pathlength"
StreamCalc <- "StreamCalc"
StreamOrde <- "StreamOrde"
TerminalFl <- "TerminalFl"
Divergence <- "Divergence"
TerminalPa <- "TerminalPa"
StartFlag <- "StartFlag"
FTYPE <- "FTYPE"
FromMeas <- "FromMeas"
ToMeas <- "ToMeas"
REACHCODE <- "REACHCODE"
REACH_meas <- "REACH_meas"
HUC12 <- "HUC12"
TOHUC <- "TOHUC"
ReachCode <- "ReachCode"
VPUID <- "VPUID"


# List of input names that should be changed to replacement names
nhdplus_attributes <- list(
  COMID = COMID, NHDPlusID = COMID,
  FEATUREID = FEATUREID,
  Hydroseq = Hydroseq, HydroSeq = Hydroseq,
  DnHydroseq = DnHydroseq, DnHydroSeq = DnHydroseq,
  DnMinorHyd = DnMinorHyd,
  LevelPathI = LevelPathI,
  DnLevelPat = DnLevelPat,
  ToNode = ToNode,
  FromNode = FromNode,
  TotDASqKM = TotDASqKM, TotDASqKm = TotDASqKM,
  AreaSqKM = AreaSqKM, AreaSqKm = AreaSqKM,
  LENGTHKM = LENGTHKM, LengthKM = LENGTHKM,
  Pathlength = Pathlength, PathLength = Pathlength,
  StreamCalc = StreamCalc,
  StreamOrde = StreamOrde,
  TerminalFl = TerminalFl,
  Divergence = Divergence,
  TerminalPa = TerminalPa,
  StartFlag = StartFlag,
  FTYPE = FTYPE, FType = FTYPE,
  FromMeas = FromMeas,
  ToMeas = ToMeas,
  REACHCODE = REACHCODE, ReachCode = REACHCODE,
  REACH_meas = REACH_meas,
  HUC12 = HUC12,
  TOHUC = TOHUC)

.data <- . <- NULL

assign("nhdplus_attributes", nhdplus_attributes, envir = nhdplusTools_env)

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

assign("calculate_levelpaths_attributes",
       c("ID", "toID", "nameID", "weight"),
       envir = nhdplusTools_env)

check_names <- function(x, function_name) {
  x <- rename_nhdplus(x)
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
  return(x)
}

#' @noRd
rename_nhdplus <- function(x) {
  attribute_names <- get("nhdplus_attributes", envir = nhdplusTools_env)

  old_names <- names(x)
  new_names <- old_names

  matched <- match(names(x), names(attribute_names))
  replacement_names <- as.character(attribute_names[matched[which(!is.na(matched))]])

  new_names[which(old_names %in% names(attribute_names))] <- replacement_names

  names(x) <- new_names

  if("GridCode" %in% names(x)) names(x)[which(names(x) == "COMID")] <- "FEATUREID"

  return(x)
}

default_nhdplus_path <- "../NHDPlusV21_National_Seamless.gdb"

assign("default_nhdplus_path", default_nhdplus_path, envir = nhdplusTools_env)

nhdhr_bucket <- "https://prd-tnm.s3.amazonaws.com/"
nhdhr_file_list <- "?prefix=StagedProducts/Hydrography/NHDPlus/HU4/HighResolution/GDB/"

assign("nhdhr_bucket", nhdhr_bucket, envir = nhdplusTools_env)
assign("nhdhr_file_list", nhdhr_file_list, envir = nhdplusTools_env)

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

#' @noRd
#' These are the names that come from the packaged data: "petapsco_flowlines.gpkg"
#' and thus the assumed names all nhdplusTools functions work on:

good_names = c("COMID", "FDATE", "RESOLUTION",
               "GNIS_ID", "GNIS_NAME", "LENGTHKM",
               "REACHCODE", "FLOWDIR", "WBAREACOMI",
               "FTYPE", "FCODE", "Shape_Length",
               "StreamLeve", "StreamOrde", "StreamCalc",
               "FromNode", "ToNode", "Hydroseq",
               "LevelPathI", "Pathlength", "TerminalPa",
               "ArbolateSu", "Divergence", "StartFlag",
               "TerminalFl", "DnLevel", "UpLevelPat",
               "UpHydroseq", "DnLevelPat", "DnMinorHyd",
               "DnDrainCou", "DnHydroseq", "FromMeas",
               "ToMeas", "RtnDiv", "VPUIn",
               "VPUOut", "AreaSqKM", "TotDASqKM",
               "DivDASqKM", "Tidal", "TOTMA",
               "WBAreaType", "HWNodeSqKM", "MAXELEVRAW",
               "MINELEVRAW", "MAXELEVSMO", "MINELEVSMO",
               "SLOPE", "ELEVFIXED", "HWTYPE",
               "SLOPELENKM",
               "QA_MA", "VA_MA", "QC_MA", "VC_MA", "QE_MA", "VE_MA",
               "QA_01", "VA_01", "QC_01", "VC_01", "QE_01", "VE_01",
               "QA_02", "VA_02", "QC_02", "VC_02", "QE_02", "VE_02",
               "QA_03", "VA_03", "QC_03", "VC_03", "QE_03", "VE_03",
               "QA_04", "VA_04", "QC_04", "VC_04", "QE_04", "VE_04",
               "QA_05", "VA_05", "QC_05", "VC_05", "QE_05", "VE_05",
               "QA_06", "VA_06", "QC_06", "VC_06", "QE_06", "VE_06",
               "QA_07", "VA_07", "QC_07", "VC_07", "QE_07", "VE_07",
               "QA_08", "VA_08", "QC_08", "VC_08", "QE_08", "VE_08",
               "QA_09", "VA_09", "QC_09", "VC_09", "QE_09", "VE_09",
               "QA_10", "VA_10", "QC_10", "VC_10", "QE_10", "VE_10",
               "QA_11", "VA_11", "QC_11", "VC_11", "QE_11", "VE_11",
               "QA_12", "VA_12", "QC_12", "VC_12", "QE_12", "VE_12",
               "LakeFract", "SurfArea", "RAreaHLoad",
               "RPUID", "VPUID", "Enabled",
               "geom")


#' @title Align NHD Dataset Names
#' @description this function takes any NHDPlus dataset and aligns the attribute names with those used in nhdplusTools.
#' @param x a \code{sf} object of nhdplus flowlines
#' @return a renamed \code{sf} object
#' @export
#' @examples
#' \dontrun{
#' a = AOI::getAOI(list("UCSB", 1, 1))
#' n = HydroData::findNHD(a)[[2]] %>% align_nhdplus_names()
#' UM_comids = get_UM(n, n$COMID[3])
#' }

align_nhdplus_names = function(x){

  old_names <- names(x)
  new_names <- old_names

  matched <- match(toupper(names(x)), toupper(good_names))

  replacement_names <- as.character(good_names[matched[which(!is.na(matched))]])

  new_names[which(toupper(old_names) %in% toupper(good_names))] <- replacement_names
  names(x) <- new_names
  return(x)

}





