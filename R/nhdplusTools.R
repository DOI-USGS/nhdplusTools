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
