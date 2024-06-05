# Primary HydroShare Data Resource
vaa_hydroshare <-
  'https://www.hydroshare.org/resource/6092c8a62fac45be97a09bfd0b0bf726/data/contents/nhdplusVAA.fst'

vaa_sciencebase <-
  'https://www.sciencebase.gov/catalog/file/get/63cb311ed34e06fef14f40a3?name=enhd_nhdplusatts.fst'

nhdplusTools_env <- new.env()

# NHDPlus Attributes
COMID <- "COMID"
FEATUREID <- "FEATUREID"
Permanent_Identifier <- "Permanent_Identifier"
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
  Permanent_Identifier = Permanent_Identifier,
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

assign("arcrest_root", "https://hydro.nationalmap.gov/arcgis/rest/services/",
       envir = nhdplusTools_env)

assign("gocnx_ref_base_url", "https://reference.geoconnex.us/",
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

nhd_bucket <- "https://prd-tnm.s3.amazonaws.com/"
nhdhr_file_list <- "?prefix=StagedProducts/Hydrography/NHDPlusHR/VPU/Current/GDB/"
nhd_file_list <- "?prefix=StagedProducts/Hydrography/NHD/HU4/GDB/"

assign("nhd_bucket", nhd_bucket, envir = nhdplusTools_env)
assign("nhdhr_file_list", nhdhr_file_list, envir = nhdplusTools_env)

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

    nhdpt_dat_dir <- try(get("nhdpt_dat_dir", envir = nhdplusTools_env), silent = TRUE)

    if(inherits(nhdpt_dat_dir, "try-error") || grepl("CRAN", nhdpt_dat_dir)) {
      assign("nhdpt_dat_dir",
             tools::R_user_dir("nhdplusTools"),
             envir = nhdplusTools_env)
    }

    return(get("nhdpt_dat_dir", envir = nhdplusTools_env))


  } else {
    assign("nhdpt_dat_dir",
           dir,
           envir = nhdplusTools_env)
    return(invisible(get("nhdpt_dat_dir", envir = nhdplusTools_env)))
  }
}

.onAttach <- function(libname, pkgname) {
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

#' @title nhdplusTools cache settings
#' @description
#' Provides an interface to adjust nhdplusTools `memoise` cache.
#'
#' Mode and timeout can also be set using environment variables.
#' `NHDPLUSTOOLS_MEMOISE_CACHE` and `NHDPLUSTOOLS_MEMOISE_TIMEOUT` are
#' used unless overriden with this function.
#'
#' @param mode character 'memory' or 'filesystem'
#' @param timeout numeric number of seconds until caches invalidate
#' @return list containing settings at time of calling. If inputs are
#' NULL, current settings. If settings are altered, previous setting values.
#' @export
#'
nhdplusTools_cache_settings <- function(mode = NULL, timeout = NULL) {
  current_mode <- tryCatch(get("nhdpt_mem_cache", envir = nhdplusTools_env),
                           error = \(e) "filesystem") # default to filesystem
  current_timeout <- tryCatch(get("nhdpt_cache_timeout", envir = nhdplusTools_env),
                              error = \(e) nhdplusTools_memoise_timeout())

  if(!is.null(mode) && mode %in% c("memory", "filesystem")) {
    assign("nhdpt_mem_cache", mode, envir = nhdplusTools_env)
  }

  if(!is.null(timeout) && is.numeric(timeout)) {
    assign("nhdpt_cache_timeout", timeout, envir = nhdplusTools_env)
  }

  return(invisible(list(mode = current_mode, timeout = current_timeout)))
}

#' @importFrom memoise memoise cache_memory cache_filesystem
#' @importFrom digest digest
nhdplusTools_memoise_cache <- function() {
  sys_memo_cache <- Sys.getenv("NHDPLUSTOOLS_MEMOISE_CACHE")
  ses_memo_cache <- try(get("nhdpt_mem_cache", envir = nhdplusTools_env), silent = TRUE)

  # if it hasn't been set up yet, try to use the system env
  if(!inherits(ses_memo_cache, "try-error")) {
    return(ses_memo_cache)
  } else {
    if(sys_memo_cache == "memory") {
      memoise::cache_memory()
    } else {
      dir.create(nhdplusTools_data_dir(), showWarnings = FALSE, recursive = TRUE)

      memoise::cache_filesystem(nhdplusTools_data_dir())
    }

  }

}

nhdplusTools_memoise_timeout <- function() {
  sys_timeout <- Sys.getenv("NHDPLUSTOOLS_MEMOISE_TIMEOUT")
  ses_timeout <- try(get("nhdpt_cache_timeout", envir = nhdplusTools_env), silent = TRUE)

  # if it hasn't been set up yet, try to use the system env
  if(!inherits(ses_timeout, "try-error")) {
    return(ses_timeout)
  } else {
    if(sys_timeout != "") {
      as.numeric(sys_timeout)
    } else {
      # default to one day
      oneday_seconds <- 60 * 60 * 24
    }
  }
}

.onLoad <- function(libname, pkgname) {
  query_usgs_arcrest <<- memoise::memoise(query_usgs_arcrest,
                                          ~memoise::timeout(nhdplusTools_memoise_timeout()),
                                          cache = nhdplusTools_memoise_cache())
  query_usgs_geoserver <<- memoise::memoise(query_usgs_geoserver,
                                            ~memoise::timeout(nhdplusTools_memoise_timeout()),
                                            cache = nhdplusTools_memoise_cache())
  query_nldi <<- memoise::memoise(query_nldi,
                                  ~memoise::timeout(nhdplusTools_memoise_timeout()),
                                  cache = nhdplusTools_memoise_cache())
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

#' @importFrom hydroloom st_compatibalize
#' @export
hydroloom::st_compatibalize

#' @importFrom hydroloom rename_geometry
#' @export
hydroloom::rename_geometry

#' @importFrom hydroloom get_node
#' @export
hydroloom::get_node

#' @importFrom hydroloom fix_flowdir
#' @export
hydroloom::fix_flowdir

#' @importFrom hydroloom rescale_measures
#' @export
hydroloom::rescale_measures

#' @importFrom hydroloom get_hydro_location
#' @export
hydroloom::get_hydro_location

#' @importFrom hydroloom get_partial_length
#' @export
hydroloom::get_partial_length
