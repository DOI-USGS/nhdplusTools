#' Download NHDPlus HiRes
#' @param nhd_dir character directory to save output into
#' @param hu_list character vector of hydrologic region(s) to download
#' @param download_files boolean if FALSE, only URLs to files will be returned
#' can be hu02s and/or hu04s
#'
#' @return Paths to geodatabases created.
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_text
#' @importFrom utils download.file unzip
#' @export
#' @examples
#' \donttest{
#' download_nhdplushr(tempdir(), c("01", "0203"), download_files = FALSE)
#' }
download_nhdplushr <- function(nhd_dir, hu_list, download_files = TRUE) {

  nhdhr_bucket <- get("nhdhr_bucket", envir = nhdplusTools_env)
  nhdhr_file_list <- get("nhdhr_file_list", envir = nhdplusTools_env)

  hu02_list <- unique(substr(hu_list, 1, 2))
  hu04_list <- hu_list[which(nchar(hu_list) == 4)]
  subset_hu02 <- sapply(hu02_list, function(x)
    sapply(x, function(y) any(grepl(y, hu04_list))))

  out <- c()

  for(h in 1:length(hu02_list)) {
    hu02 <- hu02_list[h]

    if(download_files) {
      out <- c(out, file.path(nhd_dir, hu02))
    }

    if(download_files) {
      dir.create(out[length(out)], recursive = TRUE, showWarnings = FALSE)
    }

    file_list <- read_xml(paste0(nhdhr_bucket, nhdhr_file_list,
                                 "NHDPLUS_H_", hu02)) %>%
      xml_ns_strip() %>%
      xml_find_all(xpath = "//Key") %>%
      xml_text()

    file_list <- file_list[grepl("_GDB.zip", file_list)]

    if(subset_hu02[h]) {
      file_list <- file_list[sapply(file_list, function(f)
        any(sapply(hu04_list, grepl, x = f)))]
    }

    for(key in file_list) {
      out_file <- paste0(out[length(out)], "/", tail(strsplit(key, "/")[[1]], 1))
      url <- paste0(nhdhr_bucket, key)

      hu04 <- regexec("[0-9][0-9][0-9][0-9]", out_file)[[1]]
      hu04 <- substr(out_file, hu04, hu04 + 3)

      if(download_files & !dir.exists(gsub(".zip", ".gdb", out_file)) &
         !dir.exists(file.path(dirname(out_file), paste0(hu04, ".gdb")))) {
        download.file(url, out_file)
        unzip(out_file, exdir = out[length(out)])
        unlink(out_file)
      } else if(!download_files) {
        out <- c(out, url)
      }
    }
  }
  return(out)
}

#' Get NHDPlus HiRes
#' @param hr_dir character directory with geodatabases (gdb search is recursive)
#' @param out_gpkg character path to write output geopackage
#' @param layers character vector with desired layers to return.
#' c("NHDFlowline", "NHDPlusCatchment") is default.
#' Choose from:
#' c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody", "NHDArea", "NHDLine",
#' "NHDPlusSink", "NHDPlusWall", "NHDPoint", "NHDPlusBurnWaterbody",
#' "NHDPlusBurnLineEvent", "HYDRO_NET_Junctions",
#' "WBDHU2", "WBDHU4","WBDHU6", "WBDHU8" "WBDHU10", "WBDHU12", "WBDLine")
#' @param pattern character optional regex to select certain files in hr_dir
#' @param check_terminals boolean if TRUE, run \link{clean_hr_region} on output.
#' @param overwrite boolean should the output overwrite? If false and the output layer
#' exists, it will be read and returned so this function will always return data even
#' if called a second time for the same output. This is useful for workflows.
#' @param ... parameters passed along to \link{get_hr_data}
#' for "NHDFlowline" layers.
#' @return Response is a list of sf data.frames containing output that may also be written
#' to a geopackage for later use.
#' @details
#' NHDFlowline is joined to value added attributes prior to being
#' returned.
#' Names are not modified from the NHDPlusHR geodatabase.
#' Set layers to "NULL" to get all layers.
#'
#' @importFrom sf st_layers read_sf st_sf write_sf
#' @export
#' @examples
#' \donttest{
#' # Note this will download a lot of data to a temp directory.
#' # Change 'tempdir()' to your directory of choice.
#' download_dir <- download_nhdplushr(tempdir(), c("0302", "0303"))
#' get_nhdplushr(download_dir, file.path(download_dir, "nhdplus_0302-03.gpkg"))
#' }
get_nhdplushr <- function(hr_dir, out_gpkg = NULL,
                          layers = c("NHDFlowline", "NHDPlusCatchment"),
                          pattern = ".*GDB.gdb$", check_terminals = TRUE,
                          overwrite = FALSE, ...) {

  gdbs <- do.call(c, as.list(sapply(hr_dir, list.files,
                                    pattern = pattern,
                                    full.names = TRUE, recursive = TRUE,
                                    include.dirs = TRUE, USE.NAMES = FALSE)))

  if(length(gdbs) == 0) {
    # For testing.
    gdbs <- list.files(hr_dir, pattern = "sub.gpkg", full.names = TRUE)
  }

  if(is.null(layers)) {
    layers <- st_layers(gdbs[1])

    layers <- layers$name[!is.na(layers$geomtype) & layers$features > 0]
  }

  out_list <- list()

  layer_names <- c()
  if(!is.null(out_gpkg) && file.exists(out_gpkg))
    layer_names <- st_layers(out_gpkg)$names

  for(layer in layers) {
    if(!is.null(out_gpkg) && layer %in% layer_names & !overwrite) {
      out <- read_sf(out_gpkg, layer)
    } else {

      layer_set <- lapply(gdbs, get_hr_data, layer = layer, ...)

      out <- do.call(rbind, layer_set)

      try(out <- st_sf(out))
    }

    if(!is.null(out_gpkg) && (!layer %in% layer_names | overwrite)) {
      write_sf(out, layer = layer, dsn = out_gpkg)
    }

    out_list[layer] <- list(out)
  }
  return(out_list)
}

#' Get NHDPlus HiRes Data
#' @param gdb character path to geodatabase to get data from.
#' @param layer character layer name from geodatabase found with \link{sf::st_layers}
#' @param min_size_sqkm numeric minimum basin size to be included in the output
#' @param keep_cols character vector of column names to keep in the output. If NULL,
#' all will be kept.
#' @param proj a projection specification compatible with \link{sf::st_crs}
#' @param simp numeric simplification tolerance in units of projection
#' @export
#' @importFrom sf st_multilinestring st_zm st_transform st_simplify st_crs st_drop_geometry
#' @importFrom dplyr select group_by filter ungroup distinct
get_hr_data <- function(gdb, layer = NULL, min_size_sqkm = NULL,
                        simp = NULL, proj = NULL, keep_cols = NULL,
                        rename = TRUE) {
  if(layer == "NHDFlowline") {
    hr_data <- suppressWarnings(read_sf(gdb, "NHDPlusFlowlineVAA"))
    hr_data <- select(hr_data, -ReachCode, -VPUID)
    hr_data <- left_join( sf::st_zm(read_sf(gdb, layer)), hr_data, by = "NHDPlusID")

    fix <- which(!sapply(sf::st_geometry(hr_data), function(x) class(x)[2]) %in% c("LINESTRING", "MULTILINESTRING"))

    for(f in fix) {
      sf::st_geometry(hr_data)[[f]] <- st_multilinestring(lapply(sf::st_geometry(hr_data)[[f]][[1]],
                                                                 sf::st_cast, to = "LINESTRING"), dim = "XY")
    }

    if(rename) hr_data <- rename_nhdplus(hr_data)

    hr_data <- st_zm(hr_data)

    if(!is.null(proj) && st_crs(proj) != st_crs(hr_data))
      hr_data <- st_transform(hr_data, proj)

    if(!is.null(simp) && simp > 0)
      hr_data <- st_simplify(hr_data, dTolerance = simp)

    if(!is.null(min_size_sqkm)) {
      filter_data <- select(st_drop_geometry(hr_data), LevelPathI, TotDASqKM)
      filter_data <- ungroup(filter(group_by(filter_data, LevelPathI),
                                    TotDASqKM == max(TotDASqKM)))
      filter_data <- distinct(filter(filter_data, TotDASqKM > min_size_sqkm))
      hr_data <- hr_data[hr_data$LevelPathI %in% filter_data$LevelPathI, ]
    }

    if(!is.null(keep_cols)) hr_data <- hr_data[, keep_cols]

    return(hr_data)

  } else {
    read_sf(gdb, layer)
  }
}

#' Make isolated NHDPlusHR region a standalone dataset
#' @description Cleans up and prepares NHDPlusHR regional data for use as complete NHDPlus
#' data. The primary modification applied is to ensure that any flowpath that exits the
#' domain is labeled as a terminal path and attributes are propogated upstream such that
#' the domain is independently complete.
#' @param flowlines sf data.frame of NHDPlusHR flowlines.
#' @param fix_terminals boolean If true, an attempt is made to make flowlines that
#' leave the domain act as terminal paths.
#' @importFrom sf st_zm write_sf st_drop_geometry
#' @importFrom dplyr group_by filter select
#' @export
make_standalone <- function(flowlines, fix_terminals = TRUE) {

  terminals <- flowlines[flowlines$TerminalFl == 1, ]

  terminal_test <- flowlines$TerminalPa %in% terminals$TerminalPa

  if(fix_terminals) {

    flowlines_null <- flowlines[!terminal_test, ]

    # outlets are the lowest hydrosequence on non-terminal flagged outlets
    outlets <- group_by(st_drop_geometry(flowlines_null), .data$TerminalPa)
    outlets <- select(filter(outlets, .data$Hydroseq == min(.data$Hydroseq)),
                      .data$Hydroseq, .data$TerminalPa)

    for(term in unique(flowlines_null$TerminalPa)) {
      term_hydroseq <- outlets$Hydroseq[outlets$TerminalPa == term]
      flowlines$TerminalFl[flowlines$Hydroseq == term_hydroseq] <- 1
      flowlines$TerminalPa[flowlines$TerminalPa == term] <- outlets$Hydroseq[outlets$TerminalPa == term]
    }

    flowlines <- flowlines[(flowlines$FTYPE != 566 & flowlines$TerminalFl != 1), ]

    t_atts <- select(st_drop_geometry(flowlines), .data$COMID, .data$ToNode,
                     .data$FromNode, .data$TerminalFl)

    t_atts <- left_join(t_atts, select(t_atts,
                                       toCOMID = .data$COMID,
                                       .data$FromNode),
                        by = c("ToNode" = "FromNode"))

    na_t_atts <- filter(t_atts, is.na(t_atts$toCOMID) & .data$TerminalFl == 0)

    warning(paste("Found", nrow(na_t_atts),
                  "broken outlets where no toNode and not terminal. Fixing."))

    flowlines$TerminalFl[which(flowlines$COMID %in% na_t_atts$COMID)] <- 1

  } else {

    warning(paste("Removing", sum(!terminal_test),
                  "flowlines that are missing terminal paths."))

    flowlines <- flowlines[terminal_test, ]
  }

  return(flowlines)
}
