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
      dir_out <- ifelse(is.null(out[length(out)]), "", out[length(out)])
      out_file <- file.path(dir_out, basename(key))
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
#' Set to NULL to get all available.
#' @param pattern character optional regex to select certain files in hr_dir
#' @param check_terminals boolean if TRUE, run \link{make_standalone} on output.
#' @param overwrite boolean should the output overwrite? If false and the output layer
#' exists, it will be read and returned so this function will always return data even
#' if called a second time for the same output. This is useful for workflows. Note that
#' this will NOT delete the entire Geopackage. It will overwrite on a per layer basis.
#' @param keep_cols character vector of column names to keep in the output. If NULL,
#' all will be kept.
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
#'
#' get_nhdplushr(download_dir, file.path(download_dir, "nhdplus_0302-03.gpkg"))
#'
#' get_nhdplushr(download_dir,
#'               file.path(download_dir, "nhdplus_0302-03.gpkg"),
#'               layers = NULL, overwrite = TRUE)
#'
#' get_nhdplushr(download_dir,
#'               file.path(download_dir, "nhdplus_0302-03.gpkg"),
#'               layers = "NHDFlowline", overwrite = TRUE,
#'               min_size_sqkm = 10, simp = 10, proj = "+init=epsg:5070")
#'
#' }
get_nhdplushr <- function(hr_dir, out_gpkg = NULL,
                          layers = c("NHDFlowline", "NHDPlusCatchment"),
                          pattern = ".*GDB.gdb$", check_terminals = TRUE,
                          overwrite = FALSE, keep_cols = NULL, ...) {

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

    if(grepl("flowline", layer, ignore.case = TRUE) & check_terminals)
      out <- make_standalone(out)

    if(!is.null(out_gpkg) && (!layer %in% layer_names | overwrite)) {
      write_sf(out, layer = layer, dsn = out_gpkg)
    }

    out_list[layer] <- list(cull_cols(out, keep_cols))

  }
  return(out_list)
}

#' Get NHDPlus HiRes Data
#' @description Use to remove unwanted detail NHDPlusHR data
#' See \link{get_nhdplushr} for examples.
#' @param gdb character path to geodatabase to get data from.
#' @param layer character layer name from geodatabase found with \link[sf]{st_layers}
#' @param min_size_sqkm numeric minimum basin size to be included in the output
#' @param proj a projection specification compatible with \link[sf]{st_crs}
#' @param simp numeric simplification tolerance in units of projection
#' @param rename boolean if TRUE, nhdplusTools standard attribute values will
#' be applied.
#' @export
#' @importFrom sf st_transform st_simplify st_crs st_drop_geometry st_geometry
#' @importFrom sf st_cast st_multilinestring st_zm st_geometry<-
#' @importFrom dplyr select group_by filter ungroup distinct
get_hr_data <- function(gdb, layer = NULL, min_size_sqkm = NULL,
                        simp = NULL, proj = NULL, rename = TRUE) {
  if(layer == "NHDFlowline") {
    hr_data <- suppressWarnings(read_sf(gdb, "NHDPlusFlowlineVAA"))
    hr_data <- select(hr_data, -ReachCode, -VPUID)
    hr_data <- left_join(st_zm(read_sf(gdb, layer)), hr_data, by = "NHDPlusID")

    fix <- which( # In the case things come in as non-linestring geometries
      !sapply(st_geometry(hr_data),
              function(x) class(x)[2]) %in% c("LINESTRING", "MULTILINESTRING"))

    for(f in fix) {
      st_geometry(hr_data)[[f]] <- st_multilinestring(lapply(st_geometry(hr_data)[[f]][[1]],
                                                             st_cast, to = "LINESTRING"),
                                                      dim = "XY")
    }

    hr_data <- st_zm(hr_data)

    if(!is.null(min_size_sqkm)) {

      orig_names <- names(hr_data)
      hr_data <- align_nhdplus_names(hr_data)

      filter_data <- select(st_drop_geometry(hr_data), LevelPathI, TotDASqKM)
      filter_data <- ungroup(filter(group_by(filter_data, LevelPathI),
                                    TotDASqKM == max(TotDASqKM)))
      filter_data <- distinct(filter(filter_data, TotDASqKM > min_size_sqkm))
      hr_data <- hr_data[hr_data$LevelPathI %in% filter_data$LevelPathI, ]


      names(hr_data) <- orig_names
    }

  } else {
    hr_data <- read_sf(gdb, layer)
  }

  if(rename) hr_data <- align_nhdplus_names(hr_data)

  if(!is.null(proj) && st_crs(proj) != st_crs(hr_data))
    hr_data <- st_transform(hr_data, proj)

  if(!is.null(simp) && simp > 0)
    hr_data <- st_simplify(hr_data, dTolerance = simp)

  return(hr_data)
}

cull_cols <- function(x, keep_cols) {

  if(is.null(keep_cols)) return(x)

  keep_cols <- keep_cols[keep_cols %in% names(x)]

  geom_name <- attr(x, "sf_column")
  if(!geom_name %in% keep_cols) keep_cols <- c(keep_cols, geom_name)

  x[, keep_cols]
}

#' Make isolated NHDPlusHR region a standalone dataset
#' @description Cleans up and prepares NHDPlusHR regional data for use as complete NHDPlus
#' data. The primary modification applied is to ensure that any flowpath that exits the
#' domain is labeled as a terminal path and attributes are propagated upstream such that
#' the domain is independently complete.
#' @param flowlines sf data.frame of NHDPlusHR flowlines.
#' @importFrom sf st_zm write_sf st_drop_geometry
#' @importFrom dplyr group_by filter select
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' library(sf)
#' source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))
#'
#' (outlet <- filter(hr_data$NHDFlowline, Hydroseq == min(Hydroseq)))
#' nrow(filter(hr_data$NHDFlowline, TerminalPa == outlet$Hydroseq))
#'
#' hr_data$NHDFlowline <- make_standalone(hr_data$NHDFlowline)
#'
#' (outlet <- filter(hr_data$NHDFlowline, Hydroseq == min(Hydroseq)))
#' nrow(filter(hr_data$NHDFlowline, TerminalPa == outlet$Hydroseq))
#'
#' source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))
#'
#' # Remove mainstem and non-dendritic stuff.
#' subset <- filter(hr_data$NHDFlowline,
#'                         StreamLeve > min(hr_data$NHDFlowline$StreamLeve) &
#'                           StreamOrde == StreamCalc)
#'
#' subset <- subset_nhdplus(subset$COMID, nhdplus_data = hr_gpkg)$NHDFlowline
#'
#' plot(sf::st_geometry(hr_data$NHDFlowline))
#'
#' flowline_mod <- make_standalone(subset)
#'
#' terminals <- unique(flowline_mod$TerminalPa)
#'
#' colors <- sample(hcl.colors(length(terminals), palette = "Zissou 1"))
#'
#' for(i in 1:length(terminals)) {
#'   fl <- flowline_mod[flowline_mod$TerminalPa == terminals[i], ]
#'   plot(st_geometry(fl), col = colors[i], lwd = 2, add = TRUE)
#' }
#'
#' ol <- filter(flowline_mod, TerminalFl == 1 & TerminalPa %in% terminals)
#'
#' plot(st_geometry(ol), lwd = 2, add = TRUE)
#'}
make_standalone <- function(flowlines) {

  flowlines <- check_names(flowlines, "make_standalone")

  # Remove non-terminal coastal flowlines
  flowlines <- flowlines[!(flowlines$FTYPE == 566 & flowlines$TerminalFl != 1), ]

  outlets <- select(st_drop_geometry(flowlines),
                    .data$COMID, .data$ToNode,
                    .data$FromNode, .data$TerminalFl,
                    .data$Hydroseq, .data$TerminalPa,
                    .data$LevelPathI)

  outlets <- left_join(outlets,
                       select(outlets,
                              toCOMID = .data$COMID, .data$FromNode),
                       by = c("ToNode" = "FromNode"))

  outlets <- filter(outlets,
                    is.na(.data$toCOMID) & .data$TerminalFl == 0)

  outlets <- select(outlets, .data$Hydroseq, .data$LevelPathI, .data$COMID)

  for(i in seq_len(nrow(outlets))) {
    flowlines = fix_term(outlets[i, ], flowlines)
  }

  return(flowlines)
}


fix_term <- function(term, flowlines) {
  term_hydroseq <- term$Hydroseq
  term_comid <- term$COMID

  # old_term_levelpath is the levelpath of the mainstem of the basin.
  old_term_levelpath <- flowlines$LevelPathI[flowlines$Hydroseq == term_hydroseq]
  old_term_levelpath <- old_term_levelpath[!is.na(old_term_levelpath)]

  # Set the terminal flag of the new basin outlet.
  flowlines$TerminalFl[flowlines$Hydroseq == term_hydroseq] <- 1

  # Change all terminal path IDs to match the new Termal ID of the basin.
  ut <- get_UT(flowlines, term_comid)
  flowlines$TerminalPa[flowlines$COMID %in% ut] <- term_hydroseq

  # Change the mainstem levelpath ID to match the new Terminal ID of the basin.
  flowlines$LevelPathI[flowlines$LevelPathI == old_term_levelpath] <- term_hydroseq

  # Change the old Down Level Paths so they point to the new mainstem levelpath ID
  flowlines$DnLevelPat[flowlines$DnLevelPat == old_term_levelpath] <- term_hydroseq

  # Change olf Up Level Path to point to the new mainstem levelpath ID
  flowlines$UpLevelPat[flowlines$UpLevelPat == old_term_levelpath] <- term_hydroseq

  # Make the Down Hydrosequence and Down Level and Path look like an outlet.
  flowlines$DnLevel[flowlines$Hydroseq == term_hydroseq] <- 0
  flowlines$DnLevelPat[flowlines$Hydroseq == term_hydroseq] <- 0
  flowlines$DnHydroseq[flowlines$Hydroseq == term_hydroseq] <- 0

  return(flowlines)
}
