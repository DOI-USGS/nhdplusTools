#' @title nhdplus refactor
#' @description refactors nhdplus flowlines
#' @param nhdplus_flines data.frame nhdplus flowloines
#' @param split_flines_meters numeric split threshold in meters
#' @param split_flines_cores numeric number of cores to use for split function
#' @param collapse_flines_meters numeric collapse threshold in meters
#' @param collapse_flines_main_meters numeric collapse threshold for mainstems in meters
#' @param out_collapsed character geopackage path to write collapsed to (one layer per gpkg)
#' @param out_reconciled character geopackage path to write reconciled to (one layer per gpkg)
#' @param three_pass boolean perform three collapse passes or just one
#' @param purge_non_dendritic boolean passed on to prepare_nhdplus
#' @param warn boolean controls whether warning an status messages are printed
#' @importFrom dplyr inner_join
#' @export
#' @examples
#' nhdplus_flowlines <- sf::st_zm(sample_flines)
#' refactor_nhdplus(nhdplus_flines = nhdplus_flowlines,
#'                  split_flines_meters = 2000,
#'                  split_flines_cores = 3,
#'                  collapse_flines_meters = 500,
#'                  collapse_flines_main_meters = 500,
#'                  out_collapsed = "temp.gpkg",
#'                  out_reconciled = "temp_rec.gpkg",
#'                  three_pass = TRUE,
#'                  purge_non_dendritic = FALSE,
#'                  warn = FALSE)
#' unlink("temp.gpkg")
#' unlink("temp_rec.gpkg")
#'

refactor_nhdplus <- function(nhdplus_flines,
                             split_flines_meters,
                             split_flines_cores,
                             collapse_flines_meters,
                             collapse_flines_main_meters,
                             out_collapsed,
                             out_reconciled,
                             three_pass = FALSE,
                             purge_non_dendritic = TRUE,
                             warn = TRUE) {

  if ("FTYPE" %in% names(nhdplus_flines)) {
    nhdplus_flines <- sf::st_set_geometry(nhdplus_flines, NULL) %>%
      prepare_nhdplus(0, 0, purge_non_dendritic, warn = warn) %>%
      inner_join(select(nhdplus_flines, COMID), by = "COMID") %>%
      sf::st_as_sf()
  }

  flines <- nhdplus_flines %>%
    sf::st_cast("LINESTRING", warn = warn) %>%
    sf::st_transform(5070) %>%
    split_flowlines(split_flines_meters, split_flines_cores)

  rm(nhdplus_flines)

  if (warn) {
    message("flowlines split complete, collapsing")
  }

  if (three_pass) {
    collapsed_flines <- collapse_flowlines(sf::st_set_geometry(flines, NULL),
                                           (0.25 * collapse_flines_meters / 1000),
                                           TRUE,
                                           (0.25 * collapse_flines_main_meters / 1000))

    collapsed_flines <- collapse_flowlines(collapsed_flines,
                                           (0.5 * collapse_flines_meters / 1000),
                                           TRUE,
                                           (0.5 * collapse_flines_main_meters / 1000),
                                           warn = FALSE)

    collapsed_flines <- collapse_flowlines(collapsed_flines,
                                           (collapse_flines_meters / 1000),
                                           TRUE,
                                           (collapse_flines_main_meters / 1000),
                                           warn = FALSE)
  } else {
    collapsed_flines <- collapse_flowlines(sf::st_set_geometry(flines, NULL),
                                           (collapse_flines_meters / 1000),
                                           TRUE,
                                           (collapse_flines_main_meters / 1000))
  }

  collapsed_flines %>%
    inner_join(select(flines, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326) %>%
    sf::st_write(out_collapsed, layer_options = "OVERWRITE=YES", quiet = !warn)

  if (warn) {
    message("collapse complete, out collapse written to disk, reconciling")
  }

  collapsed <- reconcile_collapsed_flowlines(collapsed_flines, select(flines, COMID), id = "COMID")

  collapsed$member_COMID <- unlist(lapply(collapsed$member_COMID, function(x) paste(x, collapse = ",")))

  sf::st_write(sf::st_transform(collapsed, 4326),
               out_reconciled,
               layer_options = "OVERWRITE=YES",
               quiet = !warn)
}


#' @title Prep NHDPlus Data for Refactor
#' @description Function to prep NHDPlus data for network refactoring
#' @param flines data.frame NHDPlus flowlines including at a minimum: COMID, LENGTHKM,
#' FTYPE, TerminalFl, FromNode, ToNode, TotDASqKM, StartFlag, StreamOrde,
#' StreamCalc, TerminalPa, and Pathlength variables.
#' @param min_network_size numeric Minimum size (sqkm) of drainage network to include in output.
#' @param  min_path_length numeric Minimum length (km) of terminal level path of a network.
#' @param purge_non_dendritic boolean Should non dendritic paths be removed or not.
#' @param warn boolean controls whether warning an status messages are printed
#' @return data.frame ready to be used with the refactor_flowlines function.
#' @importFrom dplyr select filter left_join
#' @export
#'
prepare_nhdplus <- function(flines,
                            min_network_size,
                            min_path_length,
                            purge_non_dendritic = TRUE,
                            warn = TRUE) {

  if ("sf" %in% class(flines)) {
    if (warn) warning("removing geometry")
    flines <- sf::st_set_geometry(flines, NULL)
  }

  orig_rows <- nrow(flines)

  flines <- select(flines, COMID, LENGTHKM, FTYPE, TerminalFl,
                   FromNode, ToNode, TotDASqKM, StartFlag,
                   StreamOrde, StreamCalc, TerminalPa, Pathlength, Divergence)

  if (purge_non_dendritic) {
    flines <- filter(flines, FTYPE != "Coastline" & # Remove Coastlines
                       StreamOrde == StreamCalc) #& # Also use streamorder and streamcalc to select only the main paths.
  } else {
    flines <- filter(flines, FTYPE != "Coastline") # Remove Coastlines
    flines[["FromNode"]][which(flines$Divergence == 2)] <- NA
  }
  terminal_filter <- flines$TerminalFl == 1 & flines$TotDASqKM < min_network_size
  start_filter <- flines$StartFlag == 1 & flines$Pathlength < min_path_length

  if (any(terminal_filter) | any(start_filter)) {

    tiny_networks <- rbind(filter(flines, terminal_filter),
                           filter(flines, start_filter))

    flines <- filter(flines, !flines$TerminalPa %in% unique(tiny_networks$TerminalPa))
  }
  if (warn) {
    warning(paste("Removed", orig_rows - nrow(flines), "flowlines that don't apply.\n",
                  "Includes: Coastlines, non-dendritic paths, \nand networks",
                  "with drainage area less than",
                  min_network_size, "sqkm"))
  }

  # Join ToNode and FromNode along with COMID and Length to get downstream attributes.
  flines <- left_join(flines, select(flines, toCOMID = COMID, FromNode), by = c("ToNode" = "FromNode"))

  if (!all(flines[["TerminalFl"]][which(is.na(flines$toCOMID))] == 1)) {
    stop(paste("FromNode - ToNode imply terminal flowlines that are not\n",
               "flagged terminal. Can't assume NA toCOMIDs go to the ocean."))
  }

  select(flines, -ToNode, -FromNode, -TerminalFl, -StartFlag,
         -StreamOrde, -StreamCalc, -TerminalPa, -FTYPE, -Pathlength, -Divergence)
}
