#' @title Prep NHDPlus Data for Refactor
#' @description Function to prep NHDPlus data for network refactoring
#' @param flines data.frame NHDPlus flowlines including at a minimum: COMID, LENGTHKM,
#' FTYPE, TerminalFl, FromNode, ToNode, TotDASqKM, StartFlag, StreamOrde,
#' StreamCalc, TerminalPa, and Pathlength variables.
#' @param min_network_size numeric Minimum size (sqkm) of drainage network to include in output.
#' @param  min_path_length numeric Minimum length (km) of terminal level path of a network.
#' @param purge_non_dendritic boolean Should non dendritic paths be removed or not.
#' @return data.frame ready to be used with the refactor_flowlines function.
#' @import dplyr
#' @importFrom dplyr select filter left_join
#' @export
#'
prepare_nhdplus <- function(flines, min_network_size, min_path_length, purge_non_dendritic = TRUE) {
  if("sf" %in% class(flines)) {
    warning("removing geometry")
    flines <- sf::st_set_geometry(flines, NULL)
  }

  orig_rows <- nrow(flines)

  flines <- select(flines, COMID, LENGTHKM, FTYPE, TerminalFl,
                   FromNode, ToNode, TotDASqKM, StartFlag,
                   StreamOrde, StreamCalc, TerminalPa, Pathlength, Divergence)

  if(purge_non_dendritic) {
    flines <- filter(flines, FTYPE != "Coastline" & # Remove Coastlines
                     StreamOrde == StreamCalc) #& # Also use streamorder and streamcalc to select only the main paths.
  } else {
    flines <- filter(flines, FTYPE != "Coastline") # Remove Coastlines
    flines[["FromNode"]][which(flines$Divergence == 2)] <- NA
  }
  terminal_filter <- flines$TerminalFl == 1 & flines$TotDASqKM < min_network_size
  start_filter <- flines$StartFlag == 1 & flines$Pathlength < min_path_length

  if(any(terminal_filter) | any(start_filter)) {

    tiny_networks <- rbind(filter(flines, terminal_filter),
                           filter(flines, start_filter))

    flines <- filter(flines, !flines$TerminalPa %in% unique(tiny_networks$TerminalPa))
  }

  warning(paste("Removed", orig_rows - nrow(flines), "flowlines that don't apply.\n",
                "Includes: Coastlines, non-dendritic paths, \nand networks",
                "with drainage area less than",
                min_network_size, "sqkm"))

  # Join ToNode and FromNode along with COMID and Length to get downstream attributes.
  flines <- left_join(flines, select(flines, toCOMID = COMID, FromNode), by = c("ToNode" = "FromNode"))

  if(!all(flines[["TerminalFl"]][which(is.na(flines$toCOMID))] == 1)) {
    stop("FromNode - ToNode imply terminal flowlines that are not\n flagged terminal.",
         "Can't assume NA toCOMIDs go to the ocean.")
  }

  select(flines, -ToNode, -FromNode, -TerminalFl, -StartFlag,
         -StreamOrde, -StreamCalc, -TerminalPa, -FTYPE, -Pathlength, -Divergence)
}
