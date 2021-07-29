#' @title Prep NHDPlus Data
#' @description Function to prep NHDPlus data for use by nhdplusTools functions
#' @param flines data.frame NHDPlus flowlines including:
#' COMID, LENGTHKM, FTYPE (or FCODE), TerminalFl, FromNode, ToNode, TotDASqKM,
#' StartFlag, StreamOrde, StreamCalc, TerminalPa, Pathlength,
#' and Divergence variables.
#' @param min_network_size numeric Minimum size (sqkm) of drainage network
#' to include in output.
#' @param  min_path_length numeric Minimum length (km) of terminal level
#' path of a network.
#' @param  min_path_size numeric Minimum size (sqkm) of outlet level
#' path of a drainage basin. Drainage basins with an outlet drainage area
#' smaller than this will be removed.
#' @param purge_non_dendritic boolean Should non dendritic paths be removed
#' or not.
#' @param warn boolean controls whether warning an status messages are printed
#' @param error boolean controls whether to return potentially invalid data with a warning rather than an error
#' @param skip_toCOMID boolean if TRUE, toCOMID will not be added to output.
#' @return data.frame ready to be used with the refactor_flowlines function.
#' @importFrom dplyr select filter left_join group_split group_by bind_rows
#' @export
#' @examples
#'
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' prepare_nhdplus(sample_flines,
#'                 min_network_size = 10,
#'                 min_path_length = 1,
#'                 warn = FALSE)
#'
prepare_nhdplus <- function(flines,
                            min_network_size,
                            min_path_length,
                            min_path_size = 0,
                            purge_non_dendritic = TRUE,
                            warn = TRUE,
                            error = TRUE,
                            skip_toCOMID = FALSE) {

  req_names <- c("COMID", "LENGTHKM", "TerminalFl",
            "Hydroseq", "TotDASqKM", "StartFlag", "TerminalPa",
            "Pathlength", "Divergence",
            "LevelPathI")

  dend <- c("StreamOrde", "StreamCalc")
  sz <- c("TotDASqKM", "Hydroseq", "LevelPathI")
  tocomid <- c("ToNode", "FromNode")

  dont_return <- c("ToNode", "FromNode", "TerminalFl",
                   "StartFlag", "StreamOrde", "StreamCalc",
                   "TerminalPa", "Pathlength", "Divergence")

  if(purge_non_dendritic) {
    req_names <- c(req_names, dend)
  }

  if(min_path_size > 0 | min_network_size > 0 | min_path_length > 0) {
    req_names <- c(req_names, sz)
  }

  if(!skip_toCOMID) {
    req_names <- c(req_names, tocomid)
  }

  assign("prepare_nhdplus_attributes",
         unique(req_names),
         envir = nhdplusTools_env)

  flines <- check_names(flines, "prepare_nhdplus")

  if ("sf" %in% class(flines)) {
    if (warn) warning("removing geometry")
    flines <- drop_geometry(flines)
  }

  orig_rows <- nrow(flines)

  flines <- filter_coastal(flines)

  flines <- flines[ , names(flines) %in% req_names]

  if (!any(flines$TerminalFl == 1)) {
    warning("Got NHDPlus data without a Terminal catchment. Attempting to find it.")
    if (all(flines$TerminalPa == flines$TerminalPa[1])) {
      out_ind <- which(flines$Hydroseq == min(flines$Hydroseq))
      flines$TerminalFl[out_ind] <- 1
    } else {
      stop("Multiple networks without terminal flags found. Can't proceed.")
    }
  }

  if (purge_non_dendritic) {

    flines <- filter(flines, StreamOrde == StreamCalc)

  } else {
    if(!skip_toCOMID) {
      flines[["FromNode"]][which(flines$Divergence == 2)] <- NA
    }
  }

  if(min_path_size > 0) {

    remove_paths <- group_by(flines, LevelPathI)
    remove_paths <- filter(remove_paths, Hydroseq == min(Hydroseq))
    remove_paths <- filter(remove_paths, TotDASqKM < min_path_size & TotDASqKM >= 0)$LevelPathI
    flines <- filter(flines, !LevelPathI %in% remove_paths)

  }

  if(min_network_size > 0 | min_path_length > 0) {

    terminal_filter <- flines$TerminalFl == 1 &
      flines$TotDASqKM < min_network_size
    start_filter <- flines$StartFlag == 1 &
      flines$Pathlength < min_path_length

    if (any(terminal_filter, na.rm = TRUE) | any(start_filter, na.rm = TRUE)) {

      tiny_networks <- rbind(filter(flines, terminal_filter),
                             filter(flines, start_filter))

      flines <- filter(flines, !flines$TerminalPa %in%
                         unique(tiny_networks$TerminalPa))

    }
  }

  if (warn) {
    warning(paste("Removed", orig_rows - nrow(flines),
                  "flowlines that don't apply.\n",
                  "Includes: Coastlines, non-dendritic paths, \nand networks",
                  "with drainage area less than",
                  min_network_size, "sqkm, and drainage basins smaller than",
                  min_path_size))
  }

  if(skip_toCOMID) {
    return(flines[ , !names(flines) %in%
                     dont_return])
  }

  if(nrow(flines) > 0) {
    # Join ToNode and FromNode along with COMID and Length to
    # get downstream attributes. This is done grouped by
    # terminalpathID becasue duplicate node ids were encountered
    # accross basins.
    flines <- group_by(flines, TerminalPa)
    flines <- group_split(flines)

    flines <- lapply(flines, function(x){
      left_join(x, select(x,
                          toCOMID = .data$COMID,
                          .data$FromNode),
                by = c("ToNode" = "FromNode"))})

    flines <- bind_rows(flines)
  }

  if (!all(flines[["TerminalFl"]][which(is.na(flines$toCOMID))] == 1)) {
    warn <- paste("FromNode - ToNode imply terminal flowlines that are not\n",
                  "flagged terminal. Can't assume NA toCOMIDs go to the ocean.")
    if(error) {
      stop(warn)
    } else {
      warning(warn)
    }
  }

  flines[ , !names(flines) %in%
            dont_return]
}


filter_coastal <- function(flines) {
  if("FCODE" %in% names(flines)) {
    flines <- filter(flines, (FCODE != 566600))
  } else if("FTYPE" %in% names(flines)) {
    flines <- filter(flines, (FTYPE != "Coastline" | FTYPE != 566))
  } else {
    stop("must provide FCODE and/or FTYPE to filter coastline.")
  }
}
