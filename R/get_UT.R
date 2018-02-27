#' @title Get all upstream with tributaries COMIDs
#' @description Traverse NHDPlus network upstream with tributaries
#' @param network data.frame NHDPlus flowlines including at a minimum: COMID,
#' LevelPathI, DnLevelPt, and HydroSeq.
#' @param comid integer Identifier to start navigating from.
#' @return integer vector of all COMIDs upstream with tributaries of the starting catchment.
#' @importFrom dplyr filter
#' @export
#'
get_UT <- function(network, comid) {

  # Grab the submitted comids
  main <- filter(network, COMID %in% comid)

  # grab comids for "main network"
  if(length(main$Hydroseq) == 1) {
    main_comid <- filter(network, LevelPathI %in% main$LevelPathI &
                           Hydroseq >= main$Hydroseq)$COMID
  } else {
    main_comid <- filter(network, LevelPathI %in% main$LevelPathI)$COMID
  }

  # find tributary lpids
  trib_lpid <- filter(network, DnLevelPat %in% main$LevelPathI &
                        !LevelPathI %in% main$LevelPathI)$LevelPathI

  # grab comids of tributaries
  trib_comid <- filter(network, LevelPathI %in% trib_lpid)$COMID

  if(length(trib_comid) > 0) {
    comids <- c(main_comid, get_UT(network, trib_comid))
  } else {
    return(main_comid)
  }
}
