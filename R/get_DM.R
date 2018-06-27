#' @title Get all downstream mainstem COMIDs
#' @description Traverse NHDPlus network downstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum: COMID,
#' LevelPathI, DnHydroseq, and HydroSeq.
#' @param comid integer identifier to start navigating from.
#' @return integer vector of all COMIDs downstream of the starting catchment along the mainstem.
#' @importFrom dplyr filter
#' @export
#'
get_DM <- function(network, comid) {

  # Grab the submitted comids
  main <- filter(network, COMID %in% comid)

  # grab comids for "main network"
  main_ds <- filter(network, LevelPathI %in% main$LevelPathI &
                         Hydroseq <= main$Hydroseq)[c("COMID", "Hydroseq")]

  # find ds hydroseq
  ds_hs <- filter(network, Hydroseq == min(main_ds$Hydroseq))$DnHydroseq

  ds_comid <- filter(network, Hydroseq == ds_hs)$COMID

  if(length(ds_comid) > 0) {
    c(main_ds$COMID, get_DM(network, ds_comid))
  } else {
    return(main_ds$COMID)
  }
}
