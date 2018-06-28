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


#' @title Get all upstream mainstem COMIDs
#' @description Traverse NHDPlus network upstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum: COMID,
#' LevelPathI, UsHydroseq, and HydroSeq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs upstream of the starting catchment along the mainstem.
#' @importFrom dplyr filter
#' @export
#'
get_UM <- function(network, comid, distance = NULL) {

  main <- filter(network, COMID %in% comid)

  main_us <- filter(network, LevelPathI %in% main$LevelPathI &
                      Hydroseq >= main$Hydroseq)[c("COMID", "Hydroseq", "LENGTHKM")]

  if(!is.null(distance)) {
    if((sum(main_us$LENGTHKM)) >= distance) {
      # upstream/downstream order
      main_us <- arrange(main_us, Hydroseq)

      starts <- cumsum(main_us$LENGTHKM) - main_us$LENGTHKM
      ends <- cumsum(main_us$LENGTHKM)

      # returns true for all the whole ones and the one partial one.
      main_us <- filter(main_us, ends <= distance | (starts <= distance & ends >= distance))
    }
  }
  return(main_us$COMID)
}

#' @title Get all downstream mainstem COMIDs
#' @description Traverse NHDPlus network downstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum: COMID,
#' LevelPathI, DnHydroseq, and HydroSeq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs downstream of the starting catchment along the mainstem.
#' @importFrom dplyr filter
#' @export
#'
get_DM <- function(network, comid, distance = NULL) {

  private_get_DM(network, comid, distance, run_distance = 0)

}

private_get_DM <- function(network, comid, distance = NULL, run_distance = NULL) {

  main <- filter(network, COMID %in% comid)

  ds_comid <- filter(network,
                     Hydroseq %in% main$DnHydroseq)$COMID

  if(!is.null(distance)) {
    accum_distance <- run_distance + main$LENGTHKM
  }

  if(length(ds_comid) > 0) {
    if(!is.null(distance)) {
      if(accum_distance < distance) {
        c(main$COMID, private_get_DM(network, ds_comid, distance, accum_distance))
      } else {
        return(main$COMID)
      }
    } else {
      c(main$COMID, private_get_DM(network, ds_comid))
    }
  } else {
    return(main$COMID)
  }
}


#' @title Get all downstream mainstem COMIDs
#' @description Traverse NHDPlus network downstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum: COMID,
#' DnMinorHyd, DnHydroseq, and HydroSeq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are returned.
#' The COMID that exceeds the distance specified is returned.
#' The longest of the diverted paths is used for limiting distance.
#' @return integer vector of all COMIDs downstream of the starting catchment along the mainstem.
#' @importFrom dplyr filter
#' @export
#'
get_DD <- function(network, comid, distance = NULL) {

  private_get_DD(network, comid, distance, run_distance = 0)

}

private_get_DD <- function(network, comid, distance = NULL, run_distance = NULL) {

  main <- filter(network, COMID %in% comid)

  ds_comid <- filter(network,
                     Hydroseq %in% main$DnHydroseq | Hydroseq %in% main$DnMinorHyd)$COMID

  if(!is.null(distance)) {
    # This is a bit problematic. Uses the longest of the many at a fork.
    accum_distance <- run_distance + max(main$LENGTHKM)
  }

  if(length(ds_comid) > 0) {
    if(!is.null(distance)) {
      if(accum_distance < distance) {
        unique(c(main$COMID, private_get_DD(network, ds_comid, distance, accum_distance)))
      } else {
        return(main$COMID)
      }
    } else {
      unique(c(main$COMID, private_get_DD(network, ds_comid)))
    }
  } else {
    return(main$COMID)
  }
}
