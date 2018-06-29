#' @title Get all upstream with tributaries COMIDs
#' @description Traverse NHDPlus network upstream with tributaries
#' @param network data.frame NHDPlus flowlines including at a minimum: COMID,
#' Pathlength, LENGTHKM, and HydroSeq.
#' @param comid integer Identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs upstream with tributaries of the starting catchment.
#' @importFrom dplyr filter
#' @export
#'
get_UT <- function(network, comid, distance = NULL) {

  # Grab the submitted comids
  main <- filter(network, COMID %in% comid)

  if(!is.null(distance)) {
    stop_pathlength <- main$Pathlength - main$LENGTHKM + distance

    if(length(main$LENGTHKM) == 1) {
      if(main$LENGTHKM > distance) return(main$COMID)
    }
  } else {
    stop_pathlength <- NULL
  }

  return(private_get_UT(network, comid, distance, stop_pathlength))
}

private_get_UT <- function(network, comid, distance = NULL, stop_pathlength = NULL) {

  # Grab the submitted comids
  main <- filter(network, COMID %in% comid)

  if(!is.null(distance)) {

    ut_comid <- filter(network, (DnHydroseq %in% main$Hydroseq |
                         (DnMinorHyd != 0 & DnMinorHyd %in% main$Hydroseq)) &
                         Pathlength < stop_pathlength)$COMID

  } else {

    ut_comid <- filter(network, (DnHydroseq %in% main$Hydroseq |
                         (DnMinorHyd != 0 & DnMinorHyd %in% main$Hydroseq)))$COMID

  }

  if(length(ut_comid) > 0) {
    return(c(main$COMID, private_get_UT(network, ut_comid, distance, stop_pathlength)))
  } else {
    return(main$COMID)
  }
}

#' @title Get all upstream mainstem COMIDs
#' @description Traverse NHDPlus network upstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum: COMID,
#' Pathlength, LevelPathI, UsHydroseq, and HydroSeq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs upstream of the starting catchment along the mainstem.
#' @importFrom dplyr filter
#' @export
#'
get_UM <- function(network, comid, distance = NULL) {

  main <- filter(network, COMID %in% comid)

  main_us <- filter(network, LevelPathI %in% main$LevelPathI &
                      Hydroseq >= main$Hydroseq)[c("COMID", "Hydroseq",
                                                   "Pathlength", "LENGTHKM")]

  if(!is.null(distance)) {

    if(length(main$LENGTHKM) == 1) {
      if(main$LENGTHKM > distance) return(main$COMID)
    }

    stop_pathlength <- main$Pathlength - main$LENGTHKM + distance

    main_us <- filter(main_us, Pathlength <= stop_pathlength)
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
