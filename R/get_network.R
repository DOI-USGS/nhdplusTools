#' @title Get all upstream with tributaries COMIDs
#' @description Traverse NHDPlus network upstream with tributaries
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID, Pathlength, LENGTHKM, and Hydroseq.
#' @param comid integer Identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are
#' returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs upstream with tributaries of the
#' starting catchment.
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(sf)
#' plot(sample_flines$geometry)
#' start_COMID <- 11690196
#' UT_COMIDs <- get_UT(sample_flines, start_COMID)
#' plot(dplyr::filter(sample_flines, COMID %in% UT_COMIDs)$geometry,
#'      col = "red", add = TRUE)
#'
#' UT_COMIDs <- get_UT(sample_flines, start_COMID, distance = 50)
#' plot(dplyr::filter(sample_flines, COMID %in% UT_COMIDs)$geometry,
#'      col = "blue", add = TRUE)
#'
get_UT <- function(network, comid, distance = NULL) {

  check_names(names(network), "get_UT")

  # Grab the submitted comids
  main <- filter(network, COMID %in% comid)

  if (!is.null(distance)) {
    stop_pathlength <- main$Pathlength - main$LENGTHKM + distance

    if (length(main$LENGTHKM) == 1) {
      if (main$LENGTHKM > distance) return(main$COMID)
    }
  } else {
    stop_pathlength <- NULL
  }

  return(private_get_UT(network, comid, distance, stop_pathlength))
}

private_get_UT <- function(network, comid,
                           distance = NULL,
                           stop_pathlength = NULL) {

  # Grab the submitted comids
  main <- filter(network, COMID %in% comid)

  if (!is.null(distance)) {

    ut_comid <- filter(network, (DnHydroseq %in% main$Hydroseq |
                                   (DnMinorHyd != 0 &
                                      DnMinorHyd %in% main$Hydroseq)) &
                         Pathlength < stop_pathlength)$COMID

  } else {

    ut_comid <- filter(network, (DnHydroseq %in% main$Hydroseq |
                                   (DnMinorHyd != 0 &
                                      DnMinorHyd %in% main$Hydroseq)))$COMID

  }

  if (length(ut_comid) > 0) {
    return(c(main$COMID, private_get_UT(network, ut_comid,
                                        distance, stop_pathlength)))
  } else {
    return(main$COMID)
  }
}

#' @title Get all upstream mainstem COMIDs
#' @description Traverse NHDPlus network upstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID,Pathlength, LevelPathI, UpHydroseq, and Hydroseq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are
#' returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs upstream of the starting catchment
#' along the mainstem.
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(sf)
#' plot(sample_flines$geometry)
#' start_COMID <- 11690196
#' UM_COMIDs <- get_UM(sample_flines, start_COMID)
#' plot(dplyr::filter(sample_flines, COMID %in% UM_COMIDs)$geometry,
#'      col = "red", add = TRUE, lwd = 3)
#'
#' UM_COMIDs <- get_UM(sample_flines, start_COMID, distance = 50)
#' plot(dplyr::filter(sample_flines, COMID %in% UM_COMIDs)$geometry,
#'      col = "blue", add = TRUE, lwd = 2)
#'
get_UM <- function(network, comid, distance = NULL) {

  check_names(names(network), "get_UM")

  main <- filter(network, COMID %in% comid)

  main_us <- filter(network, LevelPathI %in% main$LevelPathI &
                      Hydroseq >= main$Hydroseq)[c("COMID", "Hydroseq",
                                                   "Pathlength", "LENGTHKM")]

  if (!is.null(distance)) {

    if (length(main$LENGTHKM) == 1) {
      if (main$LENGTHKM > distance) return(main$COMID)
    }

    stop_pathlength <- main$Pathlength - main$LENGTHKM + distance

    main_us <- filter(main_us, Pathlength <= stop_pathlength)
  }
  return(main_us$COMID)
}

#' @title Get all downstream mainstem COMIDs
#' @description Traverse NHDPlus network downstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID, LENGTHKM, DnHydroseq, and Hydroseq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are
#' returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs downstream of the starting catchment
#' along the mainstem.
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(sf)
#' plot(sample_flines$geometry)
#' start_COMID <- 11690092
#' DM_COMIDs <- get_DM(sample_flines, start_COMID)
#' plot(dplyr::filter(sample_flines, COMID %in% DM_COMIDs)$geometry,
#'      col = "red", add = TRUE, lwd = 3)
#'
#' DM_COMIDs <- get_DM(sample_flines, start_COMID, distance = 40)
#' plot(dplyr::filter(sample_flines, COMID %in% DM_COMIDs)$geometry,
#'      col = "blue", add = TRUE, lwd = 2)
#'
#'
get_DM <- function(network, comid, distance = NULL) {

  check_names(names(network), "get_DM")

  private_get_DM(network, comid, distance, run_distance = 0)

}

private_get_DM <- function(network, comid, distance = NULL,
                           run_distance = NULL) {

  main <- filter(network, COMID %in% comid)

  ds_comid <- filter(network,
                     Hydroseq %in% main$DnHydroseq)$COMID

  if (!is.null(distance)) {
    accum_distance <- run_distance + main$LENGTHKM
  }

  if (length(ds_comid) > 0) {
    if (!is.null(distance)) {
      if (accum_distance < distance) {
        c(main$COMID, private_get_DM(network, ds_comid,
                                     distance, accum_distance))
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


#' @title Get all downstream COMIDs including diversions
#' @description Traverse NHDPlus network downstream with diversions
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID, DnMinorHyd, DnHydroseq, and Hydroseq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many
#' COMIDs are returned.
#' The COMID that exceeds the distance specified is returned.
#' The longest of the diverted paths is used for limiting distance.
#' @return integer vector of all COMIDs downstream of the starting catchment
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(sf)
#' start_COMID <- 11688818
#' DD_COMIDs <- get_DD(sample_flines, start_COMID, distance = 4)
#' plot(dplyr::filter(sample_flines, COMID %in% DD_COMIDs)$geometry,
#'      col = "red", lwd = 2)
#'
#' DM_COMIDs <- get_DM(sample_flines, start_COMID, distance = 4)
#' plot(dplyr::filter(sample_flines, COMID %in% DM_COMIDs)$geometry,
#'      col = "blue", add = TRUE, lwd = 2)
#'
get_DD <- function(network, comid, distance = NULL) {

  check_names(names(network), "get_DD")

  private_get_DD(network, comid, distance, run_distance = 0)

}

private_get_DD <- function(network,
                           comid,
                           distance = NULL,
                           run_distance = NULL) {

  main <- dplyr::filter(network, COMID %in% comid)

  ds_comid <- dplyr::filter(network,
                     Hydroseq %in% main$DnHydroseq |
                       Hydroseq %in% main$DnMinorHyd)$COMID

  if (!is.null(distance)) {
    # This is a bit problematic. Uses the longest of the many at a fork.
    accum_distance <- run_distance + max(main$LENGTHKM)
  }

  if (length(ds_comid) > 0) {
    if (!is.null(distance)) {
      if (accum_distance < distance) {
        unique(c(main$COMID,
                 private_get_DD(network, ds_comid,
                                distance, accum_distance)))
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
