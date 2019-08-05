#' @title Navigate Upstream with Tributaries
#' @description Traverse NHDPlus network upstream with tributaries
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID, Pathlength, LENGTHKM, and Hydroseq.
#' @param comid integer Identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are
#' returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs upstream with tributaries of the
#' starting COMID.
#' @importFrom dplyr filter select
#' @export
#' @examples
#' library(sf)
#' sample_flines <- read_sf(system.file("extdata",
#'                                      "petapsco_flowlines.gpkg",
#'                                      package = "nhdplusTools"))
#' plot(sample_flines$geom)
#' start_COMID <- 11690196
#' UT_COMIDs <- get_UT(sample_flines, start_COMID)
#' plot(dplyr::filter(sample_flines, COMID %in% UT_COMIDs)$geom,
#'      col = "red", add = TRUE)
#'
#' UT_COMIDs <- get_UT(sample_flines, start_COMID, distance = 50)
#' plot(dplyr::filter(sample_flines, COMID %in% UT_COMIDs)$geom,
#'      col = "blue", add = TRUE)
#'
get_UT <- function(network, comid, distance = NULL) {

  network <- check_names(network, "get_UT")

  network <- dplyr::select(network, get("get_UT_attributes",
                                        nhdplusTools_env))

  if ("sf" %in% class(network)) network <- sf::st_set_geometry(network, NULL)

  start_comid <- filter(network, COMID == comid)

  if (!is.null(distance)) {
    if (distance < start_comid$LENGTHKM) return(comid)
  }

  all <- private_get_UT(network, comid)

  if (!is.null(distance)) {
    stop_pathlength <- start_comid$Pathlength -
      start_comid$LENGTHKM +
      distance

    network <- filter(network, COMID %in% all)

    return(filter(network, Pathlength <= stop_pathlength)$COMID)
  } else {
    return(all)
  }
}

private_get_UT <- function(network, comid) {

  main <- filter(network, COMID %in% comid)

  if (length(main$Hydroseq) == 1) {
    full_main <- filter(network,
                         LevelPathI %in% main$LevelPathI &
                           Hydroseq >= main$Hydroseq)

    trib_lpid <- filter(network, DnHydroseq %in% full_main$Hydroseq &
                          !LevelPathI %in% main$LevelPathI  &
                          Hydroseq >= main$Hydroseq)$LevelPathI
  } else {
    full_main <- filter(network, LevelPathI %in% main$LevelPathI)

    trib_lpid <- filter(network, DnHydroseq %in% full_main$Hydroseq &
                          !LevelPathI %in% main$LevelPathI)$LevelPathI
  }

  trib_comid <- filter(network, LevelPathI %in% trib_lpid)$COMID

  if (length(trib_comid) > 0) {
    return(c(full_main$COMID, private_get_UT(network, trib_comid)))
  } else {
    return(full_main$COMID)
  }
}

#' @title Navigate Upstream Mainstem
#' @description Traverse NHDPlus network upstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID,Pathlength, LevelPathI, UpHydroseq, and Hydroseq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are
#' returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs upstream of the starting COMID
#' along the mainstem.
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(sf)
#' sample_flines <- read_sf(system.file("extdata",
#'                                      "petapsco_flowlines.gpkg",
#'                                      package = "nhdplusTools"))
#' plot(sample_flines$geom)
#' start_COMID <- 11690196
#' UM_COMIDs <- get_UM(sample_flines, start_COMID)
#' plot(dplyr::filter(sample_flines, COMID %in% UM_COMIDs)$geom,
#'      col = "red", add = TRUE, lwd = 3)
#'
#' UM_COMIDs <- get_UM(sample_flines, start_COMID, distance = 50)
#' plot(dplyr::filter(sample_flines, COMID %in% UM_COMIDs)$geom,
#'      col = "blue", add = TRUE, lwd = 2)
#'
get_UM <- function(network, comid, distance = NULL) {

  network <- check_names(network, "get_UM")

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

#' @title Navigate Downstream Mainstem
#' @description Traverse NHDPlus network downstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID, LENGTHKM, DnHydroseq, and Hydroseq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are
#' returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs downstream of the starting COMID
#' along the mainstem.
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(sf)
#' sample_flines <- read_sf(system.file("extdata",
#'                                      "petapsco_flowlines.gpkg",
#'                                      package = "nhdplusTools"))
#' plot(sample_flines$geom)
#' start_COMID <- 11690092
#' DM_COMIDs <- get_DM(sample_flines, start_COMID)
#' plot(dplyr::filter(sample_flines, COMID %in% DM_COMIDs)$geom,
#'      col = "red", add = TRUE, lwd = 3)
#'
#' DM_COMIDs <- get_DM(sample_flines, start_COMID, distance = 40)
#' plot(dplyr::filter(sample_flines, COMID %in% DM_COMIDs)$geom,
#'      col = "blue", add = TRUE, lwd = 2)
#'
#'
get_DM <- function(network, comid, distance = NULL) {

  if (!is.null(distance)) {
    network <- check_names(network, "get_DM")

    network <- dplyr::select(network, get("get_DM_attributes",
                                          nhdplusTools_env))
  } else {
    network <- check_names(network, "get_DM_nolength")

    network <- dplyr::select(network, get("get_DM_nolength_attributes",
                                          nhdplusTools_env))
  }

  if ("sf" %in% class(network)) network <- sf::st_set_geometry(network, NULL)

  start_comid <- filter(network, COMID == comid)

  if (!is.null(distance)) {
    if (distance < start_comid$LENGTHKM) return(comid)
  }

  all <- private_get_DM(network, comid)

  if (!is.null(distance)) {
    stop_pathlength <- start_comid$Pathlength +
      start_comid$LENGTHKM -
      distance

    network <- filter(network, COMID %in% all)

    return(filter(network, (Pathlength + LENGTHKM) >= stop_pathlength)$COMID)
  } else {
    return(all)
  }

}

private_get_DM <- function(network, comid) {

  main <- ds_main <- filter(network, COMID %in% comid)

  if (length(main$Hydroseq) == 1) {
    ds_main <- filter(network,
                      LevelPathI %in% main$LevelPathI &
                        Hydroseq <= main$Hydroseq)
  }

  ds_hs <- filter(ds_main, !DnLevelPat %in% main$LevelPathI)$DnHydroseq

  if(length(ds_hs) > 0) {
    ds_lpid <- filter(network, Hydroseq == ds_hs)$LevelPathI
    if (length(ds_lpid) > 0) {
      ds_comid <- filter(network,
                         LevelPathI == ds_lpid &
                           Hydroseq <= ds_hs)$COMID
      c(ds_main$COMID, private_get_DM(network, ds_comid))
    } else {
      return(ds_main$COMID)
    }
  } else {
    return(ds_main$COMID)
  }
}


#' @title Navigate Downstream with Diversions
#' @description Traverse NHDPlus network downstream with diversions
#' NOTE: This algorithm may not scale well in large watersheds.
#' For reference, the lower Mississippi will take over a minute.
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID, DnMinorHyd, DnHydroseq, and Hydroseq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many
#' COMIDs are returned.
#' The COMID that exceeds the distance specified is returned.
#' The longest of the diverted paths is used for limiting distance.
#' @return integer vector of all COMIDs downstream of the starting COMID
#' @importFrom dplyr filter
#' @export
#' @examples
#' library(sf)
#' start_COMID <- 11688818
#' sample_flines <- read_sf(system.file("extdata",
#'                                      "petapsco_flowlines.gpkg",
#'                                      package = "nhdplusTools"))
#' DD_COMIDs <- get_DD(sample_flines, start_COMID, distance = 4)
#' plot(dplyr::filter(sample_flines, COMID %in% DD_COMIDs)$geom,
#'      col = "red", lwd = 2)
#'
#' DM_COMIDs <- get_DM(sample_flines, start_COMID, distance = 4)
#' plot(dplyr::filter(sample_flines, COMID %in% DM_COMIDs)$geom,
#'      col = "blue", add = TRUE, lwd = 2)
#'
get_DD <- function(network, comid, distance = NULL) {

  network <- check_names(network, "get_DD")

  network <- dplyr::select(network, get("get_DD_attributes",
                                        nhdplusTools_env))

  if ("sf" %in% class(network)) network <- sf::st_set_geometry(network, NULL)

  start_comid <- filter(network, COMID == comid)

  stop_pathlength <- 0

  if (!is.null(distance)) {
    if (distance < start_comid$LENGTHKM) return(comid)

    stop_pathlength <- start_comid$Pathlength +
      start_comid$LENGTHKM -
      distance
  }
  all <- private_get_DD(network, comid, stop_pathlength)

  if (!is.null(distance)) {
    network <- filter(network, COMID %in% unique(all))

    return(filter(network, (Pathlength + LENGTHKM) >= stop_pathlength)$COMID)
  } else {
    return(unique(all))
  }
}

private_get_DD <- function(network, comid, stop_pathlength = 0) {

  main <- ds_main <- filter(network, COMID %in% comid)

  if (length(main$Hydroseq) == 1) {
    ds_main <- filter(network,
                      LevelPathI %in% main$LevelPathI &
                        Hydroseq <= main$Hydroseq)
  }

  ds_hs <- c(filter(ds_main, !DnLevelPat %in% main$LevelPathI)$DnHydroseq,
             filter(ds_main, !DnMinorHyd == 0)$DnMinorHyd)

  ds_lpid <- filter(network, Hydroseq %in% ds_hs)$LevelPathI

  if (length(ds_lpid) > 0) {
    if (length(ds_hs) == 1) {
      # Same as DM
      ds_comid <- filter(network,
                         LevelPathI %in% ds_lpid &
                           Hydroseq <= ds_hs)$COMID
    } else {
      # Works for divergent paths.
      ds_hs <- filter(network, Hydroseq %in% ds_hs)
      ds_comid <- filter(network, LevelPathI %in% ds_lpid) %>%
        dplyr::left_join(select(ds_hs, LevelPathI, max_Hydroseq = Hydroseq),
                  by = "LevelPathI") %>%
        filter(Hydroseq <= .data$max_Hydroseq)
      ds_comid <- ds_comid$COMID
    }

    # This allows this algorithm to work for short distances
    # in a reasonable time in large systems.
    if (all(ds_main$Pathlength <= stop_pathlength)) return(ds_main$COMID)

    c(ds_main$COMID, private_get_DD(network, ds_comid, stop_pathlength))
  } else {
    return(ds_main$COMID)
  }
}
