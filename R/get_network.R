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
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
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

  if ("sf" %in% class(network)) network <- sf::st_set_geometry(network, NULL)

  network <- network %>% check_names("get_UT") %>%
    dplyr::select(get("get_UT_attributes", nhdplusTools_env))

  start_comid <- get_start_comid(network, comid)

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
#' COMID,Pathlength, LevelPathI, and Hydroseq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are
#' @param sort if TRUE, the returned COMID vector will be sorted in order of distance from the input COMID (nearest to farthest)
#' @param include if TRUE, the input COMID will be included in the returned COMID vector
#' returned. The COMID that exceeds the distance specified is returned.
#' @return integer vector of all COMIDs upstream of the starting COMID
#' along the mainstem
#' @importFrom dplyr filter select arrange
#' @export
#' @examples
#' library(sf)
#'
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
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

get_UM <- function(network, comid, distance = NULL, sort = FALSE, include = TRUE) {

  network <- check_names(network, "get_UM")

  main <- network %>%
    filter(COMID %in% comid) %>%
    select(COMID, LevelPathI, Hydroseq, Pathlength, LENGTHKM)

  main_us <- network %>%
    filter(LevelPathI %in% main$LevelPathI & Hydroseq >= main$Hydroseq) %>%
    select(COMID, Hydroseq, Pathlength, LENGTHKM)

  if (!is.null(distance)) {

    if (length(main$LENGTHKM) == 1) {
      if (main$LENGTHKM > distance) {
        return(main$COMID)
      }
    }

    stop_pathlength <- main$Pathlength - main$LENGTHKM + distance

    main_us <- filter(main_us, Pathlength <= stop_pathlength)

  }

  if(sort) { main_us <-  arrange(main_us, Hydroseq) }
  if(!include) {  main_us = filter(main_us, COMID != comid) }

  return(main_us$COMID)
}

#' @title Navigate Downstream Mainstem
#' @description Traverse NHDPlus network downstream main stem
#' @param network data.frame NHDPlus flowlines including at a minimum:
#' COMID, LENGTHKM, DnHydroseq, and Hydroseq.
#' @param comid integer identifier to start navigating from.
#' @param distance numeric distance in km to limit how many COMIDs are
#' returned. The COMID that exceeds the distance specified is returned.
#' @param sort if TRUE, the returned COMID vector will be sorted in order of distance from the input COMID (nearest to farthest)
#' @param include if TRUE, the input COMID will be included in the returned COMID vector
#' @return integer vector of all COMIDs downstream of the starting COMID
#' along the mainstem
#' @importFrom dplyr select filter arrange desc
#' @export
#' @examples
#' library(sf)
#'
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
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

get_DM <- function(network, comid, distance = NULL, sort = FALSE, include = TRUE) {

  if ("sf" %in% class(network)) { network <- sf::st_set_geometry(network, NULL) }

  type <- ifelse(is.null(distance),  "get_DM_nolength", "get_DM")

  network <- network %>%
    check_names(type) %>%
    select(get(paste0(type, "_attributes"), nhdplusTools_env))

  start_comid <- get_start_comid(network, comid)

  if (!is.null(distance)) {
    if (distance < start_comid$LENGTHKM){
      return(comid)
    }
  }

  main_ds <- private_get_DM(network, comid)

  if (!is.null(distance)) {

    stop_pathlength <- start_comid$Pathlength + start_comid$LENGTHKM - distance

    main_ds <- network %>%
      filter(COMID %in% main_ds$COMID, (Pathlength + LENGTHKM) >= stop_pathlength)
  }

  if(sort){ main_ds <- arrange(main_ds, desc(Hydroseq)) }
  if(!include){ main_ds <- filter(main_ds, COMID != comid) }

  return(main_ds$COMID)
}


private_get_DM <- function(network, comid) {

  main <- ds_main <- filter(network, COMID %in% comid)

  if (length(main$Hydroseq) == 1) {
    ds_main <- network %>%
      filter(LevelPathI %in% main$LevelPathI &
               Hydroseq <= main$Hydroseq)
  }

  ds_hs <- ds_main %>%
    filter(!DnLevelPat %in% main$LevelPathI) %>%
    select(DnHydroseq)

  if (nrow(ds_hs) > 0) {

    ds_lpid <- network %>%
      filter(Hydroseq == ds_hs$DnHydroseq) %>%
      select(LevelPathI)

    if (nrow(ds_lpid) > 0) {
      ds_comid <- network %>%
        filter(LevelPathI == ds_lpid$LevelPathI & Hydroseq <= ds_hs$DnHydroseq) %>%
        select(COMID)

      return(rbind(
        select(ds_main, COMID, Hydroseq),
        private_get_DM(network, comid = ds_comid$COMID)
      ))
    }
  }

  return(select(ds_main, COMID, Hydroseq))
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
#'
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' DD_COMIDs <- get_DD(sample_flines, start_COMID, distance = 4)
#' plot(dplyr::filter(sample_flines, COMID %in% DD_COMIDs)$geom,
#'      col = "red", lwd = 2)
#'
#' DM_COMIDs <- get_DM(sample_flines, start_COMID, distance = 4)
#' plot(dplyr::filter(sample_flines, COMID %in% DM_COMIDs)$geom,
#'      col = "blue", add = TRUE, lwd = 2)
#'
get_DD <- function(network, comid, distance = NULL) {

  if ("sf" %in% class(network)) network <- sf::st_set_geometry(network, NULL)

  network <- network %>% check_names("get_DD") %>%
    dplyr::select(get("get_DD_attributes", nhdplusTools_env))

  start_comid <- get_start_comid(network, comid)

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

get_start_comid <- function(network, comid) {
  start_comid <- filter(network, COMID == comid)

  if(nrow(start_comid) > 1) {
    stop("Found duplicate ID for starting catchment. Duplicate rows in network?")
  }

  start_comid
}

#' Navigate Network
#' @description Provides a full feature network navigation function that
#' will work with local or web service data. Parameter details provide
#' context.
#' @param start list, integer, sf, or sfc if list must be a valid NLDI feature
#' if integer must be a valid comid. If sf, must contain a "comid" field.
#' @param mode character chosen from c(UM, DM, UT, or DD)
#' @param network sf should be compatible with network navigation functions
#' If NULL, network will be derived from requests to the NLDI
#' @param output character flowline or a valid NLDI data source
#' @param distance_km numeric distance to navigate in km
#' @param trim_start logical should start be trimmed or include entire catchment?
#' @param trim_stop logical should stop(s) be trimmed or include entire catchment(s)?
#' # Not supported
#' @param trim_tolerance numeric from 0 to 100 percent of flowline length. If amount
#' to trim is less than this tolerance, no trim will be applied.
#' @export
#' @examples
#'
#' \donttest{
#' navigate_network(list(featureSource = "nwissite", featureID = "USGS-06287800"),
#'                 "UM",
#'                 output = "flowlines",
#'                 trim_start = TRUE)
#' }
#'
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' hydro_location <- list(comid = 5329339,
#'                       reachcode = "18050005000078",
#'                        reach_meas = 30)
#'
#' hydro_location <- sf::st_sf(
#'   hydro_location,
#'   geom = nhdplusTools::get_hydro_location(data.frame(hydro_location),
#'                                           walker_flowline))
#'
#' net <- navigate_network(hydro_location,
#'                        mode = "DM", network = walker_flowline,
#'                        trim_start = TRUE, distance_km = 20)
#'
#' plot(sf::st_geometry(walker_flowline))
#' plot(sf::st_geometry(hydro_location), add = TRUE)
#' plot(sf::st_geometry(net), add = TRUE, col = "blue", lwd = 2)
#'
navigate_network <- function(start, mode = "UM", network = NULL,
                             output = "flowlines", distance_km = 10,
                             trim_start = FALSE, trim_stop = FALSE,
                             trim_tolerance = 5) {

  # Work with start to get a start_comid
  if(!is.numeric(start)) {
    if(inherits(start, "sfc")) {

      start_comid <- discover_nhdplus_id(point = start)

    } else if(inherits(start, "sf")) {

      start_comid <- start$comid

    } else if(is.list(start)) {

      start <- get_nldi_feature(start)
      start_comid <- as.integer(start$comid)

    } else {

      stop("start must be integer, list, or sfc geometry")

    }

  } else {

    if(!start %% 1 == 0) {
      stop("If start is numeric it must be a comid integer")
    }

    start <- as.integer(start)
    start_comid <- start

  }

  # in the case that we don't have a network, we need to get it
  # from the NLDI and web services.
  if(is.null(network)) {

    network <- navigate_nldi(list(featureSource = "comid",
                                  featureID = start_comid),
                             mode, "flowlines", distance_km)

    network <- network[names(network) != "origin"][[1]]$nhdplus_comid

    network <- subset_nhdplus(as.integer(network),
                              nhdplus_data = "download",
                              status = TRUE,
                              flowline_only = TRUE)

    network <- network$NHDFlowline_Network

  } else {

    # If we have a network, we need to filter it down to
    # the desired navigation.
    network <- check_names(network, paste0("get_", mode),
                           tolower = TRUE)

    if(!start_comid %in% network$comid)
      stop("start comid not in network?")

    network <- dplyr::filter(network, .data$comid %in%
                               if(mode == "UM") {
                                 get_UM(network, start_comid, distance_km, include = TRUE)
                               } else if(mode == "UT") {
                                 get_UT(network, start_comid, distance_km)
                               } else if(mode == "DD") {
                                 get_DD(network, start_comid, distance_km)
                               } else if(mode == "DM") {
                                 get_DM(network, start_comid, distance_km, include = TRUE)
                               }
    )

  }


  # we now have a network that matches our navigation and, if trim options are
  # FALSE, can output the desired output data.

  # we need a precise measure for our point
  if(trim_start) {

    if(!is.numeric(start)) {

      if(inherits(start, "sf") | inherits(start, "sfc")) {
        start <- sf::st_transform(start,
                                  sf::st_crs(network))
      }

      if(sf::st_is_longlat(start)) {
        search_radius <- 0.001
      } else {
        search_radius <- 100
      }

      event <- get_flowline_index(flines = network,
                                 points = start,
                                 search_radius = search_radius,
                                 precision = 10)

      event <- sf::st_sf(event,
                         geom = get_hydro_location(event, network))

    }

  }

  # get the output data if it's not flowlines
  if(!output == "flowlines") {

    out_features <- navigate_nldi(list(featureSource = "comid",
                                       featureID = start_comid),
                                  mode, output, distance_km
                                  )[[paste0(mode, "_", output)]]

  } else {

    out_features <- NULL

  }

  if(trim_stop) {

    stop("Trim Stop Not Supported")

  }

  # now trim start if requested
  if(trim_start) {

    if(!is.integer(start)) {
      if(output == "flowlines") {
        # trim event flowline to measure of event

        l <- network[network$comid == start_comid, ]

        rm <- as.numeric(event$REACH_meas)
        lf <- l$frommeas
        lt <- l$tomeas

        # the event measure can be outside the network?
        if(!dplyr::between(rm, lf, lt)) {
          df <- abs(c(lf, lt) - rm)
          split <- c(lf, lt)[which(df == min(df))]
        } else {
          # Convert start to comid measure
          split <- rescale_measures(as.numeric(event$REACH_meas),
                                    l$frommeas, l$tomeas)
        }

        if(grepl("UM|UT", mode)) {

          f <- 0
          t <- 1 - (split / 100)

        } else {

          f <- 1 - (split / 100)
          t <- 1

        }

        if(abs(f - t) < trim_tolerance / 100) {

          warning("No split applied, under tolerance.")

        } else {

          sf::st_geometry(network)[network$comid == start_comid] <-
            suppressWarnings({

              if(!requireNamespace("lwgeom")) {
                stop("lwgeom required to trim flowlines to a specific measure.")
              }

              lwgeom::st_linesubstring(
                sf::st_cast(sf::st_geometry(l), "LINESTRING"), f, t)

            })
        }

      } else {
        if("measure" %in% names(out_features)) {
          # remove if reachcode is same as start and
          # measure is greater for upstream
          # measure is less for downstream

          out_features <- dplyr::filter(
            out_features,
            if(grepl("UM|UT", mode)) {
              .data$reachcode != event$REACHCODE |
                (.data$reachcode == event$REACHCODE & .data$measure > event$REACH_meas)
            } else {
              .data$reachcode != event$REACHCODE |
                (.data$reachcode == event$REACHCODE & .data$measure < event$REACH_meas)
            })

        }
      }
    } else {

      warning("trim_start ignored for comid start")

    }

  }

  if(!is.null(out_features)) {
    return(out_features)
  } else {
    return(network)
  }

}
