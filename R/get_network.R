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
#' @importFrom hydroloom navigate_hydro_network
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
  navigate_hydro_network(network, comid, "UT", distance)
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

  network <- align_nhdplus_names(network)

  main_us <- filter(network, .data$COMID %in%
                      navigate_hydro_network(network, comid, "UM", distance))

  if(sort) { main_us <-  arrange(main_us, .data$Hydroseq) }
  if(!include) {  main_us = filter(main_us, .data$COMID != comid) }

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
get_DM <- function(network, comid, distance = NULL, sort = FALSE, include = TRUE) {

  network <- align_nhdplus_names(network)

  main_ds <- filter(network, .data$COMID %in%
                      navigate_hydro_network(network, comid, "DM", distance))

  if(sort){ main_ds <- arrange(main_ds, desc(.data$Hydroseq)) }
  if(!include){ main_ds <- filter(main_ds, .data$COMID != comid) }

  return(main_ds$COMID)
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

  navigate_hydro_network(network, comid, "DD", distance)

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

    start <- floor(start)
    start_comid <- start

  }

  if(is.null(start) | length(start_comid) == 0) {
    warning("something went wrong trying to find the start comid, this won't work")
    return(NULL)
  }

  # in the case that we don't have a network, we need to get it
  # from the NLDI and web services.
  if(is.null(network)) {

    network <- navigate_nldi(list(featureSource = "comid",
                                  featureID = start_comid),
                             mode, "flowlines", distance_km)

    network <- network[names(network) != "origin"][[1]]$nhdplus_comid

    if(is.null(network)) {
      warning("Something went wrong getting network data.")
      return(NULL)
    }

    network <- subset_nhdplus(as.integer(network),
                              nhdplus_data = "download",
                              status = TRUE,
                              flowline_only = TRUE)

    if(is.null(network)) return(NULL)

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
        search_radius <- units::set_units(0.001, "degrees")
      } else {
        search_radius <- units::set_units(100, "m")
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

    if(!(is.numeric(start) && start %% 1 == 0)) {
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
