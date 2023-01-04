#' Get Path Members (DEPRECATED)
#' @description Given a network and set of IDs, finds paths between all
#' identified flowpath outlets. This algorithm finds members between outlets
#' regardless of flow direction.
#' @inheritParams get_path_lengths
#' @return list of lists containing flowpath identifiers along path that connect
#' outlets.
#' @export
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' fline <- walker_flowline
#'
#' outlets <- c(5329303, 5329357, 5329317, 5329365, 5329435, 5329817)
#'
#' # Add toCOMID
#' fline <- nhdplusTools::get_tocomid(fline, add = TRUE)
#'
#' fl <- dplyr::select(fline, ID = comid, toID = tocomid, lengthkm)
#'
#' get_path_members(outlets, fl)
#'
get_path_members <- function(outlets, network, cores = 1, status = FALSE) {

  get_paths_internal(outlets, network, cores, status, lengths = FALSE)

}

#' Get Path Lengths (DEPRECATED)
#' @description Given a network and set of IDs, finds path lengths between all
#' identified flowpath outlets. This algorithm finds distance between outlets
#' regardless of flow direction.
#' @param outlets vector of IDs from data.frame
#' @param network data.frame with ID, toID, and lengthkm attributes.
#' @param cores integer number of cores to use for parallel computation.
#' @param status logical print status and progress bars?
#' @return data.frame containing the distance between pairs of network outlets.
#' For a network with one terminal outlet, the data.frame will have
#' `nrow(network)^2` rows.
#' @importFrom pbapply pboptions pbapply pblapply
#' @importFrom utils combn
#' @export
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' fline <- walker_flowline
#'
#' outlets <- c(5329303, 5329357, 5329317, 5329365, 5329435, 5329817)
#'
#' # Add toCOMID
#' fline <- nhdplusTools::get_tocomid(fline, add = TRUE)
#'
#' fl <- dplyr::select(fline, ID = comid, toID = tocomid, lengthkm)
#'
#' get_path_lengths(outlets, fl)
#'
get_path_lengths <- function(outlets, network, cores = 1, status = FALSE) {

  get_paths_internal(outlets, network, cores, status, lengths = TRUE)
}

#' @importFrom hydroloom navigate_connected_paths
get_paths_internal <- function(outlets, network, cores = 1, status = FALSE,
                               lengths = FALSE) {

  warning("get_paths* is deprecated in favor of get_paths in the hydroloom package.")

  if(!is.null(cores) && cores != 1) {
    message("the future plan is being modified and will be changed back on exit")
    oplan <- future::plan(future::multisession, workers = cores)
    on.exit(future::plan(oplan), add = TRUE)
  }

  paths <- navigate_connected_paths(network, outlets)

  if(!lengths) {

    return(select(paths, -network_distance_km))
  }

  select(paths, all_of(c(ID_1 = "id_1", ID_2 = "id_2", "network_distance_km")))
}

# utility function
get_fl <- function(hl, net) {
  if(hl$reach_meas == 100) {
    filter(net,
           .data$reachcode == hl$reachcode &
             .data$tomeas == hl$reach_meas)
  } else {
    filter(net,
           .data$reachcode == hl$reachcode &
             .data$frommeas <= hl$reach_meas &
             .data$tomeas > hl$reach_meas)
  }
}

#' Get Partial Flowline Length
#' @param hl list containing a hydrologic location with names reachcode
#' and reach_meas.
#' @param net data.frame containing a flowpath network with reachcode,
#' frommeas, tomeas, and lengthkm attributes. Not required if `fl` is
#' provided.
#' @param fl data.frame containing one flowline that corresponds to the
#' reachcode and measure of `hl`. Not required if `hl` is provided.
#' @description Finds the upstream and downstream lengths along a given
#' flowpath (flowline in nhdplus terminology). Internally, the function
#' rescales the reach measure to a flowpath measure and applies that
#' rescaled measure to the length of the flowpath.
#' @return list containing `up` and `dn` elements with numeric length in
#' km.
#' @export
#' @examples
#'
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' hydro_location <- list(comid = 5329339,
#'                        reachcode = "18050005000078",
#'                        reach_meas = 30)
#'
#' (pl <- get_partial_length(hydro_location, walker_flowline))
#'
#' hydro_location <- sf::st_sf(hydro_location,
#'                             geom = nhdplusTools::get_hydro_location(data.frame(hydro_location),
#'                                                                     walker_flowline))
#'
#' net <- navigate_network(hydro_location,
#'                         mode = "DM", network = walker_flowline,
#'                         distance_km = 4, trim_start = TRUE)
#'
#' plot(sf::st_geometry(walker_flowline[walker_flowline$COMID == hydro_location$comid,]))
#' plot(sf::st_geometry(hydro_location), add = TRUE)
#' plot(sf::st_geometry(net), add = TRUE, col = "blue", lwd = 2)
#'
#' sf::st_length(net)
#' pl$dn
#'

get_partial_length <- function(hl, net = NULL, fl = NULL) {

  if(is.null(fl)) {

    if(is.null(net)) {
      stop("network must be supplied if flowline is null")
    }

    net <- check_names(net, "get_partial_length", tolower = TRUE)
    fl <- get_fl(hl, net)
  }

  if(nrow(fl) == 0) {
    stop("hydrolocation not found in network provided")
  }

  meas <- rescale_measures(measure = hl$reach_meas,
                           from = fl$frommeas,
                           to = fl$tomeas) / 100

  list(dn = fl$lengthkm * meas,
       up = fl$lengthkm * (1 - meas))
}
