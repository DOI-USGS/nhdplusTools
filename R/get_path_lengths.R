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

    return(select(paths, -any_of("network_distance_km")))
  }

  select(paths, all_of(c(ID_1 = "id_1", ID_2 = "id_2", "network_distance_km")))
}
