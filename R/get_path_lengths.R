#' Get Path Lengths
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
#' #' Add toCOMID
#' fline[["toCOMID"]] <- nhdplusTools::get_tocomid(fline)
#'
#' fl <- dplyr::select(fline, ID = COMID, toID = toCOMID, lengthkm = LENGTHKM)
#'
#' path_lengths <- get_path_lengths(outlets, fl)
#'
#' outlet_geo <- sf::st_sf(
#'   dplyr::left_join(data.frame(ID = outlets),
#'                    dplyr::select(fline, ID = COMID), by = "ID))
#' sf::st_geometry(outlet_geo) <- sf::st_geometry(nhdplusTools::get_node(outlet_geo))
#'
#' plot(sf::st_geometry(fl))
#' plot(sf::st_geometry(outlet_geo), add = TRUE)
#'
get_path_lengths <- function(outlets, network, cores = 1, status = FALSE) {

  stopifnot(is.vector(outlets))

  if(!status) {
    pbopts <- pboptions(type = "none")
    on.exit(pboptions(pbopts), add = TRUE)
  }

  network <- drop_geometry(network)

  index <- get_index_ids(select(network, .data$ID, .data$toID),
                         innames = c("ID", "toID"))

  index <- cbind(select(network, .data$ID), index)

  get_dwn <- function(ID, toid) {
    next_dn <- toid[ID]
    if(next_dn == 0) {
      return(ID)
    } else {
      return(c(ID, get_dwn(next_dn, toid)))
    }
  }

  ID_match <- match(outlets, index$ID)

  if(status)
    message("Finding all downstream paths.")

  all_dn <- pbapply::pblapply(index$id[ID_match], function(x, toid) {
    out <- get_dwn(x, toid)
    if((lo <- length(out)) > 1) {
      out[2:lo] # don't want to include the starting flowpath
    } else {
      out[1]
    }
  }, toid = index$toid)

  if(cores > 1) {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  } else {
    cl <- NULL
  }

  if(status)
    message("Finding all connected pairs.")

  get_path <- function(p, all_dn) {
    x <- all_dn[[p[1]]]
    y <- all_dn[[p[2]]]

    if(length(x) == 1) # if one end is a terminal
      return(list(x = integer(0), y = y))

    if(length(y) == 1)
      return(list(x = x, y = integer(0)))

    if(tail(x, 1) == tail(y, 1))
      return(list(x = x[!x %in% y], y = y[!y %in% x]))

    list()
  }

  pairs <- t(combn(length(ID_match), 2))
  paths <- pbapply::pbapply(pairs, 1, get_path, all_dn = all_dn, cl = cl)

  lengthkm <- select(left_join(index,
                               select(network, .data$ID, .data$lengthkm),
                               by = "ID"),
                     .data$id, .data$lengthkm)

  connected_paths <- paths[lengths(paths) > 0]

  if(status)
    message("Summing length of all connected pairs.")

  get_length <- function(p, lengthkm)
    sum(lengthkm$lengthkm[p[[1]]], lengthkm$lengthkm[p[[2]]])

  path_lengths <- pbapply::pblapply(connected_paths, get_length, lengthkm = lengthkm)

  path_lengths <- cbind(as.data.frame(matrix(ID_match[pairs[lengths(paths) > 0,]],
                                             ncol = 2)),
                        data.frame(length = as.numeric(path_lengths)))

  names(path_lengths) <- c("id_1", "id_2", "network_distance_km")

  path_lengths <- left_join(path_lengths,
                            select(index, ID_1 = .data$ID, .data$id),
                            by = c("id_1" = "id")) %>%
    left_join(select(index, ID_2 = .data$ID, .data$id),
              by = c("id_2" = "id"))

  select(path_lengths, -.data$id_1, -.data$id_2)
}
