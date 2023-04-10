#' Get Level Paths (DEPRECATED)
#' @description Calculates level paths using the stream-leveling approach of
#' NHD and NHDPlus. In addition to a levelpath identifier, a topological sort and
#' levelpath outlet identifier is provided in output. If arbolate sum is provided in
#' the weight column, this will match the behavior of NHDPlus. Any numeric value can be
#' included in this column and the largest value will be followed when no nameID is available.
#' @param x data.frame with ID, toID, nameID, and weight columns.
#' @param override_factor numeric factor to use to override nameID.
#' If `weight` is `numeric_factor` times larger on a path, it will be followed
#' regardless of the nameID indication.
#' @param status boolean if status updates should be printed.
#' @param cores numeric number of cores to use in initial path ranking calculations.
#' @return data.frame with ID, outletID, topo_sort, and levelpath columns.
#' See details for more info.
#' @importFrom hydroloom add_levelpaths hy
#' @details
#' \enumerate{
#'   \item levelpath provides an identifier for the collection of flowlines
#'   that make up the single mainstem flowpath of a total upstream aggregate catchment.
#'   \item outletID is the catchment ID (COMID in the case of NHDPlus) for the catchment
#'   at the outlet of the levelpath the catchment is part of.
#'   \item topo_sort is similar to Hydroseq in NHDPlus in that large topo_sort values
#'   are upstream of small topo_sort values. Note that there are many valid topological
#'   sort orders of a directed graph.
#' }
#' @export
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE)
#'
#' test_flowline <- data.frame(
#'   ID = test_flowline$COMID,
#'   toID = test_flowline$toCOMID,
#'   nameID = walker_flowline$GNIS_ID,
#'   weight = walker_flowline$ArbolateSu,
#'   stringsAsFactors = FALSE)
#'
#' get_levelpaths(test_flowline)
#'
#'
get_levelpaths <- function(x, override_factor = NULL, status = FALSE, cores = NULL) {

  warning("get_levelpaths is deprecated in favor of add_levelpaths in the hydroloom package.")

  if(!is.null(cores)) {
    if(inherits(cores, "cluster")) stop("passing a cluster object no longer supported")
    message("the future plan is being modified and will be changed back on exit")
    oplan <- future::plan(future::multisession, workers = cores)
    on.exit(future::plan(oplan), add = TRUE)
  }

  get_levelpaths_internal(x, override_factor)

}

get_levelpaths_internal <- function(x, override_factor) {
  x <- check_names(drop_geometry(x), "get_levelpaths")

  x <- hy(select(x, all_of(c(id = "ID", toid = "toID", "nameID", "weight"))))

  x <- add_levelpaths(x, weight_attribute = "weight", name_attribute = "nameID",
                      override_factor = override_factor)

  return(as.data.frame(select(x, all_of(c("ID" = "id", "outletID" = "levelpath_outlet_id",
                                          "topo_sort" = "topo_sort", "levelpath" = "levelpath")))))
}

#' @importFrom hydroloom make_fromids
get_fromids <- function(index_ids, return_list = FALSE) {

  index_ids <- select(index_ids, indid = "id", toindid = "toid")

  make_fromids(index_ids, return_list)

}

#' Get Sorted Network
#' @description given a tree with an id and and toid in the
#' first and second columns, returns a sorted and potentially
#' split set of output.
#'
#' Can also be used as a very fast implementation of upstream
#' with tributaries navigation. The full network from each
#' outlet is returned in sorted order.
#'
#' @export
#' @param x data.frame with an identifier and to identifier in the
#' first and second columns.
#' @param split logical if TRUE, the result will be split into
#' independent networks identified by the id of their outlet. The
#' outlet id of each independent network is added as a "terminalID"
#' attribute.
#' @param outlets same as id in x; if specified only the network
#' emanating from these outlets will be considered and returned.
#' @return data.frame containing a topologically sorted version
#' of the requested network and optionally a terminal id.
#' @importFrom hydroloom sort_network
#' @examples
#' source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#'
#' fpath <- get_tocomid(
#'   dplyr::select(new_hope_flowline, COMID, FromNode, ToNode, Divergence, FTYPE,
#'                 AreaSqKM, LENGTHKM, GNIS_ID)
#' )
#'
#' head(fpath <- get_sorted(fpath, split = TRUE))
#'
#' fpath['sort_order'] <- 1:nrow(fpath)
#'
#' plot(fpath['sort_order'])
#'
get_sorted <- function(x, split = FALSE, outlets = NULL) {

  orig_names <- names(x)[1:2]

  names(x)[1:2] <- c("id", "toid")

  x <- sort_network(x, split, outlets)

  names(x)[1:2] <- orig_names

  if(split)
    names(x)[names(x) == "terminal_id"] <- "terminalID"

  return(x)
}

#' @noRd
#' @importFrom dplyr all_of
#' @importFrom hydroloom make_index_ids
get_index_ids <- function(x,
                          innames = c("comid", "tocomid"),
                          outnames = c("id", "toid")) {

  if(!all(innames %in% names(x))) {
    stop(paste(paste(innames, collapse = ", "), "must be in input or provided."))
  }

  out <- select(x, all_of(innames))

  names(out) <- c("id", "toid")

  out <- make_index_ids(out)

  out <- hydroloom:::unnest(out$to_list, "toindid")

  out <- select(out, "indid", "toindid")

  names(out) <- outnames

  out

}

#' @noRd
#' @param x data.frame with ID and toID
#' @param rev logical if TRUE (default) top down
#' @importFrom tidyr replace_na
topo_sort_network <- function(x, reverse = TRUE) {

  if(any(x$ID == 0)) stop("ID 0 must not be present. It is used as the outlet ID.")

  x[["toID"]] <- replace_na(x[["toID"]], 0)

  x <- get_sorted(x[, c("ID", "toID", names(x)[!names(x) %in% c("ID", "toID")])])

  if(reverse) {
    x <- x[nrow(x):1, ]
  }

  x[!is.na(x$ID), ]

}

#' Get Terminal ID (DEPRECATED)
#' @description Get the ID of the basin outlet for each flowline.
#' This function has been deprecated in favor of get_sorted.
#' @param x two column data.frame with IDs and toIDs. Names are ignored.
#' @param outlets IDs of outlet flowlines
#' @export
#' @return data.frame containing the terminal ID for each outlet
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' fl <- dplyr::select(prepare_nhdplus(walker_flowline, 0, 0),
#'                     ID = COMID, toID = toCOMID)
#'
#' outlet <- fl$ID[which(!fl$toID %in% fl$ID)]
#'
#' get_terminal(fl, outlet)
#'
get_terminal <- function(x, outlets) {

  message("Deprecated, use get_sorted.")

  ordered_id <- x$ID

  x <- get_sorted(x, split = TRUE, outlets = outlets)

  left_join(data.frame(ID = ordered_id),
                   x[c("ID", "terminalID")], by = "ID")
}

#' Get Path Length
#' @description Generates the main path length to a basin's terminal path.
#' @param x data.frame with ID, toID, length columns.
#' @importFrom hydroloom add_pathlength
#' @export
#' @return data.frame containing pathlength for each ID
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' fl <- dplyr::select(prepare_nhdplus(walker_flowline, 0, 0),
#'                     ID = COMID, toID = toCOMID, length = LENGTHKM)
#'
#' get_pathlength(fl)
#'
get_pathlength <- function(x) {

  x <- select(drop_geometry(x), all_of(c("id" = "ID", "toid" = "toID",
                                         "length_km" = "length")))

  x <- add_pathlength(x)

  return(data.frame(ID = x$id, pathlength = x$pathlength_km))
}
