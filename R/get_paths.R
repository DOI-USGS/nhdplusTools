#' Get Level Paths
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

  x <- check_names(drop_geometry(x), "get_levelpaths")

  x[["toID"]] <- tidyr::replace_na(x[["toID"]], 0)

  x[["nameID"]][is.na(x[["nameID"]])] <- " " # NHDPlusHR uses NA for empty names.
  x[["nameID"]][x[["nameID"]] == "-1"] <- " "

  x <- get_sorted(x)

  x[["topo_sort"]] <- seq(nrow(x), 1)
  x[["levelpath"]] <- rep(0, nrow(x))

  stop_cluster <- FALSE
  cl <- NULL

  if(!is.null(cores)) {
    cl <- get_cl(cores)
    if(!inherits(cores, "cluster")) {
      on.exit(parallel::stopCluster(cl))
    }
  }

  x <- x %>% # get downstream name ID added
    left_join(drop_geometry(select(x, .data$ID, ds_nameID = .data$nameID)),
              by = c("toID" = "ID")) %>%
    # if it's na, we need it to be an empty string
    mutate(ds_nameID = ifelse(is.na(.data$ds_nameID),
                              " ", .data$ds_nameID)) %>%
    # group on toID so we can operate on upstream choices
    group_by(.data$toID) %>%
    dplyr::group_split()

  # reweight sets up ranked upstream paths
  if(!is.null(cl)) {
    x <- parallel::parLapply(cl, x, reweight, override_factor = override_factor)
  } else {
    x <- lapply(x, reweight, override_factor = override_factor)
  }

  x <- x %>%
    bind_rows() %>%
    select(.data$ID, .data$toID, .data$topo_sort,
           .data$levelpath, .data$weight)

  diff = 1
  checker <- 0
  done <- 0

  x <- dplyr::arrange(x, .data$topo_sort)

  topo_sort <- x$topo_sort

  if(!is.null(cl)) {
    matcher <- parallel::parSapply(cl = cl, X = x$ID,
                                   FUN = function(id, df) {
      which(x$toID == id)
    }, df = x)
  } else {
    matcher <- sapply(x$ID, function(id, df) {
      which(x$toID == id)
    }, df = x)
  }

  names(matcher) <- x$ID

  x$done <- rep(FALSE, nrow(x))

  outlets <- filter(x, .data$toID == 0)

  while(done < nrow(x) & checker < 10000000) {
    tail_topo <- outlets$topo_sort

    pathIDs <- if(nrow(outlets) == 1) {
      list(par_get_path(as.list(outlets), x, matcher, status))
    } else if(!is.null(cl)) {
      parallel::parApply(cl = cl,
                         outlets[sample(nrow(outlets)), ], 1, par_get_path,
                         x_in = x, matcher = matcher,
                         status = status)
    } else {
      apply(outlets, 1, par_get_path,
             x_in = x, matcher = matcher,
             status = status)
    }

    pathIDs <- do.call(rbind, pathIDs)

    reset <- match(pathIDs$ID, x$ID)

    x$levelpath[reset] <- pathIDs$levelpath

    n_reset <- length(reset)

    done <- done + n_reset

    x$done[reset] <- rep(TRUE, n_reset)

    outlets <- x[x$toID %in% pathIDs$ID &
                   !x$ID %in% pathIDs$ID, ]

    checker <- checker + 1

    if(status && checker %% 1000 == 0) {
      message(paste(done, "of", nrow(x), "remaining."))
    }
  }

  outlets <- x %>%
    group_by(.data$levelpath) %>%
    filter(.data$topo_sort == min(.data$topo_sort)) %>%
    ungroup() %>%
    select(outletID = .data$ID, .data$levelpath)

  x <- left_join(x, outlets, by = "levelpath")

  return(select(x, .data$ID, .data$outletID, .data$topo_sort, .data$levelpath))
}

par_get_path <- function(outlet, x_in, matcher, status) {
  out <- get_path(x = x_in, tailID = outlet[names(outlet) == "ID"][[1]],
                  matcher = matcher, status = status)
  data.frame(ID = out, levelpath = rep(outlet[names(outlet) == "topo_sort"][[1]], length(out)))
}

#' get level path
#' @noRd
#' @description Recursively walks up a network following the nameID
#' or, if no nameID exists, maximum weight column.
#' @param x data.frame with ID, toID, nameID and weight columns.
#' @param tailID integer or numeric ID of outlet catchment.
#' @param override_factor numeric follow weight if this many times larger
#' @param status print status?
#'
get_path <- function(x, tailID, matcher, status) {

  keep_going <- TRUE
  tracker <- rep(NA, nrow(x))
  counter <- 1

  toID <- NULL

  while(keep_going) {
    tryCatch({
      next_tails <- x[matcher[[as.character(tailID)]], ]

      if(nrow(next_tails) > 1) {

        next_tails <- next_tails[next_tails$weight == max(next_tails$weight), ]

      }

      if(nrow(next_tails) == 0) {

        keep_going <- FALSE

      }

      if(tailID %in% tracker) stop(paste0("loop at", tailID))

      tracker[counter] <- tailID

      counter <- counter + 1

      tailID <- next_tails$ID

      if(status && counter %% 1000 == 0) message(paste("long mainstem", counter))
    }, error = function(e) {
      stop(paste0("Error with outlet tailID ", tailID, "\n",
                  "Original error was \n", e))
    })

  }


  return(tracker[!is.na(tracker)])
}

reweight <- function(x, ..., override_factor) {

  if(nrow(x) > 1) {

    cur_name <- x$ds_nameID[1]

    max_weight <- max(x$weight)

    rank <- 1

    total <- nrow(x)

    out <- x

    if(any(x$nameID != " ")) { # If any of the candidates are named.
      if(cur_name != " " & cur_name %in% x$nameID) {
        sub <- dplyr::arrange(x[x$nameID == cur_name, ], desc(.data$weight))

        out[1:nrow(sub), ] <- sub

        rank <- rank + nrow(sub)

        x <- x[!x$ID %in% sub$ID, ]
      }

      if(rank <= total) {
        if(any(x$nameID != " ")) {
          sub <-
            dplyr::arrange(x[x$nameID != " ", ], desc(.data$weight))

          out[rank:(rank + nrow(sub) - 1), ] <- sub

          rank <- rank + nrow(sub)

          x <- x[!x$ID %in% sub$ID, ]

        }

        if(rank <= total) {
          out[rank:total, ] <- x
        }

      }
    }

    if(!is.null(override_factor)) {
      out <- dplyr::mutate(out, weight = ifelse(.data$nameID == .data$ds_nameID,
                                                .data$weight * override_factor,
                                                .data$weight))
    }

    if(rank < nrow(out)) {
      out[rank:nrow(out), ] <- dplyr::arrange(x, desc(.data$weight))
    }

    if(!is.null(override_factor)) {
      out <- dplyr::arrange(out, desc(.data$weight))
    }

    x <- out

  }

  x$weight <- seq(nrow(x), 1)

  x
}

.datatable.aware <- TRUE
. <- fromid <- id <- NULL

get_fromids <- function(index_ids, return_list = FALSE) {
  index_ids <- data.table::as.data.table(index_ids)

  froms <- merge(
    index_ids[,list(id)],
    data.table::setnames(index_ids, c("toid", "id"), c("id", "fromid")),
    by = "id", all.x = TRUE
  )

  froms <- froms[,list(froms = list(c(fromid))), by = id]

  froms_l <- lengths(froms$froms)
  max_from <- max(froms_l)

  # Convert list to matrix with NA fill
  froms_m <- as.matrix(sapply(froms$froms, '[', seq(max_from)))

  # NAs should be length 0
  froms_l[is.na(froms_m[1, ])] <- 0

  if(return_list) return(list(froms = froms_m, lengths = froms_l,
                              froms_list = froms))

  return(list(froms = froms_m, lengths = froms_l))

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

  class_x <- class(x)

  x <- as.data.frame(x)

  # nrow to reuse
  n <- nrow(x)

  # index for fast traversal
  index_ids <- get_index_ids(x, innames = names(x)[1:2])

  if(!is.null(outlets)) {
    starts <- which(x[, 1] %in% outlets)
  } else {
    # All the start nodes
    starts <- which(index_ids$toid == 0)
  }

  froms <- get_fromids(index_ids)

  # Some vectors to track results
  to_visit <- out <- rep(0, n)

  if(split) {
    set <- out
    out_list <- rep(list(list()), length(starts))
  }

  # output order tracker
  o <- 1
  set_id <- 1

  for(s in starts) {

    # Set up the starting node
    node <- s

    # within set node tracker for split = TRUE
    n <- 1
    # v is a pointer into the to_visit vector
    v <- 1

    while(v > 0) {

      # track the order that nodes were visited
      out[node] <- o
      # increment to the next node
      o <- o + 1

      if(split) {
        set[n] <- node
        n <- n + 1
      }

      # does nothing if froms_l[node] == 0

      for(from in seq_len(froms$lengths[node])) {
        if(!is.na(next_node <- froms$froms[from, node])) {
          # Add the next node to visit to the tracking vector
          to_visit[v] <- next_node
          v <- v + 1
        }}

      # go to the last element added in to_visit
      v <- v - 1
      node <- to_visit[v]

    }

    if(split) {
      out_list[[set_id]] <- x[set[1:(n - 1)], 1]
      set_id <- set_id + 1
    }
  }

  if(split) names(out_list) <- x[starts, 1]

  ### rewrites x into the correct order. ###
  if(o - 1 != nrow(x)) {
    x <- x[which(out != 0), ]
    out <- out[out != 0]
  }

  x <- x[order(out)[(o-1):1], ]

  if(split) {

    # this is only two columns
    ids <- methods::as(names(out_list), class(x[1, 1]))

    out_list <- data.frame(ids = ids) %>%
      mutate(set = out_list) %>%
      tidyr::unnest_longer(.data$set)

    names(out_list) <- c("terminalID", names(x)[1])

    ### adds grouping terminalID to x ###
    x <- dplyr::left_join(x, out_list, by = names(x)[1])

  }

  if("sf" %in% class_x) {
    try(x <- sf::st_sf(x))
  }

  return(x)
}

#' @noRd
#' @param x data.frame with ID and toID (names not important)
#' in the first and second columns.
#' An "id" and "toid" (lowercase) will be added.
get_index_ids <- function(x,
                          innames = c("comid", "tocomid"),
                          outnames = c("id", "toid")) {

  if(!all(innames %in% names(x))) {
    stop(paste(paste(innames, collapse = ", "), "must be in input or provided."))
  }

  out <- data.frame(id = seq(1, nrow(x)))

  out["toid"] <- match(x[[innames[2]]], x[[innames[1]]], nomatch = 0)

  names(out) <- outnames

  out

}

#' @noRd
#' @param x data.frame with ID and toID
#' @param rev logical if TRUE (default) top down
topo_sort_network <- function(x, reverse = TRUE) {

  if(any(x$ID == 0)) stop("ID 0 must not be present. It is used as the outlet ID.")

  x[["toID"]] <- tidyr::replace_na(x[["toID"]], 0)

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

  dplyr::left_join(data.frame(ID = ordered_id),
                   x[c("ID", "terminalID")], by = "ID")
}

#' Get Path Length
#' @description Generates the main path length to a basin's
#' terminal path.
#' @param x data.frame with ID, toID, length columns.
#' @importFrom dplyr arrange
#' @importFrom methods as
#' @export
#' @return data.frame containing levelpaths for each ID
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' fl <- dplyr::select(prepare_nhdplus(walker_flowline, 0, 0),
#'                     ID = COMID, toID = toCOMID, length = LENGTHKM)
#'
#' get_pathlength(fl)
#'
get_pathlength <- function(x) {

  x <- topo_sort_network(x)

  id <- x$ID
  toid <- x$toID
  le <- x$length
  leo <- rep(0, length(le))

  toids <- match(toid, id)

  for(i in seq_len(length(id))) {
    if((tid <- toid[i]) != 0) {

      leo[i] <- le[toids[i]] + leo[toids[i]]

    }
  }
  return(data.frame(ID = id, pathlength = leo))
}
