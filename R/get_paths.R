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
#'   sort orders of a directed graph. The sort order output by this function is generated
#'   using `igraph::topo_sort`.
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
get_levelpaths <- function(x, override_factor = NULL, status = FALSE) {

  x <- check_names(x, "get_levelpaths")

  x[["toID"]][which(is.na(x[["toID"]]))] <- 0

  x[["nameID"]][is.na(x[["nameID"]])] <- " " # NHDPlusHR uses NA for empty names.
  x[["nameID"]][x[["nameID"]] == "-1"] <- " "

  sorted <- get_sorted(x)

  sorted <- sorted[sorted != 0]

  x <- left_join(data.frame(ID = as.numeric(sorted[!sorted == "NA"])),
                 x, by = "ID")

  x[["topo_sort"]] <- seq(nrow(x), 1)
  x[["levelpath"]] <- rep(0, nrow(x))

  x <- x %>% # get downstream name ID added
    left_join(select(x, ID, ds_nameID = nameID), by = c("toID" = "ID")) %>%
    # if it's na, we need it to be an empty string
    mutate(ds_nameID = ifelse(is.na(ds_nameID), " ", ds_nameID)) %>%
    # group on toID so we can operate on upstream choices
    group_by(toID) %>%
    # reweight sets up ranked upstream paths
    dplyr::group_modify(reweight, override_factor = override_factor)%>%
    ungroup() %>%
    select(ID, toID, topo_sort, levelpath, weight)

  diff = 1
  checker <- 0
  done <- 0

  x <- arrange(x, topo_sort)

  topo_sort <- x$topo_sort

  matcher <- sapply(x$ID, function(id, x) {
    which(x$toID == id)
  }, x = x)

  names(matcher) <- x$ID

  while(done < nrow(x) & checker < 10000000) {
    tail_topo <- min(topo_sort, na.rm = TRUE)

    pathIDs <- get_path(x = x,
                         tailID = x$ID[x$topo_sort == tail_topo],
                         matcher = matcher,
                         status = status)

    reset <- x$ID %in% pathIDs

    x$levelpath[reset] <- tail_topo

    n_reset <- sum(reset)

    done <- done + n_reset

    topo_sort[reset] <- rep(NA, n_reset)

    checker <- checker + 1

    if(status && checker %% 1000 == 0) {
      message(paste(nrow(flc), "of", nrow(x), "remaining."))
    }
  }

  outlets <- x %>%
    group_by(.data$levelpath) %>%
    filter(topo_sort == min(topo_sort)) %>%
    ungroup() %>%
    select(outletID = .data$ID, .data$levelpath)

  x <- left_join(x, outlets, by = "levelpath")

  return(select(x, .data$ID, .data$outletID, .data$topo_sort, .data$levelpath))
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
        sub <- arrange(x[x$nameID == cur_name, ], desc(weight))

        out[1:nrow(sub), ] <- sub

        rank <- rank + nrow(sub)

        x <- x[!x$ID %in% sub$ID, ]
      }

      if(rank <= total) {
        if(any(x$nameID != " ")) {
          sub <-
            arrange(x[x$nameID != " ", ], desc(weight))

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
      out$weight <- out$weight * override_factor
    }

    if(rank < nrow(out)) {
      out[rank:nrow(out), ] <- arrange(x, desc(weight))
    }

    if(!is.null(override_factor)) {
      out <- arrange(out, desc(weight))
    }

    x <- out

  }

  x$weight <- seq(nrow(x), 1)

  x
}

.datatable.aware <- TRUE

#' @noRd
#' @param x data.frame if an identifier and to identifier in the
#' first and second columns.
#' @importFrom igraph topo_sort graph_from_data_frame
get_sorted <- function(x) {
  x <- graph_from_data_frame(x, directed = TRUE)
  x <- topo_sort(x, mode = "out")
  names(x)
}

#' Get Terminal ID
#' @description Get the ID of the basin outlet for each flowline.
#' @param x two column data.frame with IDs and toIDs. Names are ignored.
#' @param outlets IDs of outlet flowlines
#' @export
#' @importFrom igraph dfs graph_from_data_frame V
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr unnest
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

  g <- graph_from_data_frame(x, directed = TRUE)

  basins <- lapply(outlets, function(o, g) {
    v <- V(g)[which(names(igraph::V(g)) == o)]

    b <- dfs(g, v, neimode = "in", unreachable = FALSE)

    b <- names(b$order)
    b[!is.na(b) & b !=0]

  }, g = g)

  basin_df <- data.frame(terminalID = outlets, stringsAsFactors = FALSE)
  basin_df[["ID"]] <- basins

  basin_df <- distinct(unnest(basin_df, cols = "ID"))

  if(is.integer(x[[1, 1]])) {
    basin_df[["ID"]] <- as.integer(basin_df[["ID"]])
  }

  if(is.numeric(x[[1, 1]]) & !is.integer(x[[1, 1]])) {
    basin_df[["ID"]] <- as.numeric(basin_df[["ID"]])
  }

  return(basin_df)
}

#' Get path length
#' @description Generates the main path length to a basin's
#' terminal path.
#' @param x data.frame with ID, toID, length columns.
#' @importFrom dplyr arrange
#' @importFrom methods as
#' @export
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' fl <- dplyr::select(prepare_nhdplus(walker_flowline, 0, 0),
#'                     ID = COMID, toID = toCOMID, length = LENGTHKM)
#'
#' get_pathlength(fl)
#'
get_pathlength <- function(x) {

  sorted <- as(get_sorted(x[, c("ID", "toID")]),
               class(x$ID))

  sorted <- sorted[length(sorted):1]

  sorted <- sorted[!is.na(sorted)]

  order <- match(sorted, x$ID)

  x <- x[order, ]

  x <- x[!is.na(x$ID), ]

  id <- x$ID
  toid <- x$toID
  le <- x$length
  leo <- rep(0, length(le))

  toids <- match(toid, id)

  for(i in seq_len(length(id))) {
    if(!is.na(tid <- toid[i])) {

      leo[i] <- le[toids[i]] + leo[toids[i]]

    }
  }
  return(data.frame(ID = id, pathlength = leo))
}
