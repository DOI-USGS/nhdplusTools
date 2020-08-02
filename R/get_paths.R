#' Get Level Paths
#' @description Calculates level paths using the stream-leveling approach of
#' NHD and NHDPlus. In addition to a levelpath identifier, a topological sort and
#' levelpath outlet identifier is provided in output. If arbolate sum is provided in
#' the weight column, this will match the behavior of NHDPlus. Any numeric value can be
#' included in this column and the largest value will be followed when no nameID is available.
#' @param x data.frame with ID, toID, nameID, and weight columns.
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
get_levelpaths <- function(x, status = FALSE) {

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

  flc <- x
  diff = 1
  checker <- 0
  while(nrow(flc) > 0 & checker < 10000000) {
    tail_ind <- which(flc$topo_sort == min(flc$topo_sort))
    tailID <- flc$ID[tail_ind]

    x_tail_ind <- which(x$topo_sort == min(flc$topo_sort))
    sortID <- x$topo_sort[x_tail_ind]

    pathIDs <- get_path(flc, tailID)

    x <- mutate(x,
                levelpath = ifelse(.data$ID %in% pathIDs,
                                   sortID, .data$levelpath))
    flc <- filter(flc, !.data$ID %in% pathIDs)
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
#'
get_path <- function(x, tailID) {
  # May be more than 1
  from_inds <- which(x$toID == tailID)
  if(length(from_inds) > 1) { # need to find dominant
    ind <- which(x$ID == tailID)
    next_tails <- x[from_inds, ]

    if(any(next_tails$nameID != " ")) { # If any of the candidates are named.
      next_tails <- # pick the matching one.
        filter(next_tails, .data$nameID == x$nameID[ind])

      if(nrow(next_tails) > 1) {
        next_tails <- # pick the named one.
          filter(next_tails, .data$nameID != " ")
      }
    }

    if(nrow(next_tails) != 1) { # If the above didn't result in one row.
      next_tails <- # use weighting
        filter(x[from_inds, ], .data$weight == max(.data$weight))
    }

    if(length(next_tails$ID) > 1) {
      next_tails <- next_tails[1, ]
    }
    c(tailID, get_path(x, next_tails$ID))
  } else if(length(from_inds) == 1) {
    c(tailID, get_path(x, x$ID[from_inds]))
  } else {
    return(tailID)
  }
}

#' @noRd
#' @param x data.frame if an identifier and to identifier in the
#' first and second columns.
#' @importFrom igraph topo_sort graph_from_data_frame
get_sorted <- function(x) {
  names(topo_sort(graph_from_data_frame(x,
                                        directed = TRUE),
                  mode = "out"))
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
  x <- left_join(data.frame(ID = sorted[length(sorted):1], stringsAsFactors = FALSE),
                 x, by = "ID")

  x <- x[!is.na(x$ID), ]

  id <- x$ID
  toid <- x$toID
  le <- x$length
  leo <- rep(0, length(le))

  for(i in seq_len(length(id))) {
    if(!is.na(tid <- toid[i])) {
      r <- which(id == tid)

      leo[i] <- le[r] + leo[r]
    }
  }
  return(data.frame(ID = id, pathlength = leo,
                    stringsAsFactors = FALSE))
}
