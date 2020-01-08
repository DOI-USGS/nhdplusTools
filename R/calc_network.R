#' Total Drainage Area
#' @description Calculates total drainage area given a dendritic
#' network and incremental areas.
#' @param catchment_area data.frame with ID, toID, and area columns.
#' @return numeric with total area.
#' @importFrom igraph graph_from_data_frame topo_sort
#' @importFrom dplyr select left_join
#' @export
#' @examples
#' library(dplyr)
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' catchment_area <- prepare_nhdplus(walker_flowline, 0, 0,
#'                              purge_non_dendritic = FALSE, warn = FALSE) %>%
#'   left_join(select(walker_flowline, COMID, AreaSqKM), by = "COMID") %>%
#'   select(ID = COMID, toID = toCOMID, area = AreaSqKM)
#'
#' new_da <- calculate_total_drainage_area(catchment_area)
#'
#' catchment_area$totda <- new_da
#' catchment_area$nhdptotda <- walker_flowline$TotDASqKM
#'
#' mean(abs(catchment_area$totda - catchment_area$nhdptotda))
#' max(abs(catchment_area$totda - catchment_area$nhdptotda))
#'

calculate_total_drainage_area <- function(catchment_area) {

  return(accumulate_downstream(catchment_area, "area"))

}

#' Calculate Arbolate Sum
#' @description Calculates arbolate sum given a dendritic
#' network and incremental lengths. Arbolate sum is the total length
#' of all upstream flowlines.
#' @param catchment_area data.frame with ID, toID, and length columns.
#' @return numeric with arbolate sum.
#' @export
#' @examples
#' library(dplyr)
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' catchment_length <- prepare_nhdplus(walker_flowline, 0, 0,
#'                              purge_non_dendritic = FALSE, warn = FALSE) %>%
#'   left_join(select(walker_flowline, COMID), by = "COMID") %>%
#'   select(ID = COMID, toID = toCOMID, length = LENGTHKM)
#'
#' arb_sum <- calculate_arbolate_sum(catchment_length)
#'
#' catchment_length$arb_sum <- arb_sum
#' catchment_length$nhd_arb_sum <- walker_flowline$ArbolateSu
#'
#' mean(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum))
#' max(abs(catchment_length$arb_sum - catchment_length$nhd_arb_sum))
#'

calculate_arbolate_sum <- function(catchment_area) {

  return(accumulate_downstream(catchment_area, "length"))

}

#' @importFrom dplyr select left_join ungroup distinct
#' @noRd
#'
accumulate_downstream <- function(dat_fram, var) {

  cat_order <- select(dat_fram, .data$ID)

  dat_fram[["toID"]][which(is.na(dat_fram[["toID"]]))] <- 0

  sorted <- get_sorted(dat_fram)

  sorted <- sorted[sorted != "0" & sorted %in% as.character(cat_order$ID)]

  dat_fram <- left_join(data.frame(ID = as.numeric(sorted[!sorted == "NA"])),
                        dat_fram, by = "ID")

  dat_fram[["toID_row"]] <- match(dat_fram[["toID"]], dat_fram[["ID"]])

  var_out <- dat_fram[[var]]
  toid_row <- dat_fram[["toID_row"]]

  for(cat in 1:length(var_out)) {
    var_out[toid_row[cat]] <- var_out[toid_row[cat]] + var_out[cat]
  }

  dat_fram[[var]] <- var_out

  dat_fram <- distinct(left_join(cat_order, dat_fram, by = "ID"))

  return(dat_fram[[var]])
}

#' Get Level Paths
#' @description Calculates level paths using the stream-leveling approach of
#' NHD and NHDPlus. In addition to a levelpath identifier, a topological sort and
#' levelpath outlet identifier is provided in output. If arbolate sum is provided in
#' the weight column, this will match the behavior of NHDPlus. Any numeric value can be
#' included in this column and the largest value will be followed when no nameID is available.
#' @param flowline data.frame with ID, toID, nameID, and weight columns.
#' @param status boolean if status updates should be printed.
#' @return data.frame with ID, outletID, topo_sort, and levelpath collumns.
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
get_levelpaths <- function(flowline, status = FALSE) {

  flowline <- check_names(flowline, "get_levelpaths")

  flowline[["toID"]][which(is.na(flowline[["toID"]]))] <- 0

  flowline[["nameID"]][is.na(flowline[["nameID"]])] <- " " # NHDPlusHR uses NA for empty names.
  flowline[["nameID"]][flowline[["nameID"]] == "-1"] <- " "

  sorted <- get_sorted(flowline)

  sorted <- sorted[sorted != 0]

  flowline <- left_join(data.frame(ID = as.numeric(sorted[!sorted == "NA"])),
                        flowline, by = "ID")

  flowline[["topo_sort"]] <- seq(nrow(flowline), 1)
  flowline[["levelpath"]] <- rep(0, nrow(flowline))

  flc <- flowline
  diff = 1
  checker <- 0
  while(nrow(flc) > 0 & checker < 10000000) {
    tail_ind <- which(flc$topo_sort == min(flc$topo_sort))
    tailID <- flc$ID[tail_ind]
    sortID <- flowline$topo_sort[tail_ind]

    pathIDs <- get_path(flc, tailID)

    flowline <- mutate(flowline,
                       levelpath = ifelse(.data$ID %in% pathIDs,
                                          sortID, .data$levelpath))
    flc <- filter(flc, !.data$ID %in% pathIDs)
    checker <- checker + 1

    if(status && checker %% 1000 == 0) {
      message(paste(nrow(flc), "of", nrow(flowline), "remaining."))
    }
  }

  outlets <- flowline %>%
    group_by(.data$levelpath) %>%
    filter(topo_sort == min(topo_sort)) %>%
    ungroup() %>%
    select(outletID = .data$ID, .data$levelpath)

  flowline <- left_join(flowline, outlets, by = "levelpath")

  return(select(flowline, .data$ID, .data$outletID, .data$topo_sort, .data$levelpath))
}

#' get level path
#' @noRd
#' @description Recursively walks up a network following the nameID
#' or, if no nameID exists, maximum weight column.
#' @param flowline data.frame with ID, toID, nameID and weight columns.
#' @param tailID integer or numeric ID of outlet catchment.
#'
get_path <- function(flowline, tailID) {
  # May be more than 1
  from_inds <- which(flowline$toID == tailID)
  if(length(from_inds) > 1) { # need to find dominant
    ind <- which(flowline$ID == tailID)
    next_tails <- flowline[from_inds, ]

    if(any(next_tails$nameID != " ")) { # If any of the candidates are named.
      next_tails <- # pick the matching one.
        filter(next_tails, .data$nameID == flowline$nameID[ind])

      if(nrow(next_tails) > 1) {
        next_tails <- # pick the named one.
          filter(next_tails, .data$nameID != " ")
      }
    }

    if(nrow(next_tails) != 1) { # If the above didn't result in one row.
      next_tails <- # use weighting
        filter(flowline[from_inds, ], .data$weight == max(.data$weight))
    }

    if(length(next_tails$ID) > 1) {
      next_tails <- next_tails[1, ]
    }
    c(tailID, get_path(flowline, next_tails$ID))
  } else if(length(from_inds) == 1) {
    c(tailID, get_path(flowline, flowline$ID[from_inds]))
  } else {
    return(tailID)
  }
}

#' @title Get Streamorder
#' @description Applies a topological sort and calculates strahler stream order.
#' Algorithm: If more than one upstream flowpath has an order equal to the
#' maximum upstream order then the downstream flowpath is assigned the maximum
#' upstream order plus one. Otherwise it is assigned the max upstream order.
#' @param fl data.frame with dendritic ID and toID columns.
#' @return numeric stream order in same order as input
#' @importFrom dplyr left_join select
#' @export
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE)
#'
#' test_flowline <- data.frame(
#'   ID = test_flowline$COMID,
#'   toID = test_flowline$toCOMID)
#'
#' (order <- get_streamorder(test_flowline))
#'
#' walker_flowline$order <- order
#'
#' plot(sf::st_geometry(walker_flowline), lwd = walker_flowline$order, col = "blue")
#'
get_streamorder <- function(fl) {
  check_names(fl, "get_streamorder")

  o_sort <- select(fl, .data$ID)

  fl[["toID"]][which(is.na(fl[["toID"]]))] <- 0

  sorted <- get_sorted(fl)

  fl <- left_join(data.frame(ID = as.numeric(sorted[!sorted == "0"])),
                  fl, by = "ID")

  ID <- as.numeric(fl$ID)
  toID <- as.numeric(fl$toID)
  order <- rep(1, length(ID))

  for(i in seq(1, length(ID))) {
    from <- toID == ID[i]
    if(any(from, na.rm = TRUE)) {
      orders <- order[from]

      m <- max(orders)

      if(length(orders[orders == m]) > 1) {
        order[i] <- m + 1
      } else {
        order[i] <- m
      }
    }
  }

  distinct(left_join(o_sort, data.frame(ID = ID, order = order), by = "ID"))[["order"]]

}

#' @title Get Pfafstetter Codes (Experimental)
#' @description Determines Pfafstetter codes for a dendritic network with
#' total drainage area, levelpath, and topo_sort attributes.
#' @param fl sf data.frame with ID, toID, totda, outletID, topo_sort,
#' and levelpath attributes.
#' @param max_level integer number of pfaf levels to attempt to calculate.
#' If the network doesn't have resolution to support the desired level,
#' unexpected behavior may occur.
#' @param status boolean print status or not
#' @return data.frame with ID and pfaf columns.
#' @export
#' @importFrom sf st_drop_geometry
#' @importFrom methods is
#' @examples
#' library(dplyr)
#' source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))
#' hr_flowline <- align_nhdplus_names(hr_data$NHDFlowline)
#'
#' fl <- prepare_nhdplus(hr_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
#'   left_join(select(hr_flowline, COMID, AreaSqKM), by = "COMID") %>%
#'   sf::st_sf() %>%
#'   select(ID = COMID, toID = toCOMID, area = AreaSqKM)
#'
#' fl$nameID = ""
#' fl$totda <- calculate_total_drainage_area(sf::st_set_geometry(fl, NULL))
#' fl <- left_join(fl, get_levelpaths(rename(sf::st_set_geometry(fl, NULL),
#'                                    weight = totda)), by = "ID")
#'
#' pfaf <- get_pfaf(fl, max_level = 3)
#'
#' fl <- left_join(fl, pfaf, by = "ID")
#'
#' plot(fl["pf_level_3"], lwd = 2)
#'
#' pfaf <- get_pfaf(fl, max_level = 4)
#'
#' hr_catchment <- left_join(hr_data$NHDPlusCatchment, pfaf, by = c("FEATUREID" = "ID"))
#'
#' colors <- data.frame(pf_level_4 = unique(hr_catchment$pf_level_4),
#'                      color = sample(terrain.colors(length(unique(hr_catchment$pf_level_4)))),
#'                     stringsAsFactors = FALSE)
#' hr_catchment <- left_join(hr_catchment, colors, by = "pf_level_4")
#' plot(hr_catchment["color"], border = NA, reset = FALSE)
#' plot(sf::st_geometry(hr_flowline), col = "blue", add = TRUE)
#'
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' fl <- prepare_nhdplus(walker_flowline, 0, 0, purge_non_dendritic = FALSE, warn = FALSE) %>%
#'   left_join(select(walker_flowline, COMID, AreaSqKM), by = "COMID") %>%
#'   sf::st_sf() %>%
#'   select(ID = COMID, toID = toCOMID, area = AreaSqKM)
#'
#' fl$nameID = ""
#' fl$totda <- calculate_total_drainage_area(sf::st_set_geometry(fl, NULL))
#' fl <- left_join(fl, get_levelpaths(rename(sf::st_set_geometry(fl, NULL),
#'                                    weight = totda)), by = "ID")
#'
#' pfaf <- get_pfaf(fl, max_level = 2)
#'
#' fl <- left_join(fl, pfaf, by = "ID")
#'
#' plot(fl["pf_level_2"], lwd = 2)
#'
get_pfaf <- function(fl, max_level = 2, status = FALSE) {
  if(is(fl, "sf")) fl <- st_drop_geometry(fl)
  check_names(fl, "get_pfaf")

  mainstem_levelpath <- unique(fl$levelpath[fl$topo_sort == min(fl$topo_sort)])

  mainstem <- fl[fl$levelpath == mainstem_levelpath, ]

  pfaf <- do.call(rbind, get_pfaf_9(fl, mainstem, max_level, status = status))

  return(cleanup_pfaf(pfaf))
}

#' @noRd
#' @importFrom dplyr arrange left_join
#' @importFrom methods is
get_pfaf_9 <- function(fl, mainstem, max_level, pre_pfaf = 0, assigned = NA, status = FALSE) {

  if((pre_pfaf / 10^(max_level-1)) > 1) return()

  if(status && ((pre_pfaf - 1111) %% 1000) == 0) {
    message(paste("On level:", pre_pfaf - 1111))
  }
  # Get all tributary outlets that go to the passed mainstem.
  trib_outlets <- fl[fl$toID %in% mainstem$ID &
                   fl$levelpath != mainstem$levelpath[1], ]

  # Exclude those that have already been defined as drainage basin outlets
  if(is(assigned, "data.frame")) {
    trib_outlets <- trib_outlets[!trib_outlets$ID %in%
                                   assigned$members[(assigned$pfaf %% 2) == 0], ]
  }

  if(length(mainstem$ID) == 1 && nrow(trib_outlets) == 0) {
    return()
  }

  # Get the top 4 tributaries (or less) by total drainage area and arrange along the mainstem
  area_filter <- (if(nrow(trib_outlets) >= 4) 4 else nrow(trib_outlets))
  area_filter <- sort(trib_outlets$totda, decreasing = TRUE)[area_filter]
  t4_tribs <- trib_outlets[trib_outlets$totda >= area_filter, ]
  t4_tribs <- left_join(t4_tribs, select(fl, .data$ID, ms_ts = .data$topo_sort),
                        by = c("toID" = "ID")) %>% arrange(.data$ms_ts)

  # t4_tribs <- t4_tribs[t4_tribs$ms_ts < max(mainstem$topo_sort),]

  ms_inter <- lapply(seq_len(5), function(x, ms, ts) {
    if(x > (length(ts) + 1)) return(data.frame(ID = NA_real_))
    if(x == 1) {
      ms <- ms[ms$topo_sort <= ts[x], ]
    } else if(x == 5 | x == (length(ts) + 1)) {
      ms <- ms[ms$topo_sort > ts[x - 1], ]
    } else {
      ms <- ms[ms$topo_sort > ts[x - 1] & ms$topo_sort <= ts[x], ]
    }
    if(nrow(ms) > 0) ms$p_id <- c(1, 3, 5, 7, 9)[x]
    ms
  }, ms = mainstem, ts = t4_tribs$ms_ts)

  out <- data.frame(p_id = c(1:9))
  out[["members"]] <- list(ms_inter[[1]]$ID, fl$ID[fl$outletID == t4_tribs$outletID[1]],
                           ms_inter[[2]]$ID, fl$ID[fl$outletID == t4_tribs$outletID[2]],
                           ms_inter[[3]]$ID, fl$ID[fl$outletID == t4_tribs$outletID[3]],
                           ms_inter[[4]]$ID, fl$ID[fl$outletID == t4_tribs$outletID[4]],
                           ms_inter[[5]]$ID)
  out[["pfaf"]] <- out$p_id + pre_pfaf * 10

  if(all(sapply(out$members, function(x) all(is.na(x))))) out$members[[1]] <- mainstem$ID
  out <- tidyr::unnest(out, cols = c(.data$members))
  out <- list(out[!is.na(out$members), ])

  if(nrow(out[[1]]) == 0 | all(out[[1]]$members %in% mainstem$ID)) {
    return(out)
  }

  c(out, unlist(lapply(c(1:9), apply_fun,
                       p9 = out[[1]], fl = fl, max_level = max_level, status = status),
                recursive = FALSE))
}

apply_fun <- function(p, p9, fl, max_level, status) {
  p_sub <- p9[p9$p_id == p, ]
  ms_ids <- p_sub$members
  pre_pfaf <- unique(p_sub$pfaf)
  mainstem <- fl[fl$ID %in% ms_ids, ]

  if(length(pre_pfaf) > 0) {
    get_pfaf_9(fl, mainstem, max_level, pre_pfaf = pre_pfaf, assigned = p9, status = status)
  } else {
    NULL
  }
}

#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select
#' @noRd
cleanup_pfaf <- function(pfaf) {
  # Add level number
  pfaf$level <- ceiling(log10(pfaf$pfaf + 0.01))
  pfaf <- select(pfaf, -.data$p_id, ID = .data$members)

  pfaf$uid <- 1:nrow(pfaf)

  # Deduplicate problem tributaries
  remove <- do.call(c, lapply(1:length(unique(pfaf$level)), function(l, pfaf) {
    check <- pfaf[pfaf$level == l, ]
    check <- dplyr::group_by(check, .data$ID)
    check <- dplyr::filter(check, n() > 1 & .data$pfaf < max(.data$pfaf))$uid
  }, pfaf = pfaf))

  pfaf <- pivot_wider(select(pfaf[!pfaf$uid %in% remove, ], -.data$uid),
                      id_cols = .data$ID, names_from = "level",
                      names_prefix = "pf_level_", values_from = .data$pfaf)

  # replace NAs with known values.
  for(i in 3:ncol(pfaf)) {
    pfaf[, i][is.na(pfaf[, i]) & !is.na(pfaf[, (i - 1)]), ] <-
      1 + (pfaf[, (i - 1)][is.na(pfaf[, i]) & !is.na(pfaf[, (i - 1)]), ] * 10)
  }

  for(i in (ncol(pfaf) - 1):2) {
    pfaf[, i][is.na(pfaf[, i]), ] <-
      floor(pfaf[, (i + 1)][is.na(pfaf[, i]), ] / 10)
  }

  return(pfaf)
}

#' @noRd
#' @param flowline data.frame if an identifier and to identifier in the
#' first and second columns.
#' @importFrom igraph topo_sort graph_from_data_frame
get_sorted <- function(flowline) {
  names(topo_sort(graph_from_data_frame(flowline,
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



