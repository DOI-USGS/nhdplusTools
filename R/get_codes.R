#' @title Get Streamorder
#' @description Applies a topological sort and calculates strahler stream order.
#' Algorithm: If more than one upstream flowpath has an order equal to the
#' maximum upstream order then the downstream flowpath is assigned the maximum
#' upstream order plus one. Otherwise it is assigned the max upstream order.
#' @param x data.frame with dendritic ID and toID columns.
#' @param status logical show progress update messages?
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
get_streamorder <- function(x, status = TRUE) {
  check_names(x, "get_streamorder")

  o_sort <- select(x, "ID")

  x[["toID"]] <- tidyr::replace_na(x[["toID"]], 0)

  # First sort so we have upstream first and outlets last.
  x <- get_sorted(x)

  # Now generate a working index against the sorted data.
  index_ids <- get_index_ids(x, innames = c("ID", "toID"))

  # Find fromids from the working index.
  # columns of the included matrix correspond to the index ids.
  # rows of the matrix correspond to adjacent upstream ids
  froms <- get_fromids(index_ids)

  # will fill in order as we go in this
  order <- rep(1, nrow(x))

  for(i in seq_len(nrow(x))) {

    # nothing to do if nothing upstream
    if((l <- froms$lengths[i]) > 0) {

      # these are the upstream orders
      orders <- order[froms$froms[1:l,i]]

      # Need the max upstream order for this work
      m <- max(orders)

      # the core stream order algorithm.
      # if more than one upstream order is the same
      # as the max upstream order, increment by one.
      # otherwise use the max upstream order.
      if(length(orders[orders == m]) > 1) {
        order[i] <- m + 1
      } else {
        order[i] <- m
      }

    }
  }

  left_join(o_sort,
            bind_cols(x, data.frame(order = order)),
            by = "ID")[["order"]]

}

#' @title Get Streamlevel
#' @description Applies a topological sort and calculates stream level.
#' Algorithm: Terminal level paths are assigned level 1 (see note 1).
#' Paths that terminate at a level 1 are assigned level 2. This pattern is
#' repeated until no paths remain.
#'
#' If a TRUE/FALSE coastal attribute is included, coastal terminal paths
#' begin at 1 and internal terminal paths begin at 4 as is implemented by
#' the NHD stream leveling rules.
#'
#' @param x data.frame with levelpathi, dnlevelpat, and optionally a
#' coastal flag. If no coastal flag is included, all terminal paths are
#' assumed to be coastal.
#'
#' @return numeric stream order in same order as input
#' @export
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' test_flowline <- data.frame(
#'  levelpathi = walker_flowline$LevelPathI,
#'  dnlevelpat = walker_flowline$DnLevelPat)
#'
#'  test_flowline$dnlevelpat[1] <- 0
#'
#' (level <- get_streamlevel(test_flowline))
#'
#' walker_flowline$level <- level
#'
#' plot(sf::st_geometry(walker_flowline), lwd = walker_flowline$level, col = "blue")
#'
#' test_flowline$coastal <- rep(FALSE, nrow(test_flowline))
#' (level <- get_streamlevel(test_flowline))
#'
#' test_flowline$coastal[!test_flowline$dnlevelpat %in% test_flowline$levelpathi] <- TRUE
#' (level <- get_streamlevel(test_flowline))
#'
get_streamlevel <- function(x) {

  check_names(x, "get_streamlevel")

  x$dnlevelpat[is.na(x$dnlevelpat)] <- 0

  l <- x %>%
    dplyr::filter(.data$levelpathi != .data$dnlevelpat) %>%
    dplyr::rename(ID = "levelpathi",
                  toID = "dnlevelpat") %>%
    topo_sort_network(reverse = TRUE) %>%
    dplyr::distinct()

  l$level <- rep(0, nrow(l))

  l$level[!l$toID %in% l$ID] <- 1

  if("coastal" %in% names(l)) {
    l$level[l$level == 1 & !l$coastal] <- 4
  }

  id <- l$ID
  toid <- l$toID
  level <- l$level

  toids <- match(toid, id)

  # walk the network from bottom path up
  for(i in seq_len(length(id))) {
    if(!is.na(toids[i])) {

      level[i] <- # level at current
        level[toids[i]] + 1 # level of downstream + 1

    }
  }

  left_join(x, data.frame(levelpathi = id, out = level), by = "levelpathi")$out

}

#' @title Get Pfafstetter Codes (Experimental)
#' @description Determines Pfafstetter codes for a dendritic network with
#' total drainage area, levelpath, and topo_sort attributes.
#' @param x sf data.frame with ID, toID, totda, outletID, topo_sort,
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
#' \donttest{
#' library(dplyr)
#' source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))
#' hr_flowline <- align_nhdplus_names(hr_data$NHDFlowline)
#'
#' fl <-  select(hr_flowline, COMID, AreaSqKM) %>%
#'   right_join(prepare_nhdplus(hr_flowline, 0, 0,
#'                              purge_non_dendritic = FALSE,
#'                              warn = FALSE),
#'              by = "COMID") %>%
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
#' fl <- select(walker_flowline, COMID, AreaSqKM) %>%
#'   right_join(prepare_nhdplus(walker_flowline, 0, 0,
#'                             purge_non_dendritic = FALSE, warn = FALSE),
#'             by = "COMID") %>%
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
#' }
get_pfaf <- function(x, max_level = 2, status = FALSE) {
  if(is(x, "sf")) x <- st_drop_geometry(x)
  check_names(x, "get_pfaf")

  mainstem_levelpath <- unique(x$levelpath[x$topo_sort == min(x$topo_sort)])

  mainstem <- x[x$levelpath == mainstem_levelpath, ]

  pfaf <- do.call(rbind, get_pfaf_9(x, mainstem, max_level, status = status))

  return(cleanup_pfaf(pfaf))
}

#' @noRd
#' @importFrom dplyr arrange left_join
#' @importFrom methods is
get_pfaf_9 <- function(x, mainstem, max_level, pre_pfaf = 0, assigned = NA, status = FALSE) {

  if((pre_pfaf / 10^(max_level-1)) > 1) return()

  if(status && ((pre_pfaf - 1111) %% 1000) == 0) {
    message(paste("On level:", pre_pfaf - 1111))
  }
  # Get all tributary outlets that go to the passed mainstem.
  trib_outlets <- x[x$toID %in% mainstem$ID &
                      x$levelpath != mainstem$levelpath[1], ]

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
  t4_tribs <- left_join(t4_tribs, select(x, "ID", ms_ts = "topo_sort"),
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
  out[["members"]] <- list(ms_inter[[1]]$ID, x$ID[x$outletID == t4_tribs$outletID[1]],
                           ms_inter[[2]]$ID, x$ID[x$outletID == t4_tribs$outletID[2]],
                           ms_inter[[3]]$ID, x$ID[x$outletID == t4_tribs$outletID[3]],
                           ms_inter[[4]]$ID, x$ID[x$outletID == t4_tribs$outletID[4]],
                           ms_inter[[5]]$ID)
  out[["pfaf"]] <- out$p_id + pre_pfaf * 10

  if(all(sapply(out$members, function(x) all(is.na(x))))) out$members[[1]] <- mainstem$ID
  out <- tidyr::unnest(out, cols = c("members"))
  out <- list(out[!is.na(out$members), ])

  if(nrow(out[[1]]) == 0 | all(out[[1]]$members %in% mainstem$ID)) {
    return(out)
  }

  c(out, unlist(lapply(c(1:9), apply_fun,
                       p9 = out[[1]], x = x, max_level = max_level, status = status),
                recursive = FALSE))
}

apply_fun <- function(p, p9, x, max_level, status) {
  p_sub <- p9[p9$p_id == p, ]
  ms_ids <- p_sub$members
  pre_pfaf <- unique(p_sub$pfaf)
  mainstem <- x[x$ID %in% ms_ids, ]

  if(length(pre_pfaf) > 0) {
    get_pfaf_9(x, mainstem, max_level, pre_pfaf = pre_pfaf, assigned = p9, status = status)
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
  pfaf <- select(pfaf, -"p_id", ID = "members")

  pfaf$uid <- 1:nrow(pfaf)

  # Deduplicate problem tributaries
  remove <- do.call(c, lapply(1:length(unique(pfaf$level)), function(l, pfaf) {
    check <- pfaf[pfaf$level == l, ]
    check <- dplyr::group_by(check, .data$ID)
    check <- dplyr::filter(check, n() > 1 & .data$pfaf < max(.data$pfaf))$uid
  }, pfaf = pfaf))

  pfaf <- pivot_wider(select(pfaf[!pfaf$uid %in% remove, ], -"uid"),
                      id_cols = "ID", names_from = "level",
                      names_prefix = "pf_level_", values_from = "pfaf")

  # replace NAs with known values.
  for(i in 3:ncol(pfaf)) {
    pfaf[, i][is.na(pfaf[, i, drop = TRUE]) & !is.na(pfaf[, (i - 1), drop = TRUE]), ] <-
      1 + (pfaf[, (i - 1)][is.na(pfaf[, i, drop = TRUE]) & !is.na(pfaf[, (i - 1), drop = TRUE]), ] * 10)
  }

  for(i in (ncol(pfaf) - 1):2) {
    pfaf[, i][is.na(pfaf[, i, drop = TRUE]), ] <-
      floor(pfaf[, (i + 1)][is.na(pfaf[, i, drop = TRUE]), ] / 10)
  }

  return(pfaf)
}
