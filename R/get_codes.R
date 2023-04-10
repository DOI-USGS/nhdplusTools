#' @title Get Streamorder
#' @description Applies a topological sort and calculates strahler stream order.
#' Algorithm: If more than one upstream flowpath has an order equal to the
#' maximum upstream order then the downstream flowpath is assigned the maximum
#' upstream order plus one. Otherwise it is assigned the max upstream order.
#' @param x data.frame with dendritic ID and toID columns.
#' @param status logical show progress update messages?
#' @return numeric stream order in same order as input
#' @importFrom dplyr left_join select
#' @importFrom hydroloom add_streamorder
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

  add_streamorder(x, status)$stream_order

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
#' @importFrom hydroloom add_streamlevel
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

  coastal <- NULL
  coastal <- if("coastal" %in% names(x)) "coastal"

  add_streamlevel(x, coastal)$stream_level

}

#' @title Get Pfafstetter Codes (DEPRECATED)
#' @description Determines Pfafstetter codes for a dendritic network with
#' total drainage area, levelpath, and topo_sort attributes.
#' @param x sf data.frame with ID, toID, totda, outletID, topo_sort,
#' and levelpath attributes.
#' @param max_level integer number of pfaf levels to attempt to calculate.
#' If the network doesn't have resolution to support the desired level,
#' unexpected behavior may occur.
#' @param status boolean print status or not
#' @importFrom hydroloom add_pfafstetter
#' @return data.frame with ID and pfaf columns.
#' @export
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

  warning("get_pfaf is deprecated, please use hydroloom")

  x <- drop_geometry(x)

  check_names(x, "get_pfaf")

  x <- select(x, all_of(get("get_pfaf_attributes",
                            envir = nhdplusTools_env)))

  x <- add_pfafstetter(x, max_level, status)

  x <- select(x, -all_of(c("toID", "totda", "outletID", "topo_sort", "levelpath")))

  x <- filter(x, !is.na(.data$pf_level_1))

  return(x)
}
