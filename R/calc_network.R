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

#' @importFrom igraph graph_from_data_frame topo_sort
#' @importFrom dplyr select left_join ungroup
#' @noRd
#'
accumulate_downstream <- function(dat_fram, var) {

  cat_order <- select(dat_fram, .data$ID)

  dat_fram[["toID"]][which(is.na(dat_fram[["toID"]]))] <- 0

  sorted <- names(topo_sort(graph_from_data_frame(dat_fram,
                                                  directed = TRUE),
                            mode = "out"))

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

  dat_fram <- left_join(cat_order, dat_fram, by = "ID")

  return(dat_fram[[var]])
}

#' Calculate Level Paths
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
#' calculate_levelpaths(test_flowline)
#'
#'
calculate_levelpaths <- function(flowline, status = FALSE) {

  flowline <- check_names(flowline, "calculate_levelpaths")

  flowline[["toID"]][which(is.na(flowline[["toID"]]))] <- 0

  flowline[["nameID"]][is.na(flowline[["nameID"]])] <- " " # NHDPlusHR uses NA for empty names.
  flowline[["nameID"]][flowline[["nameID"]] == "-1"] <- " "

  sorted <- names(topo_sort(graph_from_data_frame(flowline,
                                                  directed = TRUE),
                            mode = "out"))

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
