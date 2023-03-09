#' Total Drainage Area
#' @description Calculates total drainage area given a dendritic
#' network and incremental areas.
#' @param x data.frame with ID, toID, and area columns.
#' @return numeric with total area.
#' @importFrom dplyr select left_join
#' @export
#' @examples
#' library(dplyr)
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' catchment_area <- select(walker_flowline, COMID, AreaSqKM) %>%
#'   right_join(prepare_nhdplus(walker_flowline, 0, 0,
#'                             purge_non_dendritic = FALSE, warn = FALSE), by = "COMID") %>%
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

calculate_total_drainage_area <- function(x) {

  return(accumulate_downstream(x, "area"))

}

#' Calculate Arbolate Sum
#' @description Calculates arbolate sum given a dendritic
#' network and incremental lengths. Arbolate sum is the total length
#' of all upstream flowlines.
#' @param x data.frame with ID, toID, and length columns.
#' @return numeric with arbolate sum.
#' @export
#' @examples
#' library(dplyr)
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' catchment_length <- select(walker_flowline, COMID, AreaSqKM) %>%
#'   right_join(prepare_nhdplus(walker_flowline, 0, 0,
#'                             purge_non_dendritic = FALSE, warn = FALSE), by = "COMID") %>%
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

calculate_arbolate_sum <- function(x) {

  return(accumulate_downstream(x, "length"))

}

#' @importFrom dplyr select left_join ungroup distinct
#' @importFrom rlang .data
#' @noRd
#'
accumulate_downstream <- function(x, var) {

  try(x <- st_drop_geometry(x), silent = TRUE)

  cat_order <- select(x, "ID")

  x[["toID"]] <- tidyr::replace_na(x[["toID"]], 0)

  x <- get_sorted(x)

  x[["toID_row"]] <- match(x[["toID"]], x[["ID"]])

  var_out <- x[[var]]

  if(any(is.na(x[[var]]))) {
    warning("NA values found, accumulation math may fail.")
  }

  toid_row <- x[["toID_row"]]

  for(cat in 1:length(var_out)) {
    var_out[toid_row[cat]] <- var_out[toid_row[cat]] + var_out[cat]
  }

  x[[var]] <- var_out

  x <- distinct(left_join(cat_order, x, by = "ID"))

  return(x[[var]])
}
