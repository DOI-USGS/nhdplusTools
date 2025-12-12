#' DEPRECATED: Total Drainage Area
#' @description Calculates total drainage area given a dendritic
#' network and incremental areas.
#' @param x data.frame with ID, toID, and area columns.
#' @return numeric with total area.
#' @importFrom dplyr select left_join
#' @importFrom hydroloom accumulate_downstream
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

  warning("calculate_total_drainage_area() is deprecated. Please switch to hydroloom equivalent.")

  return(accumulate_downstream(x, "area"))

}

#' DEPRECATED: Calculate Arbolate Sum
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

  warning("calculate_arbolate_sum is deprecated. Please switch to hydroloom equivalent.")

  return(accumulate_downstream(x, "length"))

}
