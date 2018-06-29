#' @title nhdplus refactor
#' @description refactors nhdplud flowlines
#' @param nhdplus_flines data.frame nhdplus flowloines
#' @param split_flines_meters numeric split threshold in meters
#' @param split_flines_cores numeric number of cores to use for split function
#' @param collapse_flines_meters numeric collapse threshold in meters
#' @param collapse_flines_mainstem_meters numeric collapse threshold for mainstems in meters
#' @param out_collapsed character geopackage path to write collapsed to (one layer per gpkg)
#' @param out_reconciled character geopackage path to write reconciled to (one layer per gpkg)
#' @param three_pass boolean perform three collapse passes or just one
#' @importFrom dplyr inner_join
#' @import sf
#' @export
#'

nhdplus_refactor <- function(nhdplus_flines,
                             split_flines_meters,
                             split_flines_cores,
                             collapse_flines_meters,
                             collapse_flines_mainstem_meters,
                             out_collapsed,
                             out_reconciled,
                             three_pass = FALSE) {

  if("FTYPE" %in% names(nhdplus_flines)) {
    nhdplus_flines <- st_set_geometry(nhdplus_flines, NULL) %>%
      prepare_nhdplus(0, 0) %>%
      inner_join(select(nhdplus_flines, COMID), by = "COMID") %>%
      st_as_sf()
  }

  flines <- nhdplus_flines %>%
    st_cast("LINESTRING") %>%
    st_transform(5070) %>%
    split_flowlines(split_flines_meters, split_flines_cores)

  rm(nhdplus_flines)

  message("flowlines split complete, collapsing")

  if(three_pass) {
    collapsed_flines <- collapse_flowlines(st_set_geometry(flines, NULL),
                                           (0.25*collapse_flines_meters/1000),
                                           TRUE,
                                           (0.25*collapse_flines_mainstem_meters/1000))

    collapsed_flines <- collapse_flowlines(collapsed_flines,
                                           (0.5*collapse_flines_meters/1000),
                                           TRUE,
                                           (0.5*collapse_flines_mainstem_meters/1000))

    collapsed_flines <- collapse_flowlines(collapsed_flines,
                                           (collapse_flines_meters/1000),
                                           TRUE,
                                           (collapse_flines_mainstem_meters/1000))
  } else {
    collapsed_flines <- collapse_flowlines(st_set_geometry(flines, NULL),
                                           (collapse_flines_meters/1000),
                                           TRUE,
                                           (collapse_flines_mainstem_meters/1000))
  }

  collapsed_flines %>%
    inner_join(select(flines, COMID), by = "COMID") %>%
    st_as_sf() %>%
    st_transform(4326) %>%
    st_write(out_collapsed, layer_options = "OVERWRITE=YES", quiet = TRUE)

  message("collapse complete, out collapse written to disk, reconciling")

  collapsed <- reconcile_collapsed_flowlines(collapsed_flines, select(flines, COMID), id = "COMID")

  collapsed$member_COMID <- unlist(lapply(collapsed$member_COMID, function(x) paste(x, collapse = ",")))

  st_write(st_transform(collapsed, 4326), out_reconciled, layer_options = "OVERWRITE=YES", quiet = TRUE)
}
