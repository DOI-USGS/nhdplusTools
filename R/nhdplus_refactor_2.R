#' @title nhdplus refactor 2
#' @description refactors nhdplud flowlines
#' @import dplyr
#' @export
#'

nhdplus_refactor_2 <- function(nhdplus_flines,
                             split_flines_meters,
                             split_flines_cores,
                             collapse_flines_meters,
                             collapse_flines_mainstem_meters,
                             out_collapsed,
                             out_reconciled) {

  flines <- st_set_geometry(nhdplus_flines, NULL) %>%
    prepare_nhdplus(0, 0) %>%
    inner_join(select(nhdplus_flines, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(5070) %>%
    split_flowlines(split_flines_meters, split_flines_cores)

  rm(nhdplus_flines)

  message("flowlines split complete, collapsing")

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

  collapsed_flines %>%
    inner_join(select(flines, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326) %>%
    sf::st_write(out_collapsed, layer_options = "OVERWRITE=YES")

  message("collapse complete, out collapse written to disk, reconciling")

  collapsed <- reconcile_collapsed_flowlines(collapsed_flines, select(flines, COMID), id = "COMID")

  collapsed$member_COMID <- unlist(lapply(collapsed$member_COMID, function(x) paste(x, collapse = ",")))

  sf::st_write(sf::st_transform(collapsed, 4326), out_reconciled, layer_options = "OVERWRITE=YES")
}
