#' Collapse Catchment Boundaries
#' @description Collapses catchments according to a set of input outlet catchments.
#' @export
#' @importFrom igraph graph_from_data_frame topo_sort tail_of incident_edges V bfs incident_edges head_of
#' @importFrom sf st_cast st_union st_geometry st_sfc st_sf st_crs
#' @importFrom dplyr filter
#' @examples
#'
collapse_catchments <- function(fline_rec, cat_rec, outlet_cats, inlet_cats = NULL) {
  fline_rec$toID[is.na(fline_rec$toID)] <- 0
  suppressWarnings(cat_net <- graph_from_data_frame(fline_rec, directed = TRUE))
  cat_net_sort_verts <- topo_sort(cat_net)
  outlet_verts <- tail_of(cat_net, outlet_cats)
  outlet_edge <- incident_edges(cat_net,
                                cat_net_sort_verts[cat_net_sort_verts %in% outlet_verts],
                                mode = "in")

  cat_sets <- data.frame(ID = names(outlet_edge),
                         set = I(rep(list(list()), length(outlet_cats))),
                         geom = I(rep(list(list()), length(outlet_cats))),
                         stringsAsFactors = FALSE)

  verts <- V(cat_net)

  for(cat in seq_len(nrow(cat_sets))) {
    ut <- bfs(graph = cat_net,
              root = tail_of(cat_net, cat_sets$ID[cat]),
              neimode = "in",
              order = TRUE,
              unreachable = FALSE,
              restricted = verts)
    ut_verts <- ut$order[!is.na(ut$order)]
    ut_edge <- incident_edges(cat_net, ut_verts, "in")
    cat_sets$set[[cat]] <- list(as.numeric(names(ut_edge)))
    remove <- head_of(cat_net, unlist(ut_edge))
    verts <- verts[!verts %in% remove]
    cat_sets$geom[[cat]] <- st_union(st_geometry(filter(cat_rec, ID %in% unlist(cat_sets$set[cat]))))[[1]]
  }
  cat_sets$geom <- st_cast(st_sfc(cat_sets$geom, crs = st_crs(cat_rec)), "MULTIPOLYGON")
  return(st_sf(cat_sets))
}
# plot(cat_net, edge.arrow.size = 0.1, vertex.size = 2, vertex.label = NA, edge.arrow.size = 2, edge.label = V(cat_net))
# plot(cat_net, edge.arrow.size = 0.1, vertex.size = 5, edge.arrow.size = 2)
# http://proceedings.esri.com/library/userconf/proc02/pap1224/p1224.htm
