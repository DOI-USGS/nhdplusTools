#' @title get flowline index
#' @description given an sf point geometry collumn, return COMID, reachcode, and measure for each.
#' @param points sfc of type POINT
#' @param flines sf data.frame of type LINESTRING or MULTILINESTRING including
#' COMID, reachcode, ToMeas, and FromMeas
#' @param search_radius numeric the distance for the nearest neighbor search to extend.
#' See RANN nn2 documentation for more details.
#' @return data.frame with three columns, COMID, reachcode, and REACH_meas.
#' @details Note 1: that inputs are cast into LINESTRINGS. Because of this, the measure output
#' of inputs that are true multipart lines may be in error.
#'
#' Note 2: This version of this function only resolves to the nearest geometry node. This can
#' cause problems if there are not enough nodes on a given flowline.
#'
#' @importFrom dplyr filter select mutate
#' @importFrom RANN nn2
#' @export
#' @example
#' flines <- sf::st_read(system.file("extdata/petapsco_flowlines.geojson", package = "nhdplusTools"))
#' point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
#' get_flowline_index(flines, point)
#'

get_flowline_index <- function(flines, points, search_radius = 0.1) {

  points <- sf::st_transform(points, sf::st_crs(flines))

  points <- sf::st_coordinates(points)

  flines <- select(flines, COMID, REACHCODE, FromMeas, ToMeas) %>%
    mutate(index = 1:nrow(flines))

  coords <- sf::st_coordinates(st_zm(st_cast(flines, "LINESTRING", warn = FALSE)))
  flines <- sf::st_set_geometry(flines, NULL)

  matched <- nn2(data = coords[, 1:2],
                 query = matrix(points[, c("X", "Y")], ncol = 2),
                 k = 1,
                 searchtype = "radius",
                 radius = search_radius) %>%
    data.frame() %>%
    left_join(mutate(select(as.data.frame(coords), L1),
                     index = 1:nrow(coords)),
              by = c("nn.idx" = "index")) %>%
    left_join(select(flines, COMID, index), by = c("L1" = "index"))

  rel_coords <- as.data.frame(coords) %>%
    mutate(index = 1:nrow(coords)) %>%
    filter(L1 %in% matched$L1) %>%
    group_by(L1) %>%
    mutate(len  = sqrt( ( (X - (lag(X))) ^ 2) + ( ( (Y - (lag(Y))) ^ 2)))) %>%
    mutate(len = ifelse(is.na(len), 0, len)) %>%
    mutate(len = cumsum(len)) %>%
    mutate(measure = len / max(len)) %>%
    left_join(select(matched, L1, COMID), by = "L1") %>%
    left_join(select(flines, -index), by = "COMID") %>%
    mutate(REACH_meas = FromMeas + (ToMeas - FromMeas) * (measure)) %>%
    ungroup() %>% distinct()

  matched <- select(matched, node = nn.idx, COMID)

  matched <- left_join(matched, select(rel_coords, index, REACHCODE, REACH_meas),
                        by = c("node" = "index")) %>%
    select(COMID, REACHCODE, REACH_meas)

  return(matched)
}
