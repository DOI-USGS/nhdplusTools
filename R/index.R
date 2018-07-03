#' @title get flowline index
#' @description given an sf point geometry collumn, return COMID, reachcode, and measure for each.
#' @param points sfc of type POINT
#' @param flines sf data.frame of type LINESTRING or MULTILINESTRING including
#' COMID, REACHCODE, ToMeas, and FromMeas
#' @param search_radius numeric the distance for the nearest neighbor search to extend.
#' See RANN nn2 documentation for more details.
#' @param precision numeric the resolution of measure precision in the output.
#' @return data.frame with three columns, COMID, REACHCODE, and REACH_meas.
#' @details Note 1: that inputs are cast into LINESTRINGS. Because of this, the measure output
#' of inputs that are true multipart lines may be in error.
#'
#' Note 2: This algorithm finds the nearest node in the input flowlines to identify which
#' catchment the point should belong to. As a second pass, it can calculate the measure
#' to greater precision than the nearest flowline geometry node.
#'
#' @importFrom dplyr filter select mutate
#' @importFrom RANN nn2
#' @export
#' @examples
#' flines <- readRDS(system.file("extdata/petapsco_flowlines.rds", package = "nhdplusTools"))
#' point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)), crs = 4326)
#' get_flowline_index(flines, point)
#'

get_flowline_index <- function(flines, points, search_radius = 0.1, precision = NA) {

  in_crs <- sf::st_crs(flines)
  if (sf::st_crs(points) != in_crs) {
    warning("crs of lines and points don't match. attempting st_transform of points")
    points <- sf::st_transform(points, sf::st_crs(flines))
  }

  if (!all(c("COMID", "REACHCODE", "FromMeas", "ToMeas") %in% names(flines))) {
    stop("Need: 'COMID', 'REACHCODE', 'FromMeas', 'ToMeas' columns in flines input.")
  }

  points <- sf::st_coordinates(points)

  flines <- select(flines, COMID, REACHCODE, FromMeas, ToMeas) %>%
    mutate(index = 1:nrow(flines))

  fline_atts <- sf::st_set_geometry(flines, NULL)

  flines <- sf::st_zm(sf::st_cast(flines, "LINESTRING", warn = FALSE))

  if (nrow(flines) != nrow(fline_atts)) {
    warning(paste0("measure may be inacurate due to ",
                   "conversion from multipart to singlepart lines"))
  }

  flines <- sf::st_coordinates(flines)

  matched <- matcher(flines, points, search_radius) %>%
    left_join(select(fline_atts, COMID, index), by = c("L1" = "index"))

  if (!is.na(precision)) {
    # upstream to downstream order.
    flines <- as.data.frame(flines[which(flines[, "L1"] %in% matched$L1), ])

    # Geometry nodes are in downstream to upstream order.
    flines <- sf::st_as_sf(flines, coords = c("X", "Y"),
                   crs = in_crs) %>%
      group_by(L1) %>%
      summarise(do_union = FALSE) %>%
      mutate(index = 1:nrow(.)) %>%
      sf::st_cast("LINESTRING", warn = FALSE) %>%
      sf::st_segmentize(dfMaxLength = units::as_units(precision, "m"))

    fline_atts <- right_join(fline_atts,
                         select(sf::st_set_geometry(flines, NULL),
                                L1, precision_index = index),
                         by = c("index" = "L1"))

    # downstream to upstream order
    flines <- sf::st_coordinates(flines)

    matched <- matcher(flines, points, search_radius) %>%
      left_join(select(fline_atts, COMID, precision_index), by = c("L1" = "precision_index"))
  }

  flines <- as.data.frame(flines) %>%
    mutate(index = 1:nrow(flines)) %>%
    filter(L1 %in% matched$L1) %>%
    group_by(L1) %>%
    mutate(len  = sqrt( ( (X - (lag(X))) ^ 2) + ( ( (Y - (lag(Y))) ^ 2)))) %>%
    mutate(len = ifelse(is.na(len), 0, len)) %>%
    mutate(len = cumsum(len)) %>%
    mutate(measure = len / max(len)) %>%
    left_join(select(matched, L1, COMID), by = "L1") %>%
    left_join(select(fline_atts, -index), by = "COMID") %>%
    mutate(REACH_meas = ToMeas + (FromMeas - ToMeas) * (measure)) %>%
    ungroup() %>% distinct()

  matched <- select(matched, node = nn.idx, COMID)

  matched <- left_join(matched, select(flines, index, REACHCODE, REACH_meas),
                        by = c("node" = "index")) %>%
    select(COMID, REACHCODE, REACH_meas)

  return(matched)
}

matcher <- function(coords, points, search_radius) {
  matched <- nn2(data = coords[, 1:2],
                 query = matrix(points[, c("X", "Y")], ncol = 2),
                 k = 1,
                 searchtype = "radius",
                 radius = search_radius) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    left_join(mutate(select(as.data.frame(coords), L1),
                     index = 1:nrow(coords)),
              by = c("nn.idx" = "index"))
}
