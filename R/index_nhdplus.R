#' @title Get Flowline Index
#' @description given an sf point geometry column, return COMID, reachcode,
#' and measure for each.
#' @param points sfc of type POINT
#' @param flines sf data.frame of type LINESTRING or MULTILINESTRING including
#' COMID, REACHCODE, ToMeas, and FromMeas
#' @param search_radius numeric the distance for the nearest neighbor search
#' to extend.
#' See RANN nn2 documentation for more details.
#' @param precision numeric the resolution of measure precision in the output.
#' @return data.frame with four columns, COMID, REACHCODE, REACH_meas, and offset.
#' @details Note 1: Inputs are cast into LINESTRINGS. Because of this,
#' the measure output
#' of inputs that are true multipart lines may be in error.
#'
#' Note 2: This algorithm finds the nearest node in the input flowlines to
#' identify which flowline the point should belong to. As a second pass,
#' it can calculate the measure to greater precision than the nearest flowline
#' geometry node.
#'
#' Note 3: Offset is returned in units consistant with the projection of
#' the flowlines.
#'
#' @importFrom dplyr filter select mutate right_join left_join
#' @importFrom dplyr group_by summarise distinct desc lag n
#' @importFrom RANN nn2
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' sample_flines <- sf::read_sf(system.file("extdata",
#'                                          "petapsco_flowlines.gpkg",
#'                                          package = "nhdplusTools"))
#' get_flowline_index(sample_flines,
#'                    sf::st_sfc(sf::st_point(c(-76.87479,
#'                                              39.48233)),
#'                               crs = 4326))
#'

get_flowline_index <- function(flines, points,
                               search_radius = 0.1,
                               precision = NA) {

  flines <- check_names(flines, "get_flowline_index")

  in_crs <- sf::st_crs(flines)

  if (sf::st_crs(points) != in_crs) {
    warning(paste("crs of lines and points don't match.",
                  "attempting st_transform of points"))
    points <- sf::st_transform(points, sf::st_crs(flines))
  }

  points <- sf::st_coordinates(points)

  flines <- select(flines, COMID, REACHCODE, FromMeas, ToMeas) %>%
    mutate(index = seq_len(nrow(flines)))

  fline_atts <- sf::st_set_geometry(flines, NULL)

  flines <- sf::st_zm(sf::st_cast(flines, "LINESTRING", warn = FALSE))

  if (nrow(flines) != nrow(fline_atts)) {
    warning(paste0("measure may be inacurate due to ",
                   "conversion from multipart to singlepart lines"))
  }

  flines <- sf::st_coordinates(flines)

  matched <- matcher(flines, points, search_radius) %>%
    left_join(select(fline_atts, .data$COMID, .data$index),
              by = c("L1" = "index"))

  if (!is.na(precision)) {
    # upstream to downstream order.
    flines <- as.data.frame(flines[which(flines[, "L1"] %in% matched$L1), ])

    # Geometry nodes are in downstream to upstream order.
    flines <- sf::st_as_sf(flines, coords = c("X", "Y"),
                   crs = in_crs) %>%
      group_by(.data$L1) %>%
      summarise(do_union = FALSE) %>%
      mutate(index = seq_len(nrow(.))) %>%
      sf::st_cast("LINESTRING", warn = FALSE) %>%
      sf::st_segmentize(dfMaxLength = units::as_units(precision, "m"))

    fline_atts <- right_join(fline_atts,
                         select(sf::st_set_geometry(flines, NULL),
                                .data$L1, precision_index = .data$index),
                         by = c("index" = "L1"))

    # downstream to upstream order
    flines <- sf::st_coordinates(flines)

    matched <- matcher(flines, points, search_radius) %>%
      left_join(select(fline_atts, .data$COMID, .data$precision_index),
                by = c("L1" = "precision_index"))
  }

  flines <- as.data.frame(flines) %>%
    mutate(index = seq_len(nrow(flines))) %>%
    filter(.data$L1 %in% matched$L1) %>%
    group_by(.data$L1) %>%
    mutate(len  = sqrt( ( (.data$X - (lag(.data$X))) ^ 2) +
                          ( ( (.data$Y - (lag(.data$Y))) ^ 2)))) %>%
    mutate(len = ifelse(is.na(.data$len), 0, .data$len)) %>%
    mutate(len = cumsum(.data$len)) %>%
    mutate(measure = .data$len / max(.data$len)) %>%
    left_join(select(matched, .data$L1, .data$COMID), by = "L1") %>%
    left_join(select(fline_atts, -.data$index), by = "COMID") %>%
    mutate(REACH_meas = .data$ToMeas +
             (.data$FromMeas - .data$ToMeas) * (.data$measure)) %>%
    ungroup() %>% distinct()

  matched <- select(matched, node = .data$nn.idx, offset = .data$nn.dists, .data$COMID)

  matched <- left_join(matched,
                       select(flines, .data$index, .data$REACHCODE, .data$REACH_meas),
                        by = c("node" = "index")) %>%
    select(.data$COMID, .data$REACHCODE, .data$REACH_meas, .data$offset)

  return(matched)
}

matcher <- function(coords, points, search_radius) {
  matched <- nn2(data = coords[, 1:2],
                 query = matrix(points[, c("X", "Y")], ncol = 2),
                 k = 1,
                 searchtype = "radius",
                 radius = search_radius) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    left_join(mutate(select(as.data.frame(coords), .data$L1),
                     index = seq_len(nrow(coords))),
              by = c("nn.idx" = "index"))
}
