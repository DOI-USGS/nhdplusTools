
matcher <- function(coords, points, search_radius, max_matches = 1) {

  max_match_ <- ifelse(nrow(coords) < 1000, nrow(coords), 1000)

  matched <- nn2(data = coords[, 1:2],
                 query = matrix(points[, c("X", "Y")], ncol = 2),
                 k = ifelse(max_matches > 1, max_match_, 1),
                 searchtype = "radius",
                 radius = search_radius)

  matched <- data.frame(nn.idx = as.integer(matched$nn.idx),
                        nn.dists = as.numeric(matched$nn.dists),
                        id = rep(1:nrow(points), ncol(matched$nn.idx)))

  matched <- left_join(matched, mutate(data.frame(L1 = coords[, "L1"]),
                                       index = seq_len(nrow(coords))),
                       by = c("nn.idx" = "index"))

  rm(coords)

  matched <- filter(matched, .data$nn.dists <= search_radius)

  # First get rid of duplicate nodes on the same line.
  matched <- group_by(matched, .data$L1, .data$id) %>%
    filter(.data$nn.dists == min(.data$nn.dists)) %>%
    ungroup()

  # Now limit to max matches per point
  matched <- group_by(matched, .data$id) %>%
    filter(dplyr::row_number() <= max_matches) %>%
    ungroup() %>%
    as.data.frame()

  return(matched)
}

#' @title Get Flowline Index
#' @description given an sf point geometry column, return COMID, reachcode,
#' and measure for each.
#' @param flines sf data.frame of type LINESTRING or MULTILINESTRING including
#' COMID, REACHCODE, ToMeas, and FromMeas. Can be "download_nhdplusv2" and remote
#' nhdplusv2 data will be downloaded for the bounding box surround the submitted points.
#' NOTE: The download option may not work for large areas, use with caution.
#' @param points sf or sfc of type POINT in analysis projection. NOTE: flines will
#' be projected to the projection of the points layer.
#' @param search_radius units distance for the nearest neighbor search
#' to extend in analysis projection. If missing or NULL, and points are in a lon
#' lat projection, a default of 0.01 degree is used, otherwise 200 m is used.
#' Conversion to the linear unit used by the provided crs of points is attempted.
#' See RANN nn2 documentation for more details.
#' @param precision numeric the resolution of measure precision in the output in meters.
#' @param max_matches numeric the maximum number of matches to return if multiple are
#' found in search_radius
#' @return data.frame with five columns, id, COMID, REACHCODE, REACH_meas, and offset. id is the
#' row or list element in the point input.
#' @details Note 1: Inputs are cast into LINESTRINGS. Because of this,
#' the measure output
#' of inputs that are true multipart lines may be in error.
#'
#' Note 2: This algorithm finds the nearest node in the input flowlines to
#' identify which flowline the point should belong to. As a second pass,
#' it can calculate the measure to greater precision than the nearest flowline
#' geometry node.
#'
#' Note 3: Offset is returned in units consistent with the projection of
#' the input points.
#'
#' Note 4: See `dfMaxLength` input to sf::st_segmentize() for details of
#' handling of precision parameter.
#'
#' Note 5: "from" is downstream -- 0 is the outlet "to" is upstream -- 100 is the inlet
#'
#' @importFrom dplyr filter select mutate right_join left_join
#' @importFrom dplyr group_by summarise distinct desc lag n arrange
#' @importFrom RANN nn2
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \donttest{
#'
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)),
#'                     crs = 4326)
#'
#' get_flowline_index(sample_flines, point)
#'
#' point <- sf::st_transform(point, 5070)
#'
#' get_flowline_index(sample_flines, point,
#'                    search_radius = units::set_units(200, "m"))
#'
#' get_flowline_index("download_nhdplusv2", point)
#'
#' get_flowline_index(sample_flines, point, precision = 30)
#'
#' get_flowline_index(sample_flines,
#'                    sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
#'                                    sf::st_point(c(-76.91711, 39.40884)),
#'                                    sf::st_point(c(-76.88081, 39.36354))),
#'                               crs = 4326),
#'                    search_radius = units::set_units(0.2, "degrees"),
#'                    max_matches = 10)
#'
#' }

get_flowline_index <- function(flines, points,
                               search_radius = NULL,
                               precision = NA,
                               max_matches = 1) {

  in_crs <- sf::st_crs(points)

  search_radius <- check_search_radius(search_radius, points)

  point_buffer <- sf::st_buffer(points, search_radius)

  if(is.character(flines) && flines == "download_nhdplusv2") {

    if((!is.null(nrow(points)) && nrow(points)) == 1 | length(points) == 1) {
      req <- sf::st_buffer(points, search_radius)
    } else {
      req <- points
    }

    flines <- align_nhdplus_names(
      get_nhdplus(AOI = sf::st_transform(req, 4326),
                  realization = "flowline")) %>%
      sf::st_transform(sf::st_crs(points))

  }

  flines <- check_names(flines, "get_flowline_index")

  if(units(search_radius) == units(units::as_units("degrees"))) {
    if(sf::st_is_longlat(in_crs) & search_radius > units::set_units(1, "degree")) {
      warning("search radius is large for lat/lon input, are you sure?")
    }
  }

  flines <- match_crs(flines, points,
                      paste("crs of lines and points don't match.",
                            "attempting st_transform of lines"))

  search_radius <- as.numeric(search_radius) # everything in same units now

  if(!is.na(precision)) {
    suppressWarnings(flines <- sf::st_intersection(flines, point_buffer))
  }

  flines <- select(flines, COMID, REACHCODE, FromMeas, ToMeas) %>%
    mutate(index = seq_len(nrow(flines)))

  fline_atts <- sf::st_set_geometry(flines, NULL)

  suppressWarnings(flines <- sf::st_cast(flines, "LINESTRING", warn = FALSE))

  if(!"XY" %in% class(sf::st_geometry(flines)[[1]])) {
    flines <- sf::st_zm(flines)
  }

  if (nrow(flines) != nrow(fline_atts)) {

    flines <- summarize(group_by(select(flines, "index"),
                                 .data$index),
                        do_union = FALSE)

    flines <- left_join(flines, fline_atts, by = "index")

    multi <- lengths(sf::st_geometry(flines)) > 1

    if(any(multi)) {
      warning(paste0("Attempting to combine multipart lines into single ",
                     "part lines. Check results!!"))

      st_geometry(flines)[multi] <- lapply(st_geometry(flines)[multi], function(x) {
        sf::st_linestring(do.call(rbind, x))
      })

      flines  <- sf::st_zm(sf::st_cast(flines, "LINESTRING", warn = FALSE))
    }
  }

  points <- sf::st_coordinates(points)

  if(!is.na(precision)) {

    # upstream to downstream order.
    flines <- sf::st_coordinates(flines)

    # Geometry nodes are in downstream to upstream order.
    flines <- as.data.frame(flines) %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = in_crs) %>%
      group_by(.data$L1) %>%
      summarise(do_union = FALSE) %>%
      mutate(index = seq_len(nrow(.))) %>%
      sf::st_cast("LINESTRING", warn = FALSE) %>%
      sf::st_segmentize(dfMaxLength = units::as_units(precision, "m"))

    fline_atts <- right_join(fline_atts,
                             select(drop_geometry(flines),
                                    "L1", precision_index = "index"),
                             by = c("index" = "L1"))

    # downstream to upstream order
    flines <- sf::st_coordinates(flines)


    matched <- matcher(flines, points, search_radius, max_matches = max_matches) %>%
      left_join(select(fline_atts, "COMID", "precision_index"),
                by = c("L1" = "precision_index"))

    matched <- mutate(matched, nn.dists = ifelse(.data$nn.dists > search_radius,
                                                 NA, .data$nn.dists))
  } else {

    flines <- sf::st_coordinates(flines)


    matched <- matcher(flines, points, search_radius, max_matches = max_matches) %>%
      left_join(select(fline_atts, "COMID", "index"),
                by = c("L1" = "index"))

    matched <- mutate(matched, nn.dists = ifelse(.data$nn.dists > search_radius,
                                                 NA, .data$nn.dists))

  }

  flines <- flines %>%
    add_index() %>%
    filter(.data$L1 %in% matched$L1) %>%
    group_by(.data$L1) %>%
    add_len() %>%
    left_join(select(matched, "L1", "COMID"), by = "L1") %>%
    left_join(select(fline_atts, -"index"), by = "COMID") %>%
    mutate(REACH_meas = round(
      .data$FromMeas + (.data$ToMeas - .data$FromMeas) * (.data$measure / 100),
      digits = 4)) %>%
    ungroup() %>% distinct()

  matched <- select(matched, "id", node = "nn.idx", offset = "nn.dists", "COMID")

  matched <- left_join(matched,
                      distinct(select(flines, "index", "REACHCODE", "REACH_meas")),
                        by = c("node" = "index")) %>%
    select("id", "COMID", "REACHCODE", "REACH_meas", "offset")

  return(matched)
}

check_search_radius <- function(search_radius, points) {

  if(is.null(search_radius)) {
    if(sf::st_is_longlat(points)) {
      search_radius <- units::set_units(0.01, "degrees")
    } else {
      search_radius <- units::set_units(200, "m")

      units(search_radius) <- units::as_units(
        sf::st_crs(points, parameters = TRUE)$ud_unit)
    }
  }

  if(!inherits(search_radius, "units")) {
    warning("search_radius units not set, trying units of points.")
    units(search_radius) <- units::as_units(
      sf::st_crs(points, parameters = TRUE)$ud_unit)
  }

  search_radius
}

#' @title Disambiguate Flowline Indexes
#' @description Given a set of flowline indexes and numeric or ascii criteria,
#' return closest match. If numeric criteria are used, the minimum difference
#' in the numeric attribute is used for disambiguation. If ascii criteria are used,
#' the \link[utils]{adist} function is used with the following algorithm:
#' `1 - adist_score / max_string_length`. Comparisons ignore case.
#' @param indexes data.frame as output from \link{get_flowline_index} with more than
#' one hydrologic location per indexed point.
#' @param flowpath data.frame with two columns. The first should join to the COMID
#' field of the indexes and the second should be the numeric or ascii metric such as drainage
#' area or GNIS Name. Names of this data.frame are not used.
#' @param hydro_location data.frame with two columns. The first should join to the
#' id field of the indexes and the second should be the numeric or ascii metric such as drainage
#' area or GNIS Name.. Names of this data,frame are not used.
#' @return data.frame indexes deduplicated according to the minimum difference
#' between the values in the metric columns. If two or more result in the same "minimum"
#' value, duplicates will be returned.
#' @export
#' @examples
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' hydro_location <- sf::st_sf(id = c(1, 2, 3),
#'                             geom = sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
#'                                                    sf::st_point(c(-76.91711, 39.40884)),
#'                                                    sf::st_point(c(-76.88081, 39.36354))),
#'                                               crs = 4326),
#'                             totda = c(23.6, 7.3, 427.9),
#'                             nameid = c("Patapsco", "", "Falls Run River"))
#'
#' flowpath <- dplyr::select(sample_flines,
#'                           comid = COMID,
#'                           totda = TotDASqKM,
#'                           nameid = GNIS_NAME,
#'                           REACHCODE,
#'                           ToMeas,
#'                           FromMeas)
#'
#' indexes <- get_flowline_index(flowpath,
#'                               hydro_location,
#'                               search_radius = 0.2,
#'                               max_matches = 10)
#'
#' disambiguate_flowline_indexes(indexes,
#'                               dplyr::select(flowpath, comid, totda),
#'                               dplyr::select(hydro_location, id, totda))
#'
#' result <- disambiguate_flowline_indexes(indexes,
#'                                         dplyr::select(flowpath, comid, nameid),
#'                                         dplyr::select(hydro_location, id, nameid))
#'
#' result[result$id == 1, ]
#'
#' result[result$id == 2, ]
#'
#' result[result$id == 3, ]
#'
disambiguate_flowline_indexes <- function(indexes, flowpath, hydro_location) {
  check_names(indexes, "disambiguate_flowline_indexes")

  flowpath <- drop_geometry(flowpath)
  hydro_location <- drop_geometry(hydro_location)

  if(ncol(flowpath) != 2 | ncol(hydro_location) != 2) {
    stop("flowpath and hydrolocation must be two-column data.frames")
  }

  names(flowpath) <- c("comid", "metric_fp")

  names(hydro_location) <- c('id', "metric_hl")

  if(is.numeric(flowpath$metric_fp) & is.numeric(hydro_location$metric_hl)) {

    indexes %>%
      left_join(flowpath, by = c("COMID" = "comid")) %>%
      left_join(hydro_location, by = "id") %>%
      mutate(metric_diff = abs(.data$metric_fp - .data$metric_hl)) %>%
      group_by(.data$id) %>%
      filter(.data$metric_diff == min(.data$metric_diff)) %>%
      ungroup() %>%
      select(-"metric_hl", -"metric_fp", -"metric_diff")

  } else if(is.character(flowpath$metric_fp) & is.character(hydro_location$metric_hl)) {

    indexes %>%
      left_join(flowpath, by = c("COMID" = "comid")) %>%
      left_join(hydro_location, by = "id") %>%
      mutate(metric_diff = sapply(mapply(c, .data$metric_fp, .data$metric_hl,
                                         USE.NAMES = FALSE, SIMPLIFY = FALSE),
                                  string_score)) %>%
      group_by(.data$id) %>%
      filter(.data$metric_diff == max(.data$metric_diff)) %>%
      ungroup() %>%
      select(-"metric_hl", -"metric_fp", -"metric_diff")

  } else  stop("flowpath and hydrolocation metrics must both be numeric or character")

}

string_score <- function(x) {
  raw_score <- as.numeric(utils::adist(x[[1]], x[[2]], ignore.case = TRUE))

  (1 - (raw_score) / max(c(nchar(x[[1]]), nchar(x[[2]]))))
}

#' @title Get Waterbody Index
#' @description given an sf point geometry column, return waterbody id, and
#' COMID of dominant artificial path
#' @param waterbodies sf data.frame of type POLYGON or MULTIPOLYGON including
#' COMID attributes.
#' @param flines sf data.frame of type LINESTRING or MULTILINESTRING including
#' COMID, WBAREACOMI, and Hydroseq attributes
#' @param points sfc of type POINT
#' @param search_radius units class with a numeric value indicating how far to
#' search for a waterbody boundary in units of provided projection. Set units with
#' \link[units]{set_units}.
#' @return data.frame with two columns, COMID, in_wb_COMID, near_wb_COMID,
#' near_wb_dist, and outlet_fline_COMID. Distance is in units of provided projection.
#' @importFrom sf st_join st_geometry_type
#' @importFrom dplyr select mutate bind_cols
#' @export
#' @examples
#'
#' source(system.file("extdata/sample_data.R", package = "nhdplusTools"))
#'
#' waterbodies <- sf::st_transform(
#'   sf::read_sf(sample_data, "NHDWaterbody"), 5070)
#'
#' points <- sf::st_transform(
#'   sf::st_sfc(sf::st_point(c(-89.356086, 43.079943)),
#'              crs = 4326), 5070)
#'
#' get_waterbody_index(waterbodies, points,
#'                     search_radius = units::set_units(500, "m"))
#'
get_waterbody_index <- function(waterbodies, points, flines = NULL,
                                search_radius = NULL) {
  check_names(waterbodies, "get_waterbody_index_waterbodies")

  points <- st_geometry(points)

  search_radius <- as.numeric(check_search_radius(search_radius, points))

  points <- st_sf(id = seq_len(length(points)), geometry = points)

  waterbodies <- select(waterbodies, wb_COMID = "COMID")

  points <- match_crs(points, waterbodies, "st_transform points to match waterbodies")

  points <-suppressMessages(st_join(points, waterbodies))

  wb_atts <- mutate(st_drop_geometry(waterbodies), index = seq_len(nrow(waterbodies)))

  waterbodies <- make_singlepart(waterbodies, "Converting to singlepart.")

  waterbodies <- st_coordinates(waterbodies)

  if(ncol(waterbodies) == 4) waterbodies[ ,3] <- waterbodies[ ,4]

  near_wb <- matcher(waterbodies,
                     st_coordinates(points), search_radius)
  near_wb <- left_join(near_wb, wb_atts, by = c("L1" = "index"))
  near_wb <- left_join(data.frame(id = c(1:nrow(points))), near_wb, by = "id")
  near_wb <- mutate(near_wb, nn.dists = ifelse(.data$nn.dists > search_radius,
                                               NA, .data$nn.dists))

  out <- drop_geometry(st_as_sf(bind_cols(select(near_wb, near_wb_COMID = "wb_COMID",
                                                    near_wb_dist = "nn.dists"),
                                             select(points, in_wb_COMID = "wb_COMID"))))

  if(!is.null(flines)) {

    check_names(flines, "get_waterbody_index_flines")

    out <- mutate(out, joiner = ifelse(!is.na(.data$in_wb_COMID),
                                       .data$in_wb_COMID, .data$near_wb_COMID),
                  id = seq_len(nrow(out)))

    flines <- drop_geometry(flines)

    out <- left_join(out, select(flines,
                                 outlet_fline_COMID = "COMID",
                                 "WBAREACOMI", "Hydroseq"),
                     by = c("joiner" = "WBAREACOMI"))

    out <- ungroup(filter(group_by(out, .data$id), is.na(Hydroseq) | Hydroseq == min(Hydroseq)))

    out <- select(out, -"id", -"Hydroseq", -"joiner")

  }

  out
}

make_singlepart <- function(x, warn_text = "") {
  check <- nrow(x)

  gt <- st_geometry_type(x, by_geometry = FALSE)

  if(grepl("^MULTI", gt)) {
    x <- sf::st_cast(x, gsub("^MULTI", "", gt), warn = FALSE)
  }

  if (nrow(x) != check) {
    warning(warn_text)
  }

  sf::st_zm(x)
}

match_crs <- function(x, y, warn_text = "") {
  if (sf::st_crs(x) != sf::st_crs(y)) {
    warning(warn_text)
    x <- sf::st_transform(x, sf::st_crs(y))
  }
  x
}

#' Get Hydro Location
#' @description given a flowline index, returns the hydrologic location (point)
#' along the specific linear element referenced by the index.
#' @param indexes data.frame as output from \link{get_flowline_index}.
#' @param flowpath data.frame with three columns: COMID, FromMeas, and ToMeas
#' as well as geometry.
#' @export
#' @examples
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' indexes <- get_flowline_index(sample_flines,
#'                    sf::st_sfc(sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
#'                                               sf::st_point(c(-76.91711, 39.40884)),
#'                                               sf::st_point(c(-76.88081, 39.36354))),
#'                               crs = 4326)))
#'
#' get_hydro_location(indexes, sample_flines)
#'
get_hydro_location <- function(indexes, flowpath) {
  flowpath <- check_names(flowpath, "get_hydro_location", tolower = TRUE)

  names(indexes) <- tolower(names(indexes))

  in_list <- Map(list,
                 indexes$reach_meas,
                 split(flowpath[match(indexes$comid, flowpath$comid), ],
                                 seq(1, nrow(indexes))))

  do.call(c, lapply(in_list, get_hydro_location_single))

}

get_hydro_location_single <- function(x) {

  coords <- sf::st_coordinates(x[[2]]) %>%
    add_index() %>%
    add_len()

  # First rescale 0-100 measures passed in.
  m <- rescale_measures(x[[1]], x[[2]]$frommeas, x[[2]]$tomeas)

  nus <- nrow(coords) - sum(coords$measure <= m)

  if(nus == 0) {
    nus <- 1
  }

  nds <- ifelse(nus < nrow(coords), nus + 1, nus)

  if(nds == nus) {
    return(
    sf::st_sfc(sf::st_point(c(coords$X[nds], coords$Y[nds])),
               crs = sf::st_crs(x[[2]]))
  )}

  new_m <- rescale_measures(m, coords$measure[nds], coords$measure[nus])

  new <- interp_meas(new_m, coords$X[nds], coords$Y[nds], coords$X[nus], coords$Y[nus])

  return(sf::st_sfc(sf::st_point(c(new[[1]], new[[2]])), crs = sf::st_crs(x[[2]])))
}

interp_meas <- function(m, x1, y1, x2, y2) {
  list(x1 + (m / 100) * (x2 - x1),
       y1 + (m / 100) * (y2 - y1))
}

add_index <- function(x) {
  x %>%
    as.data.frame() %>%
    mutate(index = seq_len(nrow(.)))
}

add_len <- function(x) {
  x %>%
    mutate(len  = sqrt( ( (.data$X - (lag(.data$X))) ^ 2) +
                          ( ( (.data$Y - (lag(.data$Y))) ^ 2)))) %>%
    mutate(len = tidyr::replace_na(.data$len, 0)) %>%
    mutate(len = cumsum(.data$len)) %>%
    mutate(measure = 100 - (100 * .data$len / max(.data$len)))
}

#' Rescale reachcode measure to comid flowline measure
#' @description Given a reachcode measure and the from and to measure for a
#' comid flowline, returns the measure along the comid flowline. This is
#' a utility specific to the NHDPlus data model where many comid flowlines make
#' up a single reachcode / reach. "Measures" are typically referenced to
#' reaches. Flowlines have a stated from-measure / to-measure. In some cases
#' it is useful to rescale the measure such that it is relative only to the
#' flowline.
#'
#' from is downstream -- 0 is the outlet
#' to is upstream -- 100 is the inlet
#'
#' @param measure numeric reach measure between 0 and 100
#' @param from numeric flowline from-measure relative to the reach
#' @param to numeric flowline to-measure relative to the reach
#' @return numeric rescaled measure
#' @export
#' @examples
#' rescale_measures(40, 0, 50)
#' rescale_measures(60, 50, 100)
#'
rescale_measures <- function(measure, from, to) {
  tryCatch({

    if(!dplyr::between(measure, from, to))
      stop("measure must be between from and to")

    100 * (measure - from) / (to - from)

  }, error = function(e) {
    if(measure < from & from - measure < 0.1 |
       measure > to & measure - to < 0.1) {

      to <- round(to, 1)
      from <- round(from, 1)
      measure <- round(measure, 1)
      100 * (measure - from) / (to - from)

    } else {
      stop(e)
    }
  })
}
