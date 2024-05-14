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
#' @importFrom hydroloom index_points_to_lines
#' @importFrom magrittr `%>%`
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
#'
get_flowline_index <- function(flines, points,
                               search_radius = NULL,
                               precision = NA,
                               max_matches = 1) {

  if(is.character(flines) && flines == "download_nhdplusv2") {

    if((!is.null(nrow(points)) && nrow(points)) == 1 | length(points) == 1) {

      search_radius <- hydroloom:::check_search_radius(search_radius, points)

      req <- sf::st_buffer(points, search_radius)

    } else {

      req <- points

    }

    flines <- align_nhdplus_names(
      get_nhdplus(AOI = sf::st_transform(req, 4326),
                  realization = "flowline")) |>
      sf::st_transform(sf::st_crs(points))

  }

  flines <- check_names(flines, "get_flowline_index")

  out <- index_points_to_lines(flines, points, search_radius, precision, max_matches)

  rename(out, any_of(c(id = "point_id", REACH_meas = "REACHCODE_measure")))

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
#' @importFrom hydroloom disambiguate_indexes
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

  indexes <- rename(indexes, all_of(c(point_id = "id")))

  out <- disambiguate_indexes(indexes, flowpath, hydro_location)

  rename(out, all_of(c(id = "point_id")))

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
#' @importFrom hydroloom index_points_to_waterbodies
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

  index_points_to_waterbodies(waterbodies, points, flines, search_radius)

}
