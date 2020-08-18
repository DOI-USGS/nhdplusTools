#' @title Plot NHDPlus
#' @description Given a list of outlets, get their basin boundaries and network and return a plot.
#' @param outlets list of nldi outlets. Other inputs are coerced into nldi outlets, see details.
#' @param bbox object of class bbox with a defined crs. See examples.
#' @param streamorder integer only streams of order greater than or equal will be returned
#' @param nhdplus_data geopackage containing source nhdplus data (omit to download)
#' @param gpkg path and file with .gpkg ending. If omitted, no file is written.
#' @param overwrite passed on the \link{subset_nhdplus}.
#' @param plot_config list containing plot configuration, see details.
#' @param add boolean should this plot be added to an already built map.
#' @param actually_plot boolean actually draw the plot? Use to get data subset only.
#' @param flowline_only boolean only subset and plot flowlines?
#' @param ... parameters passed on to rosm.
#' @return plot data is returned invisibly.
#' @details plot_nhdplus supports several input specifications. An unexported function "as_outlet"
#' is used to convert the outlet formats as described below.
#' \enumerate{
#'   \item if outlets is omitted, the bbox input is required and all nhdplus data
#'   in the bounding box is plotted.
#'   \item If outlets is a list of integers, it is assumed to be NHDPlus IDs (comids)
#'   and all upstream tributaries are plotted.
#'   \item if outlets is an integer vector, it is assumed to be all NHDPlus IDs (comids)
#'   that should be plotted. Allows custom filtering.
#'   \item If outlets is a character vector, it is assumed to be NWIS site ids.
#'   \item if outlets is a list containing only characters, it is assumed to be a list
#'   of nldi features and all upstream tributaries are plotted.
#'   \item if outlets is a data.frame with point geometry, a point in polygon match
#'   is performed and upstream with tributaries from the identified catchments is plotted.
#' }
#'
#' The \code{plot_config} parameter is a list with names "basin", "flowline" and "outlets".
#' The following shows the defaults that can be altered.
#' \enumerate{
#'   \item basin \code{list(lwd = 1, col = NA, border = "black")}
#'   \item flowline \code{list(lwd = 1, col = "blue")}
#'   \item outlets \preformatted{
#'    list(default = list(col = "black", border = NA, pch = 19, cex = 1),
#'         nwissite = list(col = "grey40", border = NA, pch = 17, cex = 1),
#'         huc12pp = list(col = "white", border = "black", pch = 22, cex = 1),
#'         wqp = list(col = "red", border = NA, pch = 20, cex = 1))
#'         }
#' }
#'
#' @export
#' @examples
#' \donttest{
#' options("rgdal_show_exportToProj4_warnings"="none")
#' rosm::set_default_cachedir(tempfile())
#'
#' plot_nhdplus("05428500")
#'
#' plot_nhdplus("05428500", streamorder = 2)
#'
#' plot_nhdplus(list(13293970, 13293750))
#'
#' sample_data <- system.file("extdata/sample_natseamless.gpkg", package = "nhdplusTools")
#' plot_nhdplus(list(13293970, 13293750), streamorder = 3, nhdplus_data = sample_data)
#'
#' plot_nhdplus(list(list("comid", "13293970"),
#'                   list("nwissite", "USGS-05428500"),
#'                   list("huc12pp", "070900020603"),
#'                   list("huc12pp", "070900020602")),
#'              streamorder = 2,
#'              nhdplus_data = sample_data)
#'
#'
#' plot_nhdplus(sf::st_as_sf(data.frame(x = -89.36083,
#'                                      y = 43.08944),
#'                           coords = c("x", "y"), crs = 4326),
#'              streamorder = 2,
#'              nhdplus_data = sample_data)
#'
#' plot_nhdplus(list(list("comid", "13293970"),
#'                   list("nwissite", "USGS-05428500"),
#'                   list("huc12pp", "070900020603"),
#'                   list("huc12pp", "070900020602")),
#'              streamorder = 2,
#'              nhdplus_data = sample_data,
#'              plot_config = list(basin = list(lwd = 2),
#'                                 outlets = list(huc12pp = list(cex = 1.5),
#'                                                comid = list(col = "green"))))
#'
#' bbox <- sf::st_bbox(c(xmin = -89.43, ymin = 43, xmax = -89.28, ymax = 43.1),
#'                     crs = "+proj=longlat +datum=WGS84 +no_defs")
#'
#' fline <- sf::read_sf(sample_data, "NHDFlowline_Network")
#' comids <- nhdplusTools::get_UT(fline, 13293970)
#'
#' plot_nhdplus(comids)
#'
#' #' # With Local Data
#' plot_nhdplus(bbox = bbox, nhdplus_data = sample_data)
#'
#' # With downloaded data
#' plot_nhdplus(bbox = bbox, streamorder = 3)
#'
#' # Can also plot on top of the previous!
#' plot_nhdplus(bbox = bbox, nhdplus_data = sample_data,
#'              plot_config = list(flowline = list(lwd = 0.5)))
#' plot_nhdplus(comids, nhdplus_data = sample_data, streamorder = 3, add = TRUE,
#'              plot_config = list(flowline = list(col = "darkblue")))
#' }

plot_nhdplus <- function(outlets = NULL, bbox = NULL, streamorder = NULL,
                         nhdplus_data = NULL, gpkg = NULL, plot_config = NULL,
                         add = FALSE, actually_plot = TRUE, overwrite = TRUE,
                         flowline_only = NULL, ...) {

  pd <- get_plot_data(outlets, bbox, streamorder, nhdplus_data, gpkg, overwrite, flowline_only)

  if(actually_plot) {
    st <- get_styles(plot_config)

    prettymapr::prettymap({
      if(!add) {
        rosm::osm.plot(pd$plot_bbox, type = "cartolight", quiet = TRUE, progress = "none", ...)
      }
      # plot(gt(catchment), lwd = 0.5, col = NA, border = "grey", add = TRUE)
      if(!is.null(pd$basin))
        graphics::plot(gt(pd$basin), lwd = st$basin$lwd, col = st$basin$col,
                       border = st$basin$border, add = TRUE)
      graphics::plot(gt(pd$flowline), lwd = st$flowline$lwd, col = st$flowline$col,
                     add = TRUE)
      if(!is.null(pd$outlets)) {
        for(type in unique(pd$outlets$type)) {
          st_type <- "default"
          if(type %in% names(st$outlets)) st_type <- type
          graphics::plot(gt(pd$outlets[pd$outlets$type == type, ]), col = st$outlets[[st_type]]$col,
                         pch = st$outlets[[st_type]]$pch, bg = st$outlets[[st_type]]$bg,
                         cex = st$outlets[[st_type]]$cex, add = TRUE)
        }
      }
    },
    drawarrow = TRUE)
  }
  return(invisible(pd))
}

get_styles <- function(plot_config) {
  conf <- list(basin = list(lwd = 1, col = NA, border = "black"),
               flowline = list(lwd = 1, col = "blue"),
               outlets = list(default = list(col = "black", bg = NA, pch = 19, cex = 1),
                              nwissite = list(col = "grey40", bg = NA, pch = 17, cex = 1),
                              huc12pp = list(col = "black", bg = "white", pch = 22, cex = 1),
                              wqp = list(col = "red", bg = NA, pch = 20, cex = 1)))

  validate_plot_config(plot_config)

  for(x in names(plot_config)) {
    for(y in names(plot_config[[x]])) {
      if(!is.list(plot_config[[x]][[y]])) {
        conf[[x]][[y]] <- plot_config[[x]][[y]]
      } else {
        if(!y %in% names(conf[[x]])) conf[[x]][[y]] <- conf[[x]][["default"]]
        for(z in names(plot_config[[x]][[y]])) {
          conf[[x]][[y]][[z]] <- plot_config[[x]][[y]][[z]]
        }
      }
    }
  }

  conf

}

validate_plot_config <- function(plot_config) {
  if(is.null(plot_config)) return(invisible(NULL))

  if(!all(names(plot_config) %in% c("basin", "flowline", "outlets")))
    stop(paste('Expected one or more of "basin", "flowline", or "outlets" in plot_config, got:',
               names(plot_config)))

  if("basin" %in% names(plot_config)) {
    if(!all(names(plot_config$basin) %in% c("lwd", "col", "border")))
      stop('Expected one ore more of "lwd", "col", or "border" in basins plot_config, got:',
           names(plot_config$basin))
  }

  if("flowline" %in% names(plot_config)) {
    if(!all(names(plot_config$flowline) %in% c("lwd", "col")))
      stop('Expected one ore more of "lwd" and "col" in flowlines plot_config, got:',
           names(plot_config$flowline))
  }

  if(any(sapply(plot_config$outlets, function(x) !all(names(x) %in% c("col", "bg", "pch", "cex")))))
    stop(paste('Expected one or more of "col", "bg", "pch", or "cex" in outlets plot_config, got:',
               paste(unique(unlist(sapply(plot_config$outlets, names, simplify = TRUE))), collapse = ", ")))

  return(invisible(NA))
}

get_plot_data <- function(outlets = NULL, bbox = NULL,
                          streamorder = NULL, nhdplus_data = NULL,
                          gpkg = NULL, overwrite = TRUE, flowline_only = NULL, ...) {

  if(!is.null(outlets) & !is.null(bbox)) stop("Both bbox and outlets not supported.")

  if(!is.null(nhdplus_data)) {
    if(!file.exists(nhdplus_data)) {
      if(!is.null(gpkg) && nhdplus_data != gpkg)
        stop("couldn't find nhdplus_data and output data not requested by the same path.")
      nhdplus_data <- NULL
    } else if(!is.null(gpkg) && nhdplus_data == gpkg) {
      gpkg <- NULL
    }
  }

  if(!is.null(bbox)) {
    flowline <- dl_plot_data_by_bbox(bbox, nhdplus_data, gpkg, overwrite, streamorder = streamorder)
    catchment <- flowline$catchment
    basin <- flowline$basin
    nexus <- flowline$nexus
    flowline <- flowline$flowline
  }

  comids <- NULL
  if(methods::is(outlets, "integer")) {
    comids <- outlets
    outlets <- NULL
  }

  if(!is.null(outlets)) {
    outlets <- as_outlets(outlets)
    outlet_type <- sapply(outlets, function(x) x$featureSource)
  }

  fline_layer = get_flowline_layer_name(nhdplus_data)
  catchment_layer <- get_catchment_layer_name(simplified = TRUE, nhdplus_data)

  if(!is.null(nhdplus_data) & !is.null(outlets)) {

    flowline <- sf::st_zm(sf::read_sf(nhdplus_data, fline_layer))
    flowline <- align_nhdplus_names(flowline)
    # For the "COMID" inputs we don't have to go to the NLDI,
    nexus <- lapply(outlets, get_comid_outlets, flowline = flowline)

    flowline <- sf::st_set_geometry(flowline, NULL)

    empty <- sapply(nexus, nrow) == 0
    outlets <- outlets[empty]
    nexus <- nexus[!empty]

    if(length(outlets) > 0) {
      nexus <- c(nexus, lapply(outlets, get_outlet_from_nldi))
    }

    all_comids <- lapply(nexus, function(x) get_UT(align_nhdplus_names(flowline), x$comid))

    subsets <- subset_nhdplus(comids = unlist(all_comids), output_file = gpkg,
                              nhdplus_data = nhdplus_data, status = FALSE,
                              overwrite = overwrite, streamorder = streamorder,
                              flowline_only = flowline_only)

    flowline <- sf::st_zm(subsets[[fline_layer]])

    if("CatchmentSP" %in% names(subsets)) {
      catchment <- subsets[[catchment_layer]]

      basin <- do.call(rbind, lapply(all_comids, function(x, catchment_layer, subsets) {
        make_basin(subsets, catchment_layer, x)
      }, catchment_layer = catchment_layer, subsets = subsets))
    } else {
      catchment <- NULL
      basin <- NULL
    }

    nexus <- do.call(rbind, nexus)

  } else if(all(!is.null(outlets))) {
    basin <- do.call(rbind, lapply(outlets, get_nldi_basin))

    nexus <- do.call(rbind, lapply(outlets, get_outlet_from_nldi))

    nhd_data <- dl_plot_data_by_bbox(sf::st_bbox(basin), nhdplus_data, gpkg, overwrite, streamorder = streamorder, flowline_only = flowline_only)
    flowline <- align_nhdplus_names(nhd_data$flowline)
    flowline <- do.call(rbind, lapply(nexus$comid, function(x) {
      flowline[flowline$COMID %in% get_UT(align_nhdplus_names(flowline), x), ]
    }))

    catchment <- nhd_data$catchment
  }

  if(!is.null(comids)) {
    if(is.null(nhdplus_data)) nhdplus_data <- "download"
    nhd_data <- subset_nhdplus(comids, nhdplus_data = nhdplus_data,
                               status = FALSE, overwrite = overwrite,
                               flowline_only = flowline_only)
    if("CatchmentSP" %in% names(nhd_data)) {
      bbox <- sf::st_bbox(nhd_data$CatchmentSP)
      catchment <- nhd_data[[catchment_layer]]
      basin <- make_basin(nhd_data, catchment_layer = catchment_layer)
    } else {
      bbox <- sf::st_bbox(nhd_data$NHDFlowline_Network)
      catchment <- NULL
      basin <- NULL
    }
    flowline <- sf::st_zm(nhd_data$NHDFlowline_Network)
    nexus <- NULL
  }

  if(!is.null(outlets)) {
    nexus["type"] <- outlet_type
  }

  if(is.null(bbox)) {
    if(is.null(basin)) {
      bbox <- sp_bbox(sf::st_transform(flowline, 4326))
    } else {
      bbox <- sp_bbox(sf::st_transform(basin, 4326))
    }
  }

  if(!is.null(streamorder) && "StreamOrde" %in% names(flowline)) {
    flowline <- flowline[flowline$StreamOrde >= streamorder, ]
  }

  return(list(plot_bbox = bbox, outlets = nexus, flowline = flowline,
              basin = basin, catchment = catchment))
}

dl_plot_data_by_bbox <- function(bbox, nhdplus_data, gpkg, overwrite, streamorder = NULL, flowline_only = NULL) {

  bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), 4326))

  if(!is.null(nhdplus_data)) {
    source <- nhdplus_data
  } else {
    source <- "download"
  }

  if(is.null(flowline_only) && source == "download") {
    flowline_only <- TRUE
  } else {
    flowline_only <- FALSE
  }

  d <- subset_nhdplus(bbox = bbox, output_file = gpkg, nhdplus_data = source,
                      simplified = TRUE, status = FALSE,
                      overwrite = overwrite, flowline_only = flowline_only, streamorder = streamorder)

  d <- lapply(d, align_nhdplus_names)

  return(list(catchment = d$CatchmentSP,
              flowline = d$NHDFlowline_Network,
              nexus = NULL, basin = NULL))
}

gt <- function(x) sf::st_geometry(sf::st_transform(x, 3857))

sp_bbox <- function(g) {
  matrix(as.numeric(sf::st_bbox(g)),
         nrow = 2, dimnames = list(c("x", "y"),
                                   c("min", "max")))
}

make_basin <- function(x, catchment_layer, comids = NULL) {
  x <- x[[catchment_layer]]
  if(!is.null(comids)) {
    x <- x[x$FEATUREID %in% comids, ]
  }
  sf::st_precision(x) <- 10000 # kills slivers
  sf::st_sf(geom = sf::st_union(sf::st_geometry(x)))
}

get_comid_outlets <- function(o, flowline) {
  if(o[1] %in% c("comid", "COMID")) {
    f <- flowline[flowline$COMID == as.integer(o[2]), ][c("COMID", attr(flowline, "sf_column"))]
    names(f)[names(f) == "COMID"] <- "comid"
    return(make_point(f))
  }
  return(dplyr::tibble())
}

get_outlet_from_nldi <- function(outlet) {
  make_point(get_nldi_feature(outlet))
}

make_point <- function(x, crs = 4326) {
  sf::st_geometry(x) <-
    suppressWarnings(sf::st_centroid(sf::st_geometry(x)))
  x <- set_geom_name(x)
  x <- sf::st_transform(x, crs)
  return(dplyr::select(x, "comid"))
}

set_geom_name <- function(x, new_name = "geom") {
  g <- attr(x, "sf_column")
  names(x)[names(x) == g] <- new_name
  attr(x, "sf_column") <- new_name
  x
}

make_nwis_nldi_feature <- function(x) {
  if(length(x) > 1) stop("not and nwis outlet")
  if(!grepl("^USGS-.*", x)) x <- paste0("USGS-", x)
  if(!grepl("USGS-[0-9][0-9][0-9]", x)) stop("Found invalid NWIS ID trying to intperet outlet.")
  list(featureSource = "nwissite", featureID = x)
}

make_comid_nldi_feature <- function(x) {
  list(featureSource = "comid", featureID = as.character(x))
}

as_outlets <- function(o) {
  tryCatch({
    if(is.null(o)) return(NULL)

    if(!is.list(o) && all_int(o))
      return(list(subset = o))

    if((is.list(o) && all_int(o)) |
       is.character(o) |
       is.list(o) && !"sf" %in% class(o)) {
      return(tryCatch(lapply(o, individual_outlets),
                      error = function(e) list(individual_outlets(o))))

    }

    if("sf" %in% class(o)) {
      out <- c()
      for(i in seq_len(nrow(o))) {
        out <- c(out, make_comid_nldi_feature(discover_nhdplus_id(sf::st_geometry(o)[i])))
      }
    }
    return(list(check_nldi_feature(out)))
  }, error = function(f) {
    stop(paste0("Error trying to interpret outlet specification. Original error was:\n\n", f))
  })
}

# Works on individual outlets rather than sets of them.
individual_outlets <- function(o) {
  if(length(o) == 1 && all_int(o))
    return(make_comid_nldi_feature(o))

  if(is.character(o))
    o <- tryCatch({
      return(make_nwis_nldi_feature(o))
    }, error = function(f) as.list(o))


  if(is.list(o) && !"sf" %in% class(o)) {
    if(length(o) == 2 && !is.list(o[[1]])) o <- as.list(o)
    return(check_nldi_feature(o))
  }
}

all_int <- function(o) {
  tryCatch(all(sapply(o, function(x) x %% 1) == 0),
           error = function(f) FALSE)
}
