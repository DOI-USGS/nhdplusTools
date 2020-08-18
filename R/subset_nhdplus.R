#' @title Subset NHDPlus
#' @description Saves a subset of the National Seamless database or other
#' nhdplusTools compatible data based on a specified collection of COMIDs.
#' @param comids integer vector of COMIDs to include.
#' @param output_file character path to save the output to defaults
#' to the directory of the nhdplus_data.
#' @param nhdplus_data character path to the .gpkg or .gdb containing
#' the national seamless database, a subset of NHDPlusHR,
#' or "download" to use a web service to download NHDPlusV2.1 data.
#' Not required if \code{\link{nhdplus_path}} has been set or the default
#' has been adopted. See details for more.
#' @param bbox object of class "bbox" as returned by sf::st_bbox in Latitude/Longitude.
#' If no CRS is present, will be assumed to be in WGS84 Latitude Longitude.
#' @param simplified boolean if TRUE (the default) the CatchmentSP layer
#' will be included. Not relevant to the "download" option or NHDPlusHR data.
#' @param overwrite boolean should the output file be overwritten
#' @param return_data boolean if FALSE path to output file is returned silently otherwise
#' data is returned in a list.
#' @param status boolean should the function print status messages
#' @param flowline_only boolean WARNING: experimental
#' if TRUE only the flowline network and attributes will be returned
#' @param streamorder integer only streams of order greater than or equal will be downloaded.
#' Not implemented for local data.
#' @details If \code{\link{stage_national_data}} has been run in the current
#' session, this function will use the staged national data automatically.
#'
#' This function relies on the National Seamless Geodatabase or Geopackage.
#' It can be downloaded
#' \href{https://www.epa.gov/waterdata/nhdplus-national-data}{here.}
#'
#' The "download" option of this function should be considered preliminary
#' and subject to revision. It does not include as many layers and may not
#' be available permenently.
#'
#' @return path to the saved subset geopackage
#' @export
#' @examples
#' \donttest{
#' sample_data <- system.file("extdata/sample_natseamless.gpkg",
#'                            package = "nhdplusTools")
#'
#' nhdplus_path(sample_data)
#'
#' staged_nhdplus <- stage_national_data(output_path = tempdir())
#'
#' sample_flines <- readRDS(staged_nhdplus$flowline)
#'
#' geom_col <- attr(sample_flines, "sf_column")
#'
#' plot(sample_flines[[geom_col]],
#'      lwd = 3)
#'
#' start_point <- sf::st_sfc(sf::st_point(c(-89.362239, 43.090266)),
#'                           crs = 4326)
#'
#' plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
#'
#' start_comid <- discover_nhdplus_id(start_point)
#'
#' comids <- get_UT(sample_flines, start_comid)
#'
#' plot(dplyr::filter(sample_flines, COMID %in% comids)[[geom_col]],
#'      add=TRUE, col = "red", lwd = 2)
#'
#' output_file <- tempfile(fileext = ".gpkg")
#'
#' subset_nhdplus(comids = comids,
#'                output_file = output_file,
#'                nhdplus_data = sample_data,
#'                overwrite = TRUE,
#'                status = TRUE)
#'
#' sf::st_layers(output_file)
#'
#' catchment <- sf::read_sf(output_file, "CatchmentSP")
#'
#' plot(catchment[[attr(catchment, "sf_column")]], add = TRUE)
#'
#' waterbody <- sf::read_sf(output_file, "NHDWaterbody")
#'
#' plot(waterbody[[attr(waterbody, "sf_column")]],
#'      col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)
#'
#' # Cleanup temp
#' sapply(staged_nhdplus, unlink)
#' unlink(output_file)
#'
#' # Download Option:
#' subset_nhdplus(comids = comids,
#'                output_file = output_file,
#'                nhdplus_data = "download",
#'                overwrite = TRUE,
#'                status = TRUE)
#'
#' sf::st_layers(output_file)
#'
#' # NHDPlusHR
#' source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))
#'
#' up_ids <- get_UT(hr_data$NHDFlowline, 15000500028335)
#'
#' sub_gpkg <- file.path(work_dir, "sub.gpkg")
#' sub_nhdhr <- subset_nhdplus(up_ids, output_file = sub_gpkg,
#'                             nhdplus_data = hr_gpkg, overwrite = TRUE)
#'
#' sf::st_layers(sub_gpkg)
#' names(sub_nhdhr)
#'
#' plot(sf::st_geometry(hr_data$NHDFlowline), lwd = 0.5)
#' plot(sf::st_geometry(sub_nhdhr$NHDFlowline), lwd = 0.6, col = "red", add = TRUE)
#'
#' unlink(output_file)
#' unlink(sub_gpkg)
#' }
#'

subset_nhdplus <- function(comids = NULL, output_file = NULL, nhdplus_data = NULL, bbox = NULL,
                           simplified = TRUE, overwrite = FALSE, return_data = TRUE, status = TRUE,
                           flowline_only = NULL, streamorder = NULL) {

  if(is.null(flowline_only)) {
    if(!is.null(nhdplus_data) && nhdplus_data == "download") {
      flowline_only <- TRUE
    } else {
      flowline_only <- FALSE
    }
  }

  if (status) message("All intersections performed in latitude/longitude.")

  if(any(bbox > 180 | bbox < -180)) stop("invalid bbox entry")

  if(!is.null(bbox) && nhdplus_data == "download") {
    x_range <- bbox[3] - bbox[1]
    y_range <- bbox[4] - bbox[2]

    if((x_range * y_range) > 10) {
      warning("Large bounding box submitted for download. Performance may be slow or unstable.")
    }
  }

  if(!is.null(output_file)) {
    if (!grepl("*.gpkg$", output_file)) {
      stop("output_file must end in '.gpkg'")
    }

    if (file.exists(output_file) & !overwrite) {
      stop("output_file exists and overwrite is false.")
    } else if (file.exists(output_file) & overwrite) {
      unlink(output_file)
    }
  }

  if (is.null(nhdplus_data)) {
    nhdplus_data <- nhdplus_path()
  }

  paths <- get_staged_data(nhdplus_data)

  if(is.null(bbox)) {
    if(is.null(comids)) stop("must provide comids or bounding box")

    out_list <- c(get_flowline_subset(nhdplus_data, comids,
                                      output_file, paths$fline_path,
                                      status))

    if(!flowline_only) {

      out_list <- c(out_list, get_catchment_subset(nhdplus_data, comids,
                                                   output_file, simplified,
                                                   paths$catchment_path, status))

      catch_layer <- get_catchment_layer_name(simplified, nhdplus_data)

      envelope <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(out_list[[catch_layer]])),
                                   4326)

      intersection_names <- c("NHDArea", "NHDWaterbody")
    } else {
      intersection_names <- c()
    }

  } else {
    out_list <- list()

    if(!is.null(comids)) warning("using bounding box rather than submitted comids")

    if(!is.null(attr(bbox, "crs"))) {
      envelope <- sf::st_transform(sf::st_as_sfc(bbox),
                                   4326)
    } else {
      if((length(bbox) != 4 | !is.numeric(bbox)) |
         (!(all(bbox >= -180) & all(bbox <= 180)))) stop("invalid bbox entry")
      names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
      bbox <- sf::st_bbox(bbox, crs = sf::st_crs(4326))
      envelope <- sf::st_as_sfc(bbox)
    }

    intersection_names <- c(get_catchment_layer_name(simplified, nhdplus_data),
                            get_flowline_layer_name(nhdplus_data),
                            "NHDArea", "NHDWaterbody")

    if(flowline_only) intersection_names <- get_flowline_layer_name(nhdplus_data)
  }

  if (nhdplus_data == "download") {

    for (layer_name in intersection_names) {
      layer <- sf::st_transform(envelope, 4326) %>%
        get_nhdplus_bybox(layer = tolower(layer_name), streamorder = streamorder)

      if(return_data) {
        out_list[layer_name] <- list(layer)
      }

      if(!is.null(output_file)) {
        sf::write_sf(clean_bbox(layer), output_file, layer_name)
      }
    }

  } else {
    if(!flowline_only) {
      if("Gage" %in% st_layers(nhdplus_data)$name) {
        intersection_names <- c(intersection_names, "Gage", "Sink", "NHDFlowline_NonNetwork")
      } else {
        intersection_names <- c(intersection_names, "NHDPlusSink")
        intersection_names <- intersection_names[which(intersection_names %in% st_layers(nhdplus_data)$name)]
      }
    }

    out_list <- c(out_list,
                  stats::setNames(lapply(intersection_names, intersection_write,
                                         data_path = nhdplus_data,
                                         envelope = envelope,
                                         output_file = output_file,
                                         status), intersection_names))
  }

  if(return_data) return(out_list)

  return(output_file)
}

intersection_write <- function(layer_name, data_path, envelope,
                               output_file, status) {
  out_list <- list()

  if (status) message(paste("Reading", layer_name))
  layer <- sf::st_zm(sf::read_sf(data_path, layer_name))

  intersection_test <- c()

  try(intersection_test <- suppressMessages(sf::st_intersects(
    sf::st_transform(layer, 4326), envelope)), silent = TRUE)

  out <- dplyr::filter(layer, lengths(intersection_test) > 0)

  if (nrow(out) > 0) {
    if (status) message(paste("Writing", layer_name))
    if(is.null(output_file)) {
      return(out)
    } else {
      sf::write_sf(clean_bbox(out), output_file, layer_name)
      return(layer_name)
    }
  } else {
    if (status) message(paste("No features to write in", layer_name))
  }
}

#' @title Stage NHDPlus National Data
#' @description Breaks down the national geo database into a collection
#' of quick to access R binary files.
#' @param include character vector containing one or more of:
#' "attributes", "flowline", "catchment".
#' @param output_path character path to save the output to defaults
#' to the directory of the nhdplus_data.
#' @param nhdplus_data character path to the .gpkg or .gdb
#' containing the national seamless dataset. Not required if
#' \code{\link{nhdplus_path}} has been set.
#' @param simplified boolean if TRUE (the default) the CatchmentSP layer
#' will be included.
#' @details "attributes" will save `NHDFlowline_Network` attributes
#' as a separate data.frame without the geometry. The others will save
#' the `NHDFlowline_Network` and `Catchment` or `CatchmentSP`
#' (per the `simplified` parameter) as sf data.frames with
#' superfluous Z information dropped.
#'
#' The returned list of paths is also added to the nhdplusTools_env
#' as "national_data".
#'
#' @return list containing paths to the .rds files.
#' @export
#' @examples
#' sample_data <- system.file("extdata/sample_natseamless.gpkg",
#'                            package = "nhdplusTools")
#'
#' stage_national_data(nhdplus_data = sample_data, output_path = tempdir())
#'
stage_national_data <- function(include = c("attribute",
                                            "flowline",
                                            "catchment"),
                                output_path = NULL,
                                nhdplus_data = NULL,
                                simplified = TRUE) {

  if (is.null(output_path)) {
    output_path <- dirname(nhdplus_path())
    warning(paste("No output path provided, using:", output_path))
  }

  if (is.null(nhdplus_data)) {
    nhdplus_data <- nhdplus_path()

    if (nhdplus_data == get("default_nhdplus_path",
                            envir = nhdplusTools_env) &
        !file.exists(nhdplus_data)) {
      stop(paste("Didn't find NHDPlus national data in default location:",
                 nhdplus_data))
    } else if (!file.exists(nhdplus_data)) {
      stop(paste("Didn't find NHDPlus national data",
                 "in user specified location:",
                 nhdplus_data))
    }
  }

  allow_include <- c("attribute", "flowline", "catchment")

  if (!all(include %in% allow_include)) {
    stop(paste0("Got invalid include entries. Expect one or more of: ",
                paste(allow_include, collapse = ", "), "."))
  }

  outlist <- list()

  if (any(c("attribute", "flowline") %in% include)) {

    out_path_attributes <- file.path(output_path,
                                     "nhdplus_flowline_attributes.rds")
    out_path_flines <- file.path(output_path, "nhdplus_flowline.rds")

    if (!(file.exists(out_path_flines) | file.exists(out_path_attributes))) {
      fline <- sf::st_zm(sf::read_sf(nhdplus_data,
                                     get_flowline_layer_name(nhdplus_data)))
    }

    if ("attribute" %in% include) {
      if (file.exists(out_path_attributes)) {
        warning("attributes file exists")
      } else {
        saveRDS(sf::st_set_geometry(fline, NULL), out_path_attributes)
      }
      outlist["attributes"] <- out_path_attributes
    }

    if ("flowline" %in% include) {
      if (file.exists(out_path_flines)) {
        warning("flowline file exists")
      } else {
        saveRDS(fline, out_path_flines)
      }
      outlist["flowline"] <- out_path_flines
    }
  }

  if (exists("fline")) rm(fline)

  if ("catchment" %in% include) {
    out_path_catchments <- file.path(output_path, "nhdplus_catchment.rds")
    if (file.exists(out_path_catchments)) {
      warning("catchment already exists.")
    } else {

      layer_name <- get_catchment_layer_name(simplified, nhdplus_data)

      saveRDS(sf::st_zm(sf::read_sf(nhdplus_data, layer_name)),
              out_path_catchments)
    }
    outlist["catchment"] <- out_path_catchments
  }
  assign("national_data", outlist, envir = nhdplusTools_env)

  return(outlist)
}

#' @title Try to find staged NHDPlus data
#' @noRd
get_staged_data <- function(nhdplus_data) {

  if(nhdplus_data == "download") return(list(fline_path = NULL, catchment_path = NULL))

  staged_data <- try(get("national_data",
                         envir = nhdplusTools_env),
                     silent = TRUE)

  if (is.list(staged_data)) {
    if (all(c("flowline", "catchment") %in% names(staged_data)) &
        file.exists(staged_data$flowline) &
        file.exists(staged_data$catchment)) {
      fline_path <- staged_data$flowline
      catchment_path <- staged_data$catchment
    } else {
      fline_path <- nhdplus_data
      catchment_path <- nhdplus_data
    }
  } else if(file.exists(nhdplus_data)) {
    fline_path <- nhdplus_data
    catchment_path <- nhdplus_data
  } else {
    stop("couldn't find nhdplus data")
  }
  return(list(fline_path = fline_path, catchment_path = catchment_path))
}

#' @title Get subset of flowline data later.
#' @noRd
get_flowline_subset <- function(nhdplus_data, comids, output_file,
                                fline_path, status) {

  layer_name <- get_flowline_layer_name(nhdplus_data)

  if (status) message(paste("Reading", layer_name))

  if (nhdplus_data == "download") {

    if (length(comids) > 3000) {
      warning("Download functionality not tested for this many comids")
    }

    fline <- get_nhdplus_byid(comids, tolower(layer_name))
  } else {

    if (grepl("*.rds$", fline_path)) {
      fline <- readRDS(fline_path)
    } else {
      if(!layer_name %in% st_layers(fline_path)$name) {
        layer_name <- "NHDFlowline"
      }
      fline <- sf::read_sf(fline_path, layer_name)
      fline <- align_nhdplus_names(fline)
    }

    fline <- dplyr::filter(fline, .data$COMID %in% comids)
  }

  if (status) message(paste("Writing", layer_name))

  if(!is.null(output_file)) {
    sf::write_sf(clean_bbox(fline), output_file, layer_name)
  }
  out <- list()
  out[layer_name] <- list(fline)
  return(out)
}

#' @title Get subset of catchment data layer.
#' @noRd
get_catchment_subset <- function(nhdplus_data, comids, output_file,
                                 simplified, catchment_path, status) {

  layer_name <- get_catchment_layer_name(simplified, nhdplus_data)

  if (status) message(paste("Reading", layer_name))

  if (nhdplus_data == "download") {

    catchment <- get_nhdplus_byid(comids, tolower(layer_name))

  } else {

    if (grepl("*.rds$", catchment_path)) {
      catchment <- readRDS(catchment_path)
    } else {
      catchment <- sf::read_sf(catchment_path, layer_name)
      catchment <- align_nhdplus_names(catchment)
    }

    catchment <- dplyr::filter(catchment, .data$FEATUREID %in% comids)

  }

  if (status) message(paste("Writing", layer_name))

  if(!is.null(output_file)) {
    sf::write_sf(clean_bbox(catchment), output_file, layer_name)
  }
  out <- list()
  out[layer_name] <- list(catchment)
  return(out)
}

clean_bbox <- function(x) {
  if("bbox" %in% names(x) && class(x$bbox[1]) == "list") {
    x$bbox <- sapply(x$bbox, paste, collapse = ",")
  }
  return(x)
}

get_catchment_layer_name <- function(simplified, nhdplus_data) {
  if(is.null(nhdplus_data) || nhdplus_data == "download") { # Only simplified via download
    layer_name <- "CatchmentSP"
  } else {
    if(simplified) { # Can get simplified from local data
      layer_name <- "CatchmentSP"
    } else { # Has to be full catchment
      layer_name <- "Catchment"
    }
    if(!layer_name %in% sf::st_layers(nhdplus_data)$name) # unless it's high res.
      layer_name <- "NHDPlusCatchment"
  }
  return(layer_name)
}

get_flowline_layer_name <- function(nhdplus_data) {
  layer_name <- "NHDFlowline_Network"
  if(nhdplus_data != "download" &&
     !is.null(nhdplus_data) &&
     !layer_name %in% sf::st_layers(nhdplus_data)$name) # unless it's high res.
    layer_name <- "NHDFlowline"
  layer_name
}

#' Subset by Raster Processing Unit.
#' @description Given flowlines and an rpu_code, performs a network-safe subset such
#' that the result can be used in downstream processing. Has been tested to work
#' against the entire NHDPlusV2 domain and satisfies a number of edge cases.
#' @param fline sf data.frame NHD Flowlines with COMID, Pathlength, LENGTHKM, and Hydroseq.
#' LevelPathI, RPUID, ToNode, FromNode, and ArbolateSu.
#' @param rpu character e.g. "01a"
#' @param run_make_standalone boolean should the run_make_standalone function be run on result?
#' @export
#' @importFrom dplyr filter arrange summarize
#' @importFrom sf st_sf st_drop_geometry
#' @examples
#' sample_data <- system.file("extdata/sample_natseamless.gpkg",
#'                            package = "nhdplusTools")
#'
#' nhdplus_path(sample_data)
#'
#' staged_nhdplus <- stage_national_data(output_path = tempdir())
#'
#' sample_flines <- readRDS(staged_nhdplus$flowline)
#'
#' subset_rpu(sample_flines, rpu = "07b")
subset_rpu <- function(fline, rpu, run_make_standalone = TRUE) {
  # Find all outlets of current rpu and sort by size
  # !ToNode %in% FromNode finds non-terminal flowlines that exit the domain.
  outlets <- filter(fline, .data$RPUID %in% rpu)

  outlets <- st_sf(filter(outlets, .data$TerminalFl == 1 |
                                !.data$ToNode %in% .data$FromNode))

  outlets <- arrange(outlets, desc(.data$ArbolateSu))

  # run nhdplusTools::get_UT for all outlets and concatenate.
  network <- lapply(outlets$COMID,
                    function(x, fline) get_UT(fline, x),
                    fline = fline)
  network <- do.call(c, network)

  # Filter so only navigable flowlines are included.
  fline <- fline[fline$COMID %in% network, ]

  # For flowlines labaled as in the RPU, find the top and bottom of each
  # LevelPath. This was required for some unique network situations.
  fline_sub <- filter(drop_geometry(fline), .data$RPUID %in% rpu)

  fline_sub <- group_by(fline_sub, .data$LevelPathI)

  fline_sub <- summarize(fline_sub,
                         lp_top = max(.data$Hydroseq),
                         lp_bot = min(.data$Hydroseq))

  # Using the levelpath top and bottoms found above, filter the complete
  # domain to the hydrosequence of the levelpath top and bottoms instead
  # of trusting the RPUID to be useable.
  fline <- left_join(fline, fline_sub, by = "LevelPathI")

  fline <- group_by(filter(fline, .data$LevelPathI %in% fline_sub$LevelPathI),
                    .data$LevelPathI)

  fline <- ungroup(filter(fline, .data$Hydroseq >= .data$lp_bot &
                            .data$Hydroseq <= .data$lp_top))

  if(run_make_standalone) {
    make_standalone(fline)
  } else {
    fline
  }
}

drop_geometry <- function(x) {
  if("sf" %in% class(x)) {
    sf::st_drop_geometry(x)
  } else {
    x
  }
}

