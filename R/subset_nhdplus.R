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
#' @param simplified boolean if TRUE (the default) the CatchmentSP layer
#' will be included. Not relevant to the "download" option or NHDPlusHR data.
#' @param overwrite boolean should the output file be overwritten
#' @param status boolean should the function print status messages
#' @details If \code{\link{stage_national_data}} has been run in the current
#' session, this function will use the staged national data automatically.
#'
#' This function relies on the National Seamless Geodatabase or Geopackage.
#' It can be downloaded
#' \href{https://www.epa.gov/waterdata/nhdplus-national-data}{here.}
#'
#' The "download" option of this function should be considered preliminary
#' and subject to revision. It does not include as many layers and may not
#' be available permenantly.
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
#' temp_dir <- tempdir()
#' temp_file <- tempfile(fileext = ".zip", tmpdir = temp_dir)
#' download.file("https://usgs-r.github.io/nhdplusTools/data/03_sub.zip",
#'               temp_file)
#' unzip(temp_file, exdir = temp_dir)
#'
#' hr_data <- get_nhdplushr(temp_dir,
#'                          out_gpkg = file.path(temp_dir, "nhd_hr.gpkg"),
#'                          layers = NULL)
#' flowlines <- sf::read_sf(hr_data, "NHDFlowline")
#'
#' up_ids <- get_UT(flowlines, 15000500028335)
#'
#' sub_nhdhr <- subset_nhdplus(up_ids, file.path(temp_dir, "sub.gpkg"),
#'                       hr_data, overwrite = TRUE)
#'
#' sf::st_layers(sub_nhdhr)
#'
#' sub_flowline <- sf::read_sf(sub_nhdhr, "NHDFlowline")
#' plot(sf::st_geometry(flowlines), lwd = 0.5)
#' plot(sf::st_geometry(sub_flowline), lwd = 0.6, col = "red", add = TRUE)
#' }
#'

subset_nhdplus <- function(comids, output_file, nhdplus_data = NULL,
                           simplified = TRUE, overwrite = FALSE, status = TRUE) {

  if (status) message("All intersections performed in latitude/longitude.")

  if (!grepl("*.gpkg$", output_file)) {
    stop("output_file must end in '.gpkg'")
  }

  if (file.exists(output_file) & !overwrite) {
    stop("output_file exists and overwrite is false.")
  } else if (file.exists(output_file) & overwrite) {
    unlink(output_file)
  }

  if (is.null(nhdplus_data)) {
    nhdplus_data <- nhdplus_path()
  }

  layer_name <- "NHDFlowline_Network"
  if (status) message(paste("Reading", layer_name))

  if (nhdplus_data == "download") {

    if (length(comids) > 3000) {
      warning("Download functionality not tested for this many comids")
    }

    fline <- get_nhdplus_byid(comids, tolower(layer_name))
  } else {

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

  if (grepl("*.rds$", fline_path)) {
    fline <- readRDS(fline_path)
  } else {
    if(!layer_name %in% st_layers(fline_path)$name) {
      layer_name <- "NHDFlowline"
    }
    fline <- sf::read_sf(fline_path, layer_name)
    fline <- rename_nhdplus(fline)
  }

  fline <- dplyr::filter(fline, .data$COMID %in% comids)

  }

  if (status) message(paste("Writing", layer_name))

  sf::write_sf(fline, output_file, layer_name)

  rm(fline)

  if (simplified) {
    layer_name <- "CatchmentSP"
  } else {
    layer_name <- "Catchment"
  }

  if (status) message(paste("Reading", layer_name))

  if (nhdplus_data == "download") {
    layer_name <- "CatchmentSP" # Need to handle SP and regular better!!
    catchment <- get_nhdplus_byid(comids, tolower(layer_name))
  } else {

  if (grepl("*.rds$", catchment_path)) {
    catchment <- readRDS(catchment_path)
  } else {
    if(!layer_name %in% st_layers(fline_path)$name) {
      layer_name <- "NHDPlusCatchment"
    }
    catchment <- sf::read_sf(catchment_path, layer_name)
    catchment <- rename_nhdplus(catchment)
  }

  catchment <- dplyr::filter(catchment, .data$FEATUREID %in% comids)

  }

  if (status) message(paste("Writing", layer_name))

  sf::write_sf(catchment, output_file, layer_name)

  envelope <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(catchment)),
                               4326)

  rm(catchment)

  if (nhdplus_data == "download") {
    layer_names <- c("NHDArea", "NHDWaterbody")

    for (layer_name in layer_names) {
      sf::st_transform(envelope, 4326) %>%
        get_nhdplus_bybox(layer = tolower(layer_name)) %>%
        sf::write_sf(output_file, layer_name)
    }

  } else {

  if("Gage" %in% st_layers(nhdplus_data)$name) {
    intersection_names <- c("Gage", "Sink", "NHDArea",
                            "NHDWaterbody", "NHDFlowline_NonNetwork")
  } else {
    intersection_names <- c("NHDWaterbody", "NHDArea", "NHDPlusSink")
    intersection_names <- intersection_names[which(intersection_names %in% st_layers(nhdplus_data)$name)]
  }

  invisible(lapply(intersection_names, intersection_write,
                   data_path = nhdplus_data,
                   envelope = envelope,
                   output_file = output_file,
                   status))

  }

  return(output_file)
}

intersection_write <- function(layer_name, data_path, envelope,
                               output_file, status) {

  if (status) message(paste("Reading", layer_name))
  layer <- sf::st_zm(sf::read_sf(data_path, layer_name))

  intersection_test <- c()

  try(intersection_test <- suppressMessages(sf::st_intersects(
    sf::st_transform(layer, 4326), envelope)), silent = TRUE)

  out <- dplyr::filter(layer, lengths(intersection_test) > 0)

  if (nrow(out) > 0) {
    if (status) message(paste("Writing", layer_name))
    sf::write_sf(out, output_file, layer_name)
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
#' as a seperate data.frame without the geometry. The others will save
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
                                     "NHDFlowline_Network"))
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
      if (simplified) {
        layer_name <- "CatchmentSP"
      } else {
        layer_name <- "Catchment"
      }
      saveRDS(sf::st_zm(sf::read_sf(nhdplus_data, layer_name)),
              out_path_catchments)
    }
    outlist["catchment"] <- out_path_catchments
  }
  assign("national_data", outlist, envir = nhdplusTools_env)

  return(outlist)
}
