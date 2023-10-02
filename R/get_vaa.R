#' @title File path to value added attribute (vaa) Cache
#' @description nhdplusTools will download and cache an `fst` file with
#' NHDPlusV2 attribute data sans geometry. This function returns the
#' file path to the cached file. Will use the user data dir indicated
#' by \link{nhdplusTools_data_dir}.
#' @param updated_network logical default FALSE. If TRUE, returns path to updated
#' network parameters. See \link{get_vaa} for more.
#' @inherit download_vaa details
#' @return character file path
#' @export
#' @examples
#' get_vaa_path()
#'
#' get_vaa_path(updated_network = TRUE)
#'

get_vaa_path <- function(updated_network = FALSE) {
  if(updated_network) {
    file.path(nhdplusTools_data_dir(), "enhd_nhdplusatts.fst")
  } else {
    file.path(nhdplusTools_data_dir(), "nhdplusVAA.fst")
  }
}

#' @title Available NHDPlusV2 Attributes
#' @description Find variables available from the NHDPlusV2 attribute data.frame
#' @inherit download_vaa details
#' @inheritParams get_vaa
#' @return character vector
#' @importFrom fst metadata_fst
#' @export
#' @examples
#' \dontrun{
#' # This will download the vaa file to the path from get_vaa_path()
#' get_vaa_names()
#'
#' #cleanup if desired
#' unlink(dirname(get_vaa_path()), recursive = TRUE)
#' }
get_vaa_names <- function(updated_network = FALSE) {
  path <- get_vaa_path(updated_network = updated_network)

  check_vaa_path(path, TRUE)

  fst::metadata_fst(path)[["columnNames"]]
}

#' @title NHDPlusV2 Attribute Subset
#' @description Return requested NHDPlusV2 Attributes.
#' @inherit download_vaa details
#' @param atts character The variable names you would like, always includes comid
#' @param path character path where the file should be saved. Default is a
#' persistent system data as retrieved by \link{nhdplusTools_data_dir}.
#' Also see: \link{get_vaa_path}
#' @param download logical if TRUE, the default, will download VAA table if not
#' found at path.
#' @param updated_network logical default FALSE. If TRUE, updated network attributes
#' from E2NHD and National Water Model retrieved from
#' \doi{10.5066/P976XCVT}.
#' @return data.frame containing requested VAA data
#' @importFrom fst read.fst
#' @export
#' @examples
#' \dontrun{
#' # This will download the vaa file to the path from get_vaa_path()
#'
#' get_vaa("slope")
#' get_vaa(c("slope", "lengthkm"))
#'
#' get_vaa(updated_network = TRUE)
#' get_vaa("reachcode", updated_network = TRUE)
#'
#' #cleanup if desired
#' unlink(dirname(get_vaa_path()), recursive = TRUE)
#' }

get_vaa <- function(atts = NULL,
                    path = get_vaa_path(),
                    download = TRUE,
                    updated_network = FALSE) {

  check_vaa_path(path, download, FALSE)

  if(updated_network) {
    updated_net_path <- file.path(dirname(path), "enhd_nhdplusatts.fst")

    check_vaa_path(updated_net_path, download, TRUE)

    new_names <- fst::metadata_fst(updated_net_path)[["columnNames"]]
  }

  available_names = get_vaa_names(updated_network)

  if(is.null(atts)) {

    atts <- available_names

  } else {

    bad_atts = atts[!atts %in% available_names]
    atts      = atts[atts %in% available_names]
    if(length(bad_atts) > 0){

      message(paste(bad_atts, collapse = ", "), " not in vaa data. Ignoring...")

    }

  }

  if(updated_network) {

    message("Caution: updated attributes drop some catchments and attributes")

    deprecated_names <- c("streamcalc", "fromnode", "tonode",
                          "dnlevel", "uplevelpat", "uphydroseq",
                          "dnminorhyd", "divdasqkm")

    include_names <- c("comid",
                       atts[!atts %in%
                              c(deprecated_names, new_names)])

    replace_names <- atts[atts %in% new_names & !atts %in% deprecated_names]

    # Grab the original vaas but not the ones we are going to replace.
    out <- fst::read.fst(path, include_names)

    # grab all the new attributes.
    new_comid <- fst::read.fst(updated_net_path, "comid")

    # reorder out to match new -- also drop stuff missing from new.
    out <- out[match(new_comid$comid, out$comid), , drop = FALSE]

    out <- cbind(out, fst::read.fst(updated_net_path,
                                    c(replace_names[replace_names != "comid"])))

    reorder <- match(get_vaa_names(updated_network), names(out))

    reorder <- reorder[!is.na(reorder)]

    return(out[, reorder])

  } else {

    return(fst::read_fst(path, c('comid', atts[atts != 'comid'])))
  }

}

check_vaa_path <- function(path, download, updated_network = FALSE) {
  if(!file.exists(path)){
    if(download) {
      message("didn't find data, downloading.")
      path <- download_vaa(path, updated_network = updated_network)
    } else {
      stop("need to download data: run `download_vaa()`")
    }
  }
}

#' @title Download NHDPlusVAA data from HydroShare
#' @description downloads and caches NHDPlusVAA data on your computer
#' @details The VAA data is a aggregate table of information from the NHDPlusV2
#' elevslope.dbf(s), PlusFlowlineVAA.dbf(s); and NHDFlowlines. All data
#' originates from the EPA NHDPlus Homepage
#' \href{https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data}{here}.
#' To see the location of cached data on your machine use
#' \code{\link{get_vaa_path}}.
#' To view aggregate data and documentation, see
#' \href{https://www.hydroshare.org/resource/6092c8a62fac45be97a09bfd0b0bf726/}{here}
#' @inheritParams get_vaa
#' @param force logical. Force data re-download. Default = FALSE
#' @return character path to cached data
#' @export
#' @importFrom httr GET progress write_disk

download_vaa <- function(path = get_vaa_path(updated_network), force = FALSE, updated_network = FALSE) {

  if (file.exists(path) & !force) {
    message("File already cached")
  } else {
    url <- ifelse(updated_network, vaa_sciencebase, vaa_hydroshare)

    if(nhdplus_debug()) {
      message(url)
    }

    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

    resp <- httr::GET(url,
                      httr::write_disk(path, overwrite = TRUE),
                      httr::progress())

    if (resp$status_code != 200) {
      stop("Download unsuccessfull :(")
    }
  }
  # return file path
  return(path)
}

#' Get catchment characteristics metadata table
#' @description Download and cache table of catchment characteristics.
#'
#' Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes
#' for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream
#' Watersheds for the Conterminous United States (ver. 3.0, January 2021): U.S.
#' Geological Survey data release, \doi{10.5066/F7765D7V}.
#' @param search character string of length 1 to free search the metadata table.
#' If no search term is provided the entire table is returned.
#' @param cache logical should cached metadata be used?
#' @importFrom utils read.delim
#' @export
#' @examples
#' get_characteristics_metadata()
get_characteristics_metadata <- function(search, cache = TRUE) {

  out <- tryCatch({
    u <- "https://prod-is-usgs-sb-prod-publish.s3.amazonaws.com/5669a79ee4b08895842a1d47/metadata_table.tsv"

    f <- file.path(nhdplusTools_data_dir(), "metadata_table.tsv")
    r <- file.path(nhdplusTools_data_dir(), "metadata_table.rds")

    if(!cache) unlink(r, force = TRUE)

    if(file.exists(r)) {
      out <- readRDS(r)
    } else {

      if(!dir.exists(dirname(f))) dir.create(dirname(f), recursive = TRUE)

      if(!file.exists(f)) resp <- httr::RETRY("GET", u, httr::write_disk(f))

      out <- read.delim(f, sep = "\t")

      saveRDS(out, r)

      unlink(f)

      out
    }
  }, error = function(e) {
    NULL
  })

  if(is.null(out)) warning("Problem getting metadata, no internet?")

  if(!missing(search)) {
    return(out[unique(unlist(
      mapply(grep, search, out, ignore.case = TRUE)
    )), ])
  }
  out
}

#' Get Catchment Characteristics
#' @description Downloads (subsets of) catchment characteristics from a cloud data
#' store. See \link{get_characteristics_metadata} for available characteristics.
#'
#' Source:
#' Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes
#' for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream
#' Watersheds for the Conterminous United States (ver. 3.0, January 2021): U.S.
#' Geological Survey data release, \doi{10.5066/F7765D7V}.
#'
#' @param varname character vector of desired variables. If repeated varnames
#' are provided, they will be downloaded once but duplicated in the output.
#' @param ids numeric vector of identifiers (comids) from the specified fabric
#' @param reference_fabric (not used) will be used to allow future specification
#' of alternate reference fabrics
#' @importFrom dplyr bind_rows filter select everything collect
#' @importFrom arrow s3_bucket open_dataset
#' @export
#' @examples
#' \donttest{
#'   get_catchment_characteristics("CAT_BFI", c(5329343, 5329427))
#' }
get_catchment_characteristics <- function(varname, ids, reference_fabric = "nhdplusv2"){

  metadata <- get_characteristics_metadata()

  out <- tryCatch({
    lapply(unique(varname), function(x) {
      if(!x %in% metadata$ID) stop(paste("Variable", x, "not found in metadata."))

      i <- metadata[metadata$ID == x,]

      if(nrow(i) > 1) warning(paste("multiple attributes found for variable,", x, "using the first one."))

      i <- i[1,]

      ds <- open_dataset(i$s3_url)

      sub <- filter(select(ds, any_of(c("COMID", x, "percent_nodata"))),
                           .data$COMID %in% ids)

      att <- collect(sub)

      att$characteristic_id <- x

      att <- dplyr::mutate_all(att, ~ifelse(. == -9999, NA, .))

      if(!"percent_nodata" %in% names(att)) {
        att$percent_nodata <- 0
      }

      att <- mutate(att, percent_nodata = ifelse(is.na(.data[[i$ID]]), 100, .data$percent_nodata))

      distinct(
      select(att, all_of(c(characteristic_id = "characteristic_id", comid = "COMID",
                           characteristic_value = x, percent_nodata = "percent_nodata"))))
    })
  }, error = function(e) {
    e
  })

  df <- sapply(out, is.data.frame)

  if(!all(df)) {
    warning(paste0("Issue getting characteristics. Error was: \n",
                   out[!df]))
    return(NULL)
  }

  names(out) <- unique(varname)

  varname <- stats::setNames(make.unique(varname, sep = "||"), varname)

  out <- lapply(1:length(varname), function(i) {
    out <- out[names(varname)[i]]

    out[[1]]$characteristic_id <- varname[[i]]

    out[[1]]
  })

  bind_rows(out)
}
