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
#' @description Return requested NHDPlusv2 Attributes.
#' @inherit download_vaa details
#' @param atts character The variable names you would like, always includes comid
#' @param path character path where the file should be saved. Default is a
#' persistent system data as retrieved by \link{nhdplusTools_data_dir}.
#' Also see: \link{get_vaa_path}
#' @param download logical if TRUE, the default, will download VAA table if not
#' found at path.
#' @param updated_network logical default FALSE. If TRUE, updated network attributes
#' from E2NHD and National Water Model retrieved from
#' \href{https://www.sciencebase.gov/catalog/item/60c92503d34e86b9389df1c9}{here.}
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

#' @title Download nhdplusVAA data from HydroShare
#' @description downloads and caches nhdplusVAA data on your computer
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

download_vaa <- function(path = get_vaa_path(), force = FALSE, updated_network = FALSE) {

  if (file.exists(path) & !force) {
    message("File already cached")
  } else {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

    resp <- httr::GET(ifelse(updated_network, vaa_sciencebase, vaa_hydroshare),
                      httr::write_disk(path, overwrite = TRUE),
                      httr::progress())

    if (resp$status_code != 200) {
      stop("Download unsuccessfull :(")
    }
  }
  # return file path
  return(path)
}



