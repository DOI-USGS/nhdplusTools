# Primary HydroShare Data Resource
vaa_hydroshare <-
  'https://www.hydroshare.org/resource/6092c8a62fac45be97a09bfd0b0bf726/data/contents/nhdplusVAA.fst'

# Where to cache the data:
#' @title VAA Cache location
#' @description Location of base fst nhdplusVAA file
#' @return file.path
#' @importFrom rappdirs user_cache_dir
#' @export

vaa_path <- function() {
  rappdirs::user_cache_dir("nhdplus-vaa/nhdplusVAA.fst")
}

#' @title Available NHDPlusV2 Attributes
#' @description Find variables available in the VAA data.frame
#' @return character vector
#' @examples
#' get_vaa_names()
#' @export

get_vaa_names <- function(){ nhdplusTools::attributes }

#' @title NHDPlusV2 Attribute Subset
#' @description Return requested NHDPlusv2 Attributes
#' @param atts The variable names you would like, always includes comid
#' @return data.frame
#' @export
#' @examples
#' get_vaa("slope")
#' get_vaa(c("slope", "lengthkm"))
#' @importFrom fst read.fst

get_vaa <- function(atts = NULL){

  if(!file.exists(vaa_path())){
    stop("need to download data: run `download_vaa()`")
  }

  if(is.null(atts)){
    return(fst::read.fst(vaa_path()))
  }

  if(all(atts %in% get_vaa_names())){
    return(fst::read.fst(vaa_path(), c('comid', atts)))
  }

}

#' @title Download nhdplusVAA data from HydroShare
#' @description downloads and caches data on your computer
#' @param url Don't mess with this :)
#' @return path to cached data
#' @export
#' @importFrom httr GET progress write_disk

download_vaa <- function(url = vaa_hydroshare) {

  if (file.exists(vaa_path())) {
    message("File already cached")
  } else {
    dir.create(dirname(vaa_path()), showWarnings = FALSE, recursive = TRUE)

    resp <- httr::GET(url,
                      httr::write_disk(vaa_path(), overwrite = TRUE),
                      httr::progress())

    if (resp$status_code != 200) {
      stop("Download unsuccessfull :(")
    }
  }
  # return file path
  return(vaa_path())
}

#' @title NHDPlusV2 attribute names
#' @name attributes
#' @docType data

NULL

