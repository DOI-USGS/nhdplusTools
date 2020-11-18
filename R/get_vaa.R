#' @title File path to VAA Cache
#' @description nhdplusTools will download and cache an fst file with NHDPlusV2 attribute data sans geometry. This function returns the file path to the cached file.
#' @return file.path (character)
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

get_vaa_names <- function(){
  # Build with: colnames(get_vaa())
  c("comid", "fdate", "streamleve", "streamorde", "streamcalc",
    "fromnode", "tonode", "hydroseq", "levelpathi", "pathlength",
    "terminalpa", "arbolatesu", "divergence", "startflag", "terminalfl",
    "dnlevel", "thinnercod", "uplevelpat", "uphydroseq", "dnlevelpat",
    "dnminorhyd", "dndraincou", "dnhydroseq", "frommeas", "tomeas",
    "reachcode", "lengthkm", "fcode", "rtndiv", "outdiv", "diveffect",
    "areasqkm", "totdasqkm", "divdasqkm", "tidal", "totma", "wbareatype",
    "pathtimema", "slope", "slopelenkm")
}

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
#' @return path to cached data
#' @export
#' @importFrom httr GET progress write_disk

download_vaa <- function() {

  if (file.exists(vaa_path())) {
    message("File already cached")
  } else {
    dir.create(dirname(vaa_path()), showWarnings = FALSE, recursive = TRUE)

    resp <- httr::GET(vaa_hydroshare,
                      httr::write_disk(vaa_path(), overwrite = TRUE),
                      httr::progress())

    if (resp$status_code != 200) {
      stop("Download unsuccessfull :(")
    }
  }
  # return file path
  return(vaa_path())
}


