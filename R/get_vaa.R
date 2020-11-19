#' @title File path to value added attribute (vaa) Cache
#' @description nhdplusTools will download and cache an `fst` file with
#' NHDPlusV2 attribute data sans geometry. This function returns the
#' file path to the cached file. Will use the user cache dir indicated
#' by \link[rappdirs]{user_cache_dir}.
#'
#' @return file.path character
#' @importFrom rappdirs user_cache_dir
#' @export
#' @examples
#' get_vaa_path()

get_vaa_path <- function() {
  user_cache_dir("nhdplus-vaa/nhdplusVAA.fst")
}

#' @title Available NHDPlusV2 Attributes
#' @description Find variables available from the NHDPlusV2 attribute data.frame
#' @return character vector
#' @examples
#' get_vaa_names()
#' @export
#' @examples
#' get_vaa_names()

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
#' @param atts character The variable names you would like, always includes comid
#' @param path character path where the file should be saved. Default is a
#' persistent system cache as retrieved by \link[rappdirs]{user_cache_dir}.
#' Also see: \link{get_vaa_path}
#' @param download logical if TRUE, the default, will download VAA table if not
#' found at path.
#' @return data.frame
#' @export
#' @examples
#' \dontrun{
#' # NOTE: path = tempfile() for demo only.
#' temp <- tempfile()
#' get_vaa("slope", path = temp)
#' get_vaa(c("slope", "lengthkm"), path = temp)
#' }
#' @importFrom fst read.fst

get_vaa <- function(atts = NULL, path = get_vaa_path(),
                    download = TRUE) {

  if(!file.exists(path)){
    if(download) {
      message("didn't find data, downloading.")
      path <- download_vaa(path = path)
    } else {
      stop("need to download data: run `download_vaa()`")
    }
  }

  if(is.null(atts)){
    return(fst::read.fst(path))
  }

  if(all(atts %in% get_vaa_names())){
    return(fst::read.fst(path, c('comid', atts)))
  }

}

#' @title Download nhdplusVAA data from HydroShare
#' @description downloads and caches data on your computer
#' @inheritParams  get_vaa
#' @return path to cached data
#' @export
#' @importFrom httr GET progress write_disk
#'
download_vaa <- function(path = get_vaa_path()) {

  if (file.exists(path)) {
    message("File already cached")
  } else {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

    resp <- httr::GET(vaa_hydroshare,
                      httr::write_disk(path, overwrite = TRUE),
                      httr::progress())

    if (resp$status_code != 200) {
      stop("Download unsuccessfull :(")
    }
  }
  # return file path
  return(path)
}


