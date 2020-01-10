#' @title Download seamless National Hydrography Dataset Version 2 (NHDPlusV2)
#' @description This function downloads and decompresses staged seamless NHDPlusV2 data.
#' The following requirements are needed: p7zip (MacOS), 7zip (windows) Please see:
#' https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data
#' for more information and metadata about this data.
#' @param outdir The folder path where data should be downloaded and extracted
#' @param url the location of the online resource
#' @return the path to the local geodatabase
#' @export
#' @examples
#' \dontrun{
#'   download_nhdplusV2("./data/nhd/")
#' }

download_nhdplusv2 <- function(outdir,
                               url = paste0("https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/",
                                            "Data/NationalData/NHDPlusV21_NationalData_Seamless",
                                            "_Geodatabase_Lower48_07.7z")) {

  file <- downloader(outdir, url, "nhdplusV2")

  check7z()

  message("Extracting data ...")

  ifelse(any(grepl("gdb", list.dirs(outdir))),
         1,
         system(paste0("7z -o", path.expand(outdir), " x ", file), intern = TRUE))

  path <- list.dirs(outdir)[grepl("gdb", list.dirs(outdir))]
  path <- path[grepl("NHDPlus", path)]

  message(paste("NHDPlusV2 data extracted to:", path))

  return(invisible(path))
}

#' @title Download the seamless Watershed Boundary Dataset (WBD)
#' @description This function downloads and decompresses staged seamless WBD data.
#' Please see:
#' https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.xml
#' for metadata.
#' @param outdir The folder path where data should be downloaded and extracted
#' @param url the location of the online resource
#' @return the path to the local geodatabase
#' @export
#' @importFrom utils unzip
#' @examples
#' \dontrun{
#'   download_wbd("./data/wbd/")
#' }

download_wbd <- function(outdir,
                         url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/",
                                      "Hydrography/WBD/National/GDB/National_WBD_GDB.zip")) {

  file <- downloader(outdir, url, "WBD")

  message("Extracting data ...")

  suppressWarnings(utils::unzip(file, exdir = outdir, overwrite = F))

  path <- list.dirs(outdir)[grepl("gdb", list.dirs(outdir))]
  path <- path[grepl("WBD", path)]

  message(paste("WBD data extracted to:", path))

  return(invisible(path))
}

#' @title Download the seamless Reach File (RF1) Database
#' @description This function downloads and decompresses staged RF1 data.
#' See: https://water.usgs.gov/GIS/metadata/usgswrd/XML/erf1_2.xml for metadata.
#' @param outdir The folder path where data should be downloaded and extracted
#' @param url the location of the online resource
#' @return the path to the local e00 file
#' @export
#' @importFrom utils unzip
#' @examples
#' \dontrun{
#'   download_wbd("./data/rf1/")
#' }

download_rf1 <- function(outdir,
                         url = "https://water.usgs.gov/GIS/dsdl/erf1_2.e00.gz"){

  file <- downloader(outdir, url, "RF1")

  message("Extracting data ...")

  R.utils::gunzip(file, remove = FALSE, skip = T)

  path     <- list.files(outdir, full.names = T)[!grepl("gz", list.files(outdir))]
  path     <- path[grepl("rf1", path)]

  message(paste("RF1 data extracted to:", path))

  return(invisible(path))

}

#' @title Function to download data from URL to out directory using httr.
#' @description General downloader
#' @param dir path to output directory
#' @param url the location of the online resource
#' @param type the type of data being downloaded
#' @return the downloaded file path
#' @importFrom httr GET write_disk progress
#' @noRd
downloader <- function(dir, url, type){

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = T)
  }

  file <-  file.path(dir, basename(url))

  if (!file.exists(file)) {

    message("Downloading ", basename(url))

    resp <-  httr::GET(url,
                     httr::write_disk(file, overwrite = TRUE),
                     httr::progress())

    if (resp$status_code != 200) {
      stop("Download unsuccessfull :(")
    }

  } else {
    message("Compressed ", toupper(type), " file already exists ...")
  }

  return(file)

}

#' @title Utility to see in 7z is local
#' @description Checks if 7z is on system. If not, provides an informative error
#' @noRd
check7z <- function() {

  tryCatch({
    system("7z", intern = TRUE)
  }, error = function(e) {
    stop( simpleError(

        "Please Install 7zip (Windows) or p7zip (MacOS/Unix). Choose accordingly:
        Windows: https://www.7-zip.org/download.html
        Mac: 'brew install p7zip' or 'sudo port install p7zip'
        Linux: https://sourceforge.net/projects/p7zip/"

      )
      )
  })

}

