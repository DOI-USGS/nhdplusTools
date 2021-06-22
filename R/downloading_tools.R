#' Download NHDPlus HiRes
#' @param nhd_dir character directory to save output into
#' @param hu_list character vector of hydrologic region(s) to download.
#' Use \link{get_huc8} to find HU codes of interest. Accepts two digit
#' and four digit codes.
#' @param download_files boolean if FALSE, only URLs to files will be returned
#' can be hu02s and/or hu04s
#'
#' @return character Paths to geodatabases created.
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_text
#' @importFrom utils download.file
#' @importFrom zip unzip
#' @export
#' @examples
#' \donttest{
#' hu <- nhdplusTools::get_huc8(sf::st_sfc(sf::st_point(c(-73, 42)), crs = 4326))
#'
#' (hu <- substr(hu$huc8, 1, 2))
#'
#' download_nhdplushr(tempdir(), c(hu, "0203"), download_files = FALSE)
#' }
download_nhdplushr <- function(nhd_dir, hu_list, download_files = TRUE) {

  nhdhr_bucket <- get("nhdhr_bucket", envir = nhdplusTools_env)
  nhdhr_file_list <- get("nhdhr_file_list", envir = nhdplusTools_env)

  hu02_list <- unique(substr(hu_list, 1, 2))
  hu04_list <- hu_list[which(nchar(hu_list) == 4)]
  subset_hu02 <- sapply(hu02_list, function(x)
    sapply(x, function(y) any(grepl(y, hu04_list))))

  out <- c()

  for(h in 1:length(hu02_list)) {
    hu02 <- hu02_list[h]

    if(download_files) {
      out <- c(out, file.path(nhd_dir, hu02))
    }

    if(download_files) {
      dir.create(out[length(out)], recursive = TRUE, showWarnings = FALSE)
    }

    file_list <- read_xml(paste0(nhdhr_bucket, nhdhr_file_list,
                                 "NHDPLUS_H_", hu02)) %>%
      xml_ns_strip() %>%
      xml_find_all(xpath = "//Key") %>%
      xml_text()

    file_list <- file_list[grepl("_GDB.zip", file_list)]

    if(subset_hu02[h]) {
      file_list <- file_list[sapply(file_list, function(f)
        any(sapply(hu04_list, grepl, x = f)))]
    }

    for(key in file_list) {
      dir_out <- ifelse(is.null(out[length(out)]), "", out[length(out)])
      out_file <- file.path(dir_out, basename(key))
      url <- paste0(nhdhr_bucket, key)

      hu04 <- regexec("[0-9][0-9][0-9][0-9]", out_file)[[1]]
      hu04 <- substr(out_file, hu04, hu04 + 3)

      if(download_files & !dir.exists(gsub(".zip", ".gdb", out_file)) &
         !dir.exists(file.path(dirname(out_file), paste0(hu04, ".gdb")))) {
        download.file(url, out_file)
        zip::unzip(out_file, exdir = out[length(out)])
        unlink(out_file)
      } else if(!download_files) {
        out <- c(out, url)
      }
    }
  }
  return(out)
}

#' @title Download seamless National Hydrography Dataset Version 2 (NHDPlusV2)
#' @description This function downloads and decompresses staged seamless NHDPlusV2 data.
#' The following requirements are needed: p7zip (MacOS), 7zip (windows) Please see:
#' https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data
#' for more information and metadata about this data.
#'
#' Default downloads lower-48 only. See examples for islands. No Alaska data
#' are available.
#'
#' @param outdir The folder path where data should be downloaded and extracted
#' @param url the location of the online resource
#' @param progress boolean display download progress?
#' @return character path to the local geodatabase
#' @export
#' @examples
#' \dontrun{
#'   download_nhdplusV2("./data/nhd/")
#'
#'   download_nhdplusv2(outdir = "./inst/",
#'       url = paste0("https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/",
#'                    "Data/NationalData/NHDPlusV21_NationalData_Seamless",
#'                    "_Geodatabase_HI_PR_VI_PI_03.7z"))
#' }

download_nhdplusv2 <- function(outdir,
                               url = paste0("https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/",
                                            "Data/NationalData/NHDPlusV21_NationalData_Seamless",
                                            "_Geodatabase_Lower48_07.7z"),
                               progress = TRUE) {

  file <- downloader(outdir, url, "nhdplusV2", progress)

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
#' @inheritParams download_nhdplusv2
#' @return character path to the local geodatabase
#' @export
#' @importFrom zip unzip
#' @examples
#' \dontrun{
#'   download_wbd("./data/wbd/")
#' }

download_wbd <- function(outdir,
                         url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/",
                                      "Hydrography/WBD/National/GDB/WBD_National_GDB.zip"),
                         progress = TRUE) {

  file <- downloader(outdir, url, "WBD", progress)

  message("Extracting data ...")

  try(suppressWarnings(zip::unzip(file, exdir = outdir, overwrite = FALSE)))

  path <- list.dirs(outdir)[grepl("gdb", list.dirs(outdir))]
  path <- path[grepl("WBD", path)]

  message(paste("WBD data extracted to:", path))

  return(invisible(path))
}

#' @title Download the seamless Reach File (RF1) Database
#' @description This function downloads and decompresses staged RF1 data.
#' See: https://water.usgs.gov/GIS/metadata/usgswrd/XML/erf1_2.xml for metadata.
#' @inheritParams download_nhdplusv2
#' @return character path to the local e00 file
#' @export
#' @examples
#' \dontrun{
#'   download_wbd("./data/rf1/")
#' }

download_rf1 <- function(outdir,
                         url = "https://water.usgs.gov/GIS/dsdl/erf1_2.e00.gz",
                         progress = TRUE){

  file <- downloader(outdir, url, "RF1", progress)

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
downloader <- function(dir, url, type, progress = TRUE){

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = T)
  }

  file <-  file.path(dir, basename(url))

  if (!file.exists(file)) {

    message("Downloading ", basename(url))

    if(progress) {
      resp <-  httr::GET(url,
                         httr::write_disk(file, overwrite = TRUE),
                         httr::progress())
    } else {
      resp <-  httr::GET(url,
                         httr::write_disk(file, overwrite = TRUE))
    }

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

