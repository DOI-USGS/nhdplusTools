#' Download NHDPlus HiRes
#' @param nhd_dir character directory to save output into
#' @param hu_list character vector of hydrologic region(s) to download.
#' Use \link{get_huc} to find HU codes of interest. Accepts two digit
#' and four digit codes.
#' @param download_files boolean if FALSE, only URLs to files will be returned
#' can be hu02s and/or hu04s
#' @param archive pull data from the "archive" folder rather than "current".
#' The archive contains the original releases of NHDPlusHR data that were updated
#' in subsequent processing. Not all subsets of NHDPlusHR were updated. See:
#' https://www.usgs.gov/national-hydrography/access-national-hydrography-products
#' for more details.
#'
#' @return character Paths to geodatabases created.
#' @export
#' @examples
#' \donttest{
#' hu <- get_huc(sf::st_sfc(sf::st_point(c(-73, 42)), crs = 4326),
#'                             type = "huc08")
#' if(inherits(hu, "sf")) {
#' (hu <- substr(hu$huc8, 1, 2))
#'
#' download_nhdplushr(tempdir(), c(hu, "0203"), download_files = FALSE)
#'
#' download_nhdplushr(tempdir(), c(hu, "0203"), download_files = FALSE, archive = TRUE)
#' }
#' }
download_nhdplushr <- function(nhd_dir, hu_list, download_files = TRUE, archive = FALSE) {

  list_source <- get("nhdhr_file_list", envir = nhdplusTools_env)

  if(archive) list_source <- get("archive_nhdhr_file_list", envir = nhdplusTools_env)

  download_nhd_internal(get("nhd_bucket", envir = nhdplusTools_env),
               list_source,
               "NHDPLUS_H_", nhd_dir, hu_list, download_files)
}

#' Download NHD
#' @inheritParams download_nhdplushr
#'
#' @return character Paths to geodatabases created.
#' @export
#' @examples
#' \donttest{
#' hu <- get_huc(sf::st_sfc(sf::st_point(c(-73, 42)), crs = 4326),
#'                             type = "huc08")
#'
#' (hu <- substr(hu$huc8, 1, 2))
#'
#' download_nhd(tempdir(), c(hu, "0203"), download_files = FALSE)
#' }
download_nhd <- function(nhd_dir, hu_list, download_files = TRUE) {

  download_nhd_internal(get("nhd_bucket", envir = nhdplusTools_env),
               get("nhd_file_list", envir = nhdplusTools_env),
               "NHD_H_",
               nhd_dir, hu_list, download_files)
}

#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_text
#' @importFrom utils download.file
#' @importFrom zip unzip
download_nhd_internal <- function(bucket, file_list_snip, prefix, nhd_dir, hu_list, download_files = TRUE) {
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

    file_list <- tryCatch({
      read_xml(paste0(bucket, file_list_snip,
                      prefix, hu02)) %>%
        xml_ns_strip() %>%
        xml_find_all(xpath = "//Key") %>%
        xml_text()
    }, error= function(e) {
      NULL
    })

    if(is.null(file_list)) {
      warning("Something went wrong retrieving the nhdhr file list.")
      return(NULL)
    }

    file_list <- file_list[grepl("_GDB.zip", file_list)]

    if(subset_hu02[h]) {
      file_list <- file_list[sapply(file_list, function(f)
        any(sapply(hu04_list, grepl, x = f)))]
    }

    for(key in file_list) {
      dir_out <- ifelse(is.null(out[length(out)]), "", out[length(out)])
      out_file <- file.path(dir_out, basename(key))
      url <- paste0(bucket, key)

      hu04 <- regexec("[0-9][0-9][0-9][0-9]", out_file)[[1]]
      hu04 <- substr(out_file, hu04, hu04 + 3)

      gdb_in_dir <- list.files(dirname(out_file), full.names = TRUE)
      gdb_in_dir <- gdb_in_dir[grepl(paste0(".*", hu04, ".*\\.gdb"), gdb_in_dir, ignore.case = TRUE)]

      if(download_files & !dir.exists(gsub(".zip", ".gdb", out_file)) &
         !(length(gdb_in_dir) > 0 && !dir.exists(gdb_in_dir))) {

        if(file.exists(out_file)) {
          unlink(out_file)
        }

        httr::RETRY("GET", url, httr::write_disk(out_file), httr::progress())

        tryCatch({zip::unzip(out_file, exdir = out[length(out)])},
                 error = function(e) {
                   warning("error unzipping with zip::unzip \n",
                           out_file, "\n", e, "\ntrying a different way", immediate. = TRUE)
                   files <- try(utils::unzip(out_file, exdir = out[length(out)]))
                   if(!inherits(files, "try-error")) {
                     warning("Success with utils::unzip", immediate. = TRUE)
                   } else {
                     warning("unzip of\n", out_file,
                             "\nfailed with utils and zip packages.\n",
                             "Try manually unzipping?", immediate. = TRUE)}
                 })

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
#'       url = paste0("https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/",
#'                    "Data/NationalData/NHDPlusV21_NationalData_Seamless",
#'                    "_Geodatabase_HI_PR_VI_PI_03.7z"))
#' }

download_nhdplusv2 <- function(outdir,
                               url = paste0("https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/",
                                            "Data/NationalData/NHDPlusV21_NationalData_Seamless",
                                            "_Geodatabase_Lower48_07.7z"),
                               progress = TRUE) {

  tryCatch({
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
  }, error = function(e) {
    warning("Something went wrong downloading nhd data.")
    return(NULL)
  })
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

  tryCatch({
  file <- downloader(outdir, url, "WBD", progress)

  message("Extracting data ...")

  try(suppressWarnings(zip::unzip(file, exdir = outdir, overwrite = FALSE)))

  path <- list.dirs(outdir)[grepl("gdb", list.dirs(outdir))]
  path <- path[grepl("WBD", path)]

  message(paste("WBD data extracted to:", path))

  return(invisible(path))
  }, error = function(e) {
    warning("Something went wrong trying to download WBD data.")
    return(NULL)
  })
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
  tryCatch({
  file <- downloader(outdir, url, "RF1", progress)

  message("Extracting data ...")

  R.utils::gunzip(file, remove = FALSE, skip = T)

  path     <- list.files(outdir, full.names = T)[!grepl("gz", list.files(outdir))]
  path     <- path[grepl("rf1", path)]

  message(paste("RF1 data extracted to:", path))

  return(invisible(path))
  }, error = function(e) {
    warning("Something went wrong trying to download RF1 data.")
    return(NULL)
  })

}

#' @title Function to download data from URL to out directory using `httr`.
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

  if(grepl("name=", basename(url))) {
    file <- file.path(dir, tail(strsplit(basename(url), "=")[[1]], 1))
  }

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

