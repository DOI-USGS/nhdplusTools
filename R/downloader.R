#' @title Downloader: Downloading National-Scale Hydro feature archives
#' @description This function downloads and decompresses staged seamless data representing various hydro features in CONUS.
#' Available options include the seamless NHDPlusV21, the Watershed Boundary dataset and the EPA Reach File (RF). The WBD and RF can be downloaded with no system dependencies.
#' To download and extract the NHDPlus users need to meet the following requirements: p7zip (MacOS), 7zip (windows)
#' @param outdir The folder path where data should be downloaded and extracted
#' @param type The dataset to download and extract. Options include (1) nhdplus (2) wbd (3) rf1
#' @return the path to the respective geodatabase or e00 file
#' @export
#' @importFrom httr progress GET write_disk
#' @importFrom utils unzip
#' @importFrom R.utils gunzip
#' @examples
#' \dontrun{
#' outdir = "./data"
#'   downloader(outdir, "nhdplus")
#'   downloader(outdir, "wbd")
#'   downloader(outdir, "rf1")
#' }

downloader = function(outdir, type = 'rf1') {

  valid_types = c("nhdplus", "wbd", 'rf1')

  bad_type = !type %in% valid_types

  if (sum(bad_type) > 0) {
    stop(paste(
      type[bad_type],
      "not a valid data source. Try one of:",
      paste0(valid_types, collapse = ", ")
    ))
  }

  url = switch(type,
               nhdplus = "https://s3.amazonaws.com/nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z",
               wbd     = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip",
               rf1     = "https://water.usgs.gov/GIS/dsdl/erf1_2.e00.gz",
               NULL)

  subdir = switch(type,
                  nhdplus = "nhdp",
                  wbd     = "wbd",
                  rf1     = "RF1",
                  NULL)

  dir = file.path(outdir, subdir)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = T)
  }

  file = file.path(dir, basename(url))

  if (!file.exists(file)) {
    message("Downloading ", basename(url))
    resp = httr::GET(url,
                httr::write_disk(file, overwrite = TRUE),
                httr::progress())
    if (resp$status_code != 200) {
      stop("Download unsuccessfull :(")
    }
  } else {
    message("Compressed ", toupper(type), " file already exists...")
  }

  message("Extracting data ...")
  switch(
    type,

    nhdplus = ifelse(any(grepl("gdb", list.dirs(dir))), 1,
                     system(paste0("7z -o", dir, " x ", file), intern = TRUE)),

    wbd     = suppressWarnings(utils::unzip(file, exdir = dir, overwrite = F)),

    rf1     = R.utils::gunzip(file, remove = FALSE, skip = T),

    NULL
  )

  fin = switch(
    type,

    nhdplus = list.dirs(dir)[grepl("gdb", list.dirs(dir))],

    wbd     = list.dirs(dir)[grepl("gdb", list.dirs(dir))],

    rf1     = list.files(dir)[!grepl("gz", list.files(dir))],

    NULL
  )

  message(paste(toupper(type), "data extracted to:", dir))
  return(fin)

}
