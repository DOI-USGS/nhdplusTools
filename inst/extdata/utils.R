download_pkg_data <- function(f, u, work_dir) {
  dir.create(work_dir, showWarnings = FALSE)

  d <- file.path(work_dir, f)

  project_file <- c(paste0("../../docs/data/", f),
                    paste0("docs/data/", f))

  project_file <- project_file[file.exists(project_file)]

  if(length(project_file) > 0 &&
     file.exists(project_file[1])) {
    file.copy(project_file, d, overwrite = TRUE)
  } else {
    url <- u
    invisible(httr::RETRY("GET", url,
                          httr::write_disk(d, overwrite=TRUE),
                          times = 3, pause_cap = 20))
  }

  if(grepl(".zip$", d)) {
    zip::unzip(d, exdir = work_dir)
    unlink(d)
  }

  return(invisible(work_dir))

}
