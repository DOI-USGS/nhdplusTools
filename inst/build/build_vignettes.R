if(!dir.exists("inst/doc")) dir.create("inst/doc", recursive = TRUE)

build_one <- function(name) {
  src <- file.path("vignettes", paste0(name, ".Rmd"))
  html <- file.path("inst/doc", paste0(name, ".html"))
  if(file.exists(html) && file.mtime(html) >= file.mtime(src)) {
    message("Skipping ", name, " (up to date)")
    return(invisible())
  }
  message("Building ", name, "...")
  tryCatch({
    suppressWarnings(rmarkdown::render(src, output_dir = "inst/doc", quiet = TRUE))
    knitr::purl(src, output = file.path("inst/doc", paste0(name, ".R")))
    file.copy(src, "inst/doc/", overwrite = TRUE)
    message("  done.")
  }, error = function(e) {
    message("  FAILED: ", conditionMessage(e))
  })
}

build_one("drainage_area_estimation")
build_one("get_3dhp_data")
build_one("get_data_overview")
build_one("indexing")
build_one("nhdplusTools")
build_one("nhdplushr")
build_one("plot_nhdplus")

message("Run devtools::check(build_args = '--no-build-vignettes')")
