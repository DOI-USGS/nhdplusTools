

source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

pt_data <- sample_flines

test_that("get_nhdplus_byid", {
  skip_on_cran()
  comid_set <- get_UT(pt_data, 11687180)

  catchmentsp <- nhdplusTools:::get_nhdplus_byid(comid_set, "catchmentsp")

  expect("sf" %in% class(catchmentsp), "expected class sf")

  expect_equal(nrow(catchmentsp), 5)

  nhdflowline_network <- nhdplusTools:::get_nhdplus_byid(comid_set, "nhdflowline_network")

  expect("sf" %in% class(nhdflowline_network), "expected class sf")

  expect_equal(nrow(nhdflowline_network), 5)

  expect_error(nhdplusTools:::get_nhdplus_byid(comid_set, "testest"),
               "Layer must be one of catchmentsp, nhdflowline_network")

})

test_that("get_nhdplus_bybox", {
  skip_on_cran()
  bbox <- pt_data %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  layers <- c("nhdarea", "nhdwaterbody")

  for (layer in layers) {
    l <- nhdplusTools:::get_nhdplus_bybox(bbox, layer)
    expect(nrow(l) > 1, "expected to get data")
    expect_true("sf" %in% class(l))
  }

  expect_error(nhdplusTools:::get_nhdplus_bybox(bbox, "borked"),
               "Layer must be one of nhdarea, nhdwaterbody, nhdflowline_network, nhdflowline_nonnetwork, catchmentsp.")

})

test_that("downloaders run", {
  skip_on_cran()
  dir <- tempdir()
  mess <- capture_messages(
    out <- download_nhdplusv2(outdir = dir,
                              url = "https://doi-usgs.github.io/nhdplusTools/data/NHDPlus_test.gdb.7z",
                              progress = FALSE))
  unlink(dir, recursive = T)
  expect_true(grepl("NHDPlus_test.gdb", out))

  dir <- tempdir()
  mess <- capture_messages(
    out <- download_rf1(outdir = dir,
                        url = "https://doi-usgs.github.io/nhdplusTools/data/rf1_test.e00.gz",
                        progress = FALSE))
  unlink(dir, recursive = T)
  expect_true(grepl("rf1_test.e00", out))

  dir <- tempdir()
  temp <- capture.output(mess <- capture_messages(out <- download_wbd(outdir = dir,
                                               url = "https://doi-usgs.github.io/nhdplusTools/data/WBD_test.gdb.zip",
                                               progress = TRUE)))
  unlink(dir, recursive = T)
  expect_true(grepl("WBD_test.gdb", out))
})


