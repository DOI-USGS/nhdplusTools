
test_that("rescale", {
  skip_on_cran()

  vars <- data.frame(characteristic_id = c("CAT_EWT", "CAT_EWT", "CAT_EWT"),
                     summary_statistic = c("area_weighted_mean", "min","sum"))

  # file_name <- "refactor_02.gpkg"
  # file_path <- file.path(tempdir(), file_name)
  # gpkg_file <- sbtools::item_file_download("61fbfdced34e622189cb1b0a",
  #                                          destinations = file_path,
  #                                          names = file_name,
  #                                          overwrite_file = TRUE)
  #
  # lookup_table <- sf::st_read(gpkg_file, layer= "lookup_table") |>
  #   dplyr::rename(comid = NHDPlusV2_COMID, member_comid = member_COMID, id = reconciled_ID) |>
  #   # subset lookup table for this example
  #   dplyr::filter(id %in% c(10012268, 10012979, 10024047, 10024048, 10024049, 10024050))
  # lookup_table
  #
  # split_divides <- sf::st_read(gpkg_file, layer = "split_divides") |>
  #   dplyr::rename("featureid" = "FEATUREID") |>
  #   dplyr::filter(as.integer(featureid) %in% lookup_table$comid)
  #
  #
  # catchment_characteristic <- get_catchment_characteristics(varname = vars$characteristic_id,
  #                                                           ids = unique(lookup_table$comid))
  #
  # catchment_areas <- nhdplusTools:::get_catchment_areas(unique(lookup_table$member_comid), split_divides)
  #
  # saveRDS(list(lookup_table = lookup_table,
  #              split_divides = split_divides,
  #              catchment_characteristic = catchment_characteristic,
  #              catchment_areas = catchment_areas),
  #         "tests/testthat/data/rescale_data.rds")

  d <- readRDS(list.files(pattern = "rescale_data.rds", recursive = TRUE, full.names = TRUE))

  rescale <- rescale_catchment_characteristics(vars, d$lookup_table, d$split_divides)

  rescale_2 <- rescale_catchment_characteristics(vars, d$lookup_table, d$split_divides,
                                                 d$catchment_characteristic, d$catchment_areas)
  expect_true(is.data.frame(rescale))

  expect_equal(length(unique(d$lookup_table$id)), nrow(rescale))

  expect_equal(rescale, rescale_2)

  borked <- dplyr::rename(d$lookup_table, borked = "member_comid")

  expect_error(rescale_catchment_characteristics(vars, borked, d$split_divides),
               "Check that lookup_table")

  borked <- dplyr::rename(vars, borked = "characteristic_id")

  expect_error(rescale_catchment_characteristics(borked, d$lookup_table, d$split_divides),
               "Check that vars contains")

  borked <- vars
  borked$summary_statistic[1] <- "borked"

  expect_error(rescale_catchment_characteristics(borked, d$lookup_table, d$split_divides),
               "Check that all entries in")

  expect_error(rescale_catchment_characteristics(vars, d$lookup_table, NULL,
                                                     d$catchment_characteristic),
               "refactored_areas required when")

  mini <- d$lookup_table[1:2,]

  rescale <- rescale_catchment_characteristics(vars, mini, d$split_divides,
                                               d$catchment_characteristic)

  expect_equal(nrow(rescale), 1)

})
