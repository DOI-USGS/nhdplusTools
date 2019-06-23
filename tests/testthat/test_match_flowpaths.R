context("match levelpaths")

test_that("match flowpaths runs", {
  source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))

  source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))

  suppressWarnings(lp_df_df <- match_flowpath(flowline = new_hope_flowline,
                                              catchment = hr_catchment))

  hr_flowline <- left_join(select(hr_flowline, NHDPlusID),
                           select(lp_df_df, NHDPlusID = members,
                                  MR_LevelPathI = LevelPathI))

  expect_equal(sum(!is.na(hr_flowline$MR_LevelPathI)), 1021)

})
