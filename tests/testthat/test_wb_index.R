test_that("index to waterbodies", {
  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

wb <- sf::read_sf(sample_data, "NHDWaterbody")
gage <- sf::read_sf(sample_data, "Gage")
fline <- sf::read_sf(sample_data, "NHDFlowline_Network")

gage_l <- sf::st_drop_geometry(gage) %>%
  dplyr::filter(!is.na(LonSite)) %>%
  sf::st_as_sf(coords = c("LonSite", "LatSite"), crs = 4326)

gage_l <- dplyr::select(gage_l, hy_locid = SOURCE_FEA)

expect_warning(match <- get_waterbody_index(wb, gage_l), "st_transform points to match waterbodies")

expect_equal(match[18,]$near_wb_COMID, match[18,]$in_wb_COMID)

expect_equal(match[13,]$near_wb_COMID, 13293262)

gage_l <- sf::st_transform(gage_l, 5070)
wb_l <- sf::st_transform(dplyr::select(wb, COMID), 5070)

match <- get_waterbody_index(wb_l, gage_l, search_radius = units::set_units(50, "m"))

expect_true(is.na(match[13,]$near_wb_COMID))

expect_true(is.na(match[13,]$near_wb_dist))

match <- get_waterbody_index(wb_l, gage_l, search_radius = units::set_units(200, "m"))

expect_equal(match[13,]$near_wb_dist, 164, tolerance = 1)

match <- get_waterbody_index(wb_l, gage_l, flines = fline, search_radius = units::set_units(200, "m"))

gage_l <- cbind(gage_l, match)

# waterbody without flowline
expect_true(is.na(match[18,]$outlet_fline_COMID))

# point near waterbody
expect_equal(match[7,]$outlet_fline_COMID, 13294312)

})
