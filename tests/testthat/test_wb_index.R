test_that("index to waterbodies", {
sample_data <- system.file("extdata/sample_natseamless.gpkg", package = "nhdplusTools")

wb <- sf::read_sf(sample_data, "NHDWaterbody")
gage <- sf::read_sf(sample_data, "Gage")

gage_l <- sf::st_drop_geometry(gage) %>%
  dplyr::filter(!is.na(LonSite)) %>%
  sf::st_as_sf(coords = c("LonSite", "LatSite"), crs = 4326)

gage_l <- dplyr::select(gage_l, hy_locid = SOURCE_FEA)

expect_warning(match <- get_waterbody_index(wb, gage_l), "st_transform points to match waterbodies")

expect_equal(match[18,]$near_wb_COMID, match[18,]$in_wb_COMID)

expect_equal(match[13,]$near_wb_COMID, 13293262)

gage_l <- sf::st_transform(gage_l, 5070)
wb_l <- sf::st_transform(dplyr::select(wb, COMID), 5070)

match <- get_waterbody_index(wb_l, gage_l, search_radius = 50)

expect_true(is.na(match[13,]$near_wb_COMID))

expect_true(is.na(match[13,]$near_wb_dist))

match <- get_waterbody_index(wb_l, gage_l, search_radius = 200)

expect_equal(match[13,]$near_wb_dist, 164, tolerance = 1)

expect_is(match, "sf")

# mapview::mapview(list(wb_l, match))
})
