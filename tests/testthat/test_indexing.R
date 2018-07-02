context("point indexing")

test_that("point indexing to nearest existing node works as expected", {

    flines_in <- readRDS("data/petapsco_network.rds")

    point <- sf::st_sfc(sf::st_point(c(-76.86934, 39.49328)), crs = 4269)

    expect_equal(get_flowline_index(flines_in, point, search_radius = 0.1),
           data.frame(COMID = 11688298, REACHCODE = "02060003000579",
                      REACH_meas = 100, stringsAsFactors = F))

})
