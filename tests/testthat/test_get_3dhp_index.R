test_that("get_3dhp_index", {

  pt <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(sf::st_point(c(-91.1962, 30.4419)),
                          sf::st_point(c(-89.276, 42.988)),
                          crs = 4326))

  expect_error(get_3dhp_index(sf::st_drop_geometry(pt)),
               "must be an sf")

  expect_error(
    get_3dhp_index(sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(-91, 30))))),
    "resolvable EPSG")

  with_mock_hgf("get_3dhp_index", {
    out <- get_3dhp_index(pt)

    expect_s3_class(out, "sf")
    expect_equal(nrow(out), 2)
    expect_equal(out$source_id, c("1", "2"))

    expect_true(all(c("zsnap", "mainstemid", "m", "gnisidlabel",
                      "gnisid", "featuredate", "snapdistance", "snapdate",
                      "snapped") %in% names(out)))

    expect_false(any(c("xsnap", "ysnap", "xorig", "yorig") %in% names(out)))

    # geometry is the snapped XYZ point; Z is the snapped elevation
    co <- sf::st_coordinates(out)
    expect_true("Z" %in% colnames(co))
    expect_equal(unname(co[1, "Z"]), out$zsnap[1])

    # first point snaps to a mainstem, second is off-network
    expect_true(out$snapped[1])
    expect_false(out$snapped[2])

    expect_true(grepl("^https://geoconnex.us/ref/mainstems/",
                      out$mainstemid[1]))

    # sentinels converted to NA on the unsnapped point
    expect_true(is.na(out$snapdistance[2]))
    expect_true(is.na(out$m[2]))
  })

  with_mock_hgf("get_3dhp_index", {
    raw <- get_3dhp_index(pt, convert_missing = FALSE)

    expect_false("snapped" %in% names(raw))
    expect_equal(raw$snapdistance[2], -1)
    expect_equal(raw$m[2], -1)
  })
})
