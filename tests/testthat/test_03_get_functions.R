# TESTING UNITS
# ==============================================================================
# area = AOI::aoi_get("Eureka, Montana")
# st_as_text(area$geometry)
area = data.frame(loc = "eurika", geometry =
        "POLYGON ((-115.0631 48.86286, -115.0345 48.86286, -115.0345 48.88706, -115.0631 48.88706, -115.0631 48.86286))") %>%
sf::st_as_sf(wkt = "geometry", crs = 4326)

# pt = AOI::geocode("Eureka, Montana", pt = TRUE)
# st_as_text(pt$geometry)
pt = data.frame(loc = "eurika", geometry = "POINT (-115.0535 48.87996)") %>%
  sf::st_as_sf(wkt = "geometry", crs = 4326)

pt2 = data.frame(loc = "ucsb", geometry = "POINT (-119.8458 34.4146)") %>%
  sf::st_as_sf(wkt = "geometry", crs = 4326)

# ==============================================================================


test_that("query water geoserver...",{
  testthat::skip_on_cran()
  #available?
  df = nhdplusTools:::query_usgs_geoserver()
  expect_equal(ncol(df), 6)

  # errors
  # Bad type request
  expect_error(query_usgs_geoserver(AOI = pt, type = 'wrong'))
  # Missing AOI and ID(s)
  expect_error(query_usgs_geoserver(AOI = NULL, id = NULL,  type = 'huc8_legacy'))
  # Providing both an AOI and ID(s)
  expect_error(query_usgs_geoserver(AOI = pt, id = 17010101,  type = 'huc8_legacy'))
})

# Walk our way through the 7 different offerings...
#   server   user_call           geoserver       ids
# 1 wmadata       huc08               huc08      huc8
# 2 wmadata       huc12               huc12     huc12
# 3 wmadata         nhd nhdflowline_network     comid
# 4 wmadata   catchment         catchmentsp featureid
# 5 wmadata     nhdarea             nhdarea     comid
# 6 wmadata waterbodies        nhdwaterbody     comid
# 7 wmadata     gagesII             gagesii     staid

# ==============================================================================

test_that("huc8", {
  testthat::skip_on_cran()
  #Point
  ptHUC8 = get_huc(AOI = pt, type = "huc08")
  expect_equal(nrow(ptHUC8), 1)
  expect_equal(ptHUC8$huc8, "17010101")
  expect_equal(sf::st_crs(ptHUC8)$epsg, 4326)

  expect_error(get_huc(AOI = rbind(pt, pt2), type = "huc08"),
               "AOI must be one an only one feature.")

  #Area
  areaHUC8 = get_huc(AOI = area, t_srs = 5070, type = "huc08")
  expect_equal(sf::st_crs(areaHUC8)$epsg, 5070)
  expect_equal(nrow(areaHUC8), 1)
  #ID
  ptHUC8id = get_huc(id = "17010101", type = "huc08")
  expect_identical(ptHUC8$huc8, ptHUC8id$huc8)
  expect_true(sf::st_crs(ptHUC8) == sf::st_crs(ptHUC8id) )
})

# ==============================================================================

test_that("huc", {
  testthat::skip_on_cran()

  expect_error(get_huc(AOI = pt, type = "borked"),
               "type must be one of huc02 huc04 huc06 huc08 huc10 huc12 huc12_nhdplusv2")
  #Point
  ptHUC12 = get_huc(AOI = pt, type = "huc12_nhdplusv2")
  expect_equal(nrow(ptHUC12), 1)
  expect_equal(ptHUC12$huc12, "170101010306")
  #Area
  areaHUC12 = get_huc(AOI = area, type = "huc12_nhdplusv2")
  expect_equal(nrow(areaHUC12), 2)
  #ID
  HUC12id = get_huc(id = "170101010306", type = "huc12_nhdplusv2")
  expect_identical(ptHUC12$huc12, HUC12id$huc12)
  # multi-id... only need to check once
  HUC12id2 = get_huc(id = areaHUC12$huc12, type = "huc12_nhdplusv2") %>%
    sf::st_transform(sf::st_crs(area))

  expect_identical(HUC12id2$geometry, areaHUC12$geometry)

  hu12 <- get_huc(AOI = pt, type = "huc12")

  expect_equal(hu12$huc12, "170101010806")

  hu10 <- get_huc(AOI = pt, type = "huc10")

  expect_equal(hu10$huc10, "1701010108")

  hu08 <- get_huc(AOI = pt, type = "huc08")

  expect_equal(hu08$huc8, "17010101")

  hu06 <- get_huc(AOI = pt, type = "huc06")

  expect_equal(hu06$huc6, "170101")

  hu04 <- get_huc(AOI = pt, type = "huc04")

  expect_equal(hu04$huc4, "1701")

  hu02 <- get_huc(AOI = pt, type = "huc02")

  expect_equal(hu02$huc2, "17")

  expect_error(get_huc(AOI = pt, type = "test"))

})

# ==============================================================================

test_that("get_nhdplus...", {
  testthat::skip_on_cran()
  #POINT, Flowlines
  fl = get_nhdplus(AOI = pt, realization = 'flowline')
  expect_equal(nrow(fl), 1)
  expect_equal(sf::st_crs(fl)$epsg, 4326)
  expect_equal(as.character(sf::st_geometry_type(fl)), 'LINESTRING')

  # test t_srs to override CRS
  fl5070 = get_nhdplus(AOI = pt, realization = 'flowline', t_srs = 5070)
  expect_equal(sf::st_crs(fl5070)$epsg, 5070)

  # POINT, Catchments
  cat = get_nhdplus(AOI = pt, realization = 'catchment')
  expect_equal(nrow(cat), 1)
  expect_equal(sf::st_crs(cat)$epsg, 4326)
  expect_true(grepl('POLYGON', as.character(sf::st_geometry_type(cat))))

  # test t_srs to override CRS
  catch5070 = get_nhdplus(AOI = pt, realization = 'catchment', t_srs = 5070)
  expect_equal(sf::st_crs(catch5070)$epsg, 5070)

  # POINT, outlet
  out = get_nhdplus(AOI = pt, realization = 'outlet')
  expect_equal(nrow(out), 1)
  expect_equal(sf::st_crs(out)$epsg, 4326)
  expect_equal(as.character(sf::st_geometry_type(out)), 'POINT')

  #POLYGON, all
  areaNHD  = get_nhdplus(AOI = area, realization = "all")
  expect_equal(length(areaNHD), 3)
  expect_equal(nrow(areaNHD$flowline), nrow(areaNHD$catchment))
  expect_equal(nrow(areaNHD$outlet), nrow(areaNHD$catchment))
  expect_equal(as.character(sf::st_geometry_type(areaNHD$outlet))[1], 'POINT')
  expect_equal(as.character(sf::st_geometry_type(areaNHD$flowline))[1], 'LINESTRING')
  expect_true(grepl('POLYGON', as.character(sf::st_geometry_type(areaNHD$catchment))[1]))

  # ID
  # forcing "no attributes found" for bad COMID
  out <- capture_warnings(get_nhdplus(comid = 1))

  expect_true("No nhd features found" %in% out)

  idCheck  = get_nhdplus(comid = 101, realization = 'all')
  expect_equal(length(idCheck), 3)
  expect_equal(nrow(idCheck$flowline), nrow(idCheck$catchment))
  expect_equal(nrow(idCheck$catchment), nrow(idCheck$outlet))
  expect_equal(as.character(sf::st_geometry_type(idCheck$outlet))[1], 'POINT')
  expect_equal(as.character(sf::st_geometry_type(idCheck$flowline))[1], 'LINESTRING')
  expect_equal(as.character(sf::st_geometry_type(idCheck$catchment))[1], 'POLYGON')

  #streamorder filter
  streamOrderSubset = get_nhdplus(AOI = area, streamorder = 3)
  expect_gt(nrow(areaNHD$flowline), nrow(streamOrderSubset))

  byGage = get_nhdplus(nwis = c('05427718'))
  expect_equal(nrow(byGage), 1)
  expect_equal(byGage$comid, 13293454)

  byGageComid = get_nhdplus(nwis = c('05427718'), comid = 101)
  expect_equal(nrow(byGageComid), 2)

  #bad realization error check
  expect_error(get_nhdplus(AOI = area, realization = "wrong"))
  expect_error(get_nhdplus(AOI = as_Spatial(area)))
  expect_error(get_nhdplus(AOI = area, comid  = 101))
  expect_error(get_nhdplus(AOI = NULL, comid = NULL))
})

# ==============================================================================

test_that("nhdarea", {
  testthat::skip_on_cran()
  # No intersecting point here...
  out <- capture_warnings(get_nhdarea(AOI  = pt))

  expect_true("No nhdarea features found" %in% out)
  # Buffer it out ...
  nhdarea = get_nhdarea(AOI  = pt, buffer = 1e4)
  expect_equal(nhdarea$ftype, "Submerged Stream")
  expect_equal(sf::st_crs(nhdarea)$epsg, 4326)
  nhdarea5070 = get_nhdarea(AOI  = pt, buffer = 1e4, t_srs = 5070)
  expect_equal(sf::st_crs(nhdarea5070)$epsg, 5070)
})

# ==============================================================================

test_that("nhdwaterbody", {
  testthat::skip_on_cran()
  wb = get_waterbodies(AOI  = pt, buffer = 2e3)
  expect_equal(nrow(wb), 3)
  expect_equal(sf::st_crs(wb)$epsg, 4326)

  wb5070 = get_waterbodies(id = 22887007, t_srs = 5070)
  expect_equal(sf::st_crs(wb5070)$epsg, 5070)
  expect_equal(wb5070$comid, 22887007)
})

# ==============================================================================

test_that("gagesii", {
  testthat::skip_on_cran()
  gages2 = get_gagesII(AOI  = pt, buffer = 3e3)
  expect_equal(nrow(gages2), 1)

  gages2 <- get_gagesII(AOI = pt, buffer = 3e3,
                        basin = TRUE)

  expect_equal(names(gages2), c("site", "basin"))

  gages2 = get_gagesII(id = "01013500", basin = TRUE)
  expect_true("Ref" %in% gages2$site$class)


})

# ==============================================================================

test_that("discover_nhdplus_id", {
  testthat::skip_on_cran()
  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
  discover_nhdplus_id(nldi_feature = nldi_nwis)
  expect_error(discover_nhdplus_id(nldi_feature = NULL, point = NULL))
})

test_that("get_nwis", {
  testthat::skip_on_cran()
  areaSearch = get_nwis(AOI = area)
  expect(nrow(areaSearch), 1)
  expect_equal(sf::st_crs(areaSearch)$epsg, 4326)

  areaSearch5070 = get_nwis(AOI = area, t_srs = 5070)
  expect(nrow(areaSearch5070), 1)
  expect_equal(sf::st_crs(areaSearch5070)$epsg, 5070)

  expect_equal(areaSearch$site_no, areaSearch5070$site_no)

  pt2BuffNorm = get_nwis(AOI = pt2)
  pt2BuffDecrease = get_nwis(AOI = pt2, buffer = 10000)

  Sys.sleep(5)
  pt2BuffIncrease = get_nwis(AOI = pt2, buffer = 40000)

  expect_true(nrow(pt2BuffIncrease) > nrow(pt2BuffNorm))
  expect_true(nrow(pt2BuffNorm) > nrow(pt2BuffDecrease))
  expect_equal(order(pt2BuffNorm$distance_m), 1:nrow(pt2BuffNorm))

  expect_error(get_nwis(AOI = pt2, buffer = 1000000))

  expect_error(get_nwis(AOI = AOI, buffer = 1))
  expect_warning(get_nwis(AOI = sf::st_buffer(sf::st_transform(pt2,5070), 1)))

})
