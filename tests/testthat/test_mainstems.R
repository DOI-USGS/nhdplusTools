test_that("mainstem_id_from_uri parses geoconnex mainstem uris", {
  expect_equal(hydrogeofetch:::mainstem_id_from_uri(
    "https://geoconnex.us/ref/mainstems/47068"), 47068L)

  expect_equal(hydrogeofetch:::mainstem_id_from_uri(
    c("https://geoconnex.us/ref/mainstems/1",
      "https://geoconnex.us/ref/mainstems/2086165")),
    c(1L, 2086165L))
})

test_that("parse_replacement_uris handles empty, single, and multiple replacements", {
  out <- hydrogeofetch:::parse_replacement_uris(c(
    "",
    "['https://geoconnex.us/ref/mainstems/47068']",
    "['https://geoconnex.us/ref/mainstems/2547827', 'https://geoconnex.us/ref/mainstems/2547826']"
  ))

  expect_equal(out[[1]], character(0))
  expect_equal(out[[2]], "https://geoconnex.us/ref/mainstems/47068")
  expect_equal(out[[3]], c("https://geoconnex.us/ref/mainstems/2547827",
                          "https://geoconnex.us/ref/mainstems/2547826"))
})

test_that("add_mainstems joins comids to mainstem ids using a cached lookup", {
  tmp_dir <- file.path(tempdir(), "mainstems_test_cache")
  dir.create(tmp_dir, showWarnings = FALSE)

  old_dir <- hydrogeofetch_data_dir()
  hydrogeofetch_data_dir(tmp_dir)
  on.exit(hydrogeofetch_data_dir(old_dir), add = TRUE)

  arrow::write_parquet(
    data.frame(uri = c("https://geoconnex.us/ref/mainstems/1826652",
                       "https://geoconnex.us/ref/mainstems/1826652"),
              comid = c(2804607, 2804621)),
    get_mainstem_lookup_path("nhdpv2"))

  out <- add_mainstems(data.frame(comid = c(2804607, 999999999)))

  expect_equal(out$mainstemid, c(1826652, NA_integer_))
  expect_equal(out$mainstem_uri[1],
              "https://geoconnex.us/ref/mainstems/1826652")
})

test_that("add_mainstems downloads and joins the real nhdpv2 lookup table", {
  skip_if_no_integration()

  out <- add_mainstems(data.frame(comid = c(2804607, 2804621)))

  expect_equal(out$mainstemid, c(1826652, 1826652))
})

test_that("check_mainstems flags superseded ids and leaves current ones alone", {
  with_mock_hgf("mainstems_check", {
    res <- check_mainstems(c(2086165, 2086637, 359842))

    expect_equal(res, c(TRUE, TRUE, FALSE))
  })
})

test_that("check_mainstems does not confuse a different mainstem namespace", {
  with_mock_hgf("mainstems_check", {
    # 2086165 (bare id) is a real, superseded ref/mainstems id. A uri with
    # the same trailing number minted under a different authority must not
    # be treated as that ref/mainstems id.
    res <- check_mainstems(c(2086165, "https://geoconnex.us/usgs/mainstems/2086165"))

    expect_equal(res, c(TRUE, FALSE))
  })
})

mainstem_points <- function(id) {
  feature <- hydrogeofetch:::hgf_sf(paste0(
    "https://reference.geoconnex.us/collections/mainstems/items/", id, "?f=json"))

  coords <- sf::st_coordinates(feature)

  pts <- sf::st_sfc(lapply(round(seq(1, nrow(coords), length.out = 3)), function(i) {
    sf::st_point(coords[i, 1:2])
  }), crs = sf::st_crs(feature))

  sf::st_sf(mainstemid = id, geometry = pts)
}

test_that("update_mainstems resolves a superseded mainstem with replacements", {
  with_mock_hgf("mainstems_update_wood", {
    x <- mainstem_points(2086165)

    out <- update_mainstems(x)

    expect_true(all(out$mainstem_update_status == "updated"))
    expect_true(all(out$mainstemid %in% c(2479116, 2479066)))
  })
})

test_that("update_mainstems flags an unresolvable supersession", {
  with_mock_hgf("mainstems_update_stony", {
    x <- mainstem_points(2086637)

    expect_warning(out <- update_mainstems(x), "could not be resolved")

    expect_true(all(out$mainstem_update_status == "unresolved"))
    expect_equal(out$mainstemid, rep(2086637, 3))
  })
})
