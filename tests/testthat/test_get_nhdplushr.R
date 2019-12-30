context("get_nhdplushr")

work_dir <- tempdir()
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
out_gpkg <- file.path(work_dir, "temp.gpkg")

test_that("we get urls for nhdplushr", {
  skip_on_cran()
  urls <- download_nhdplushr(tempdir(), c("01", "0203"), download_files = FALSE)

  expect_equal(length(urls), 11)
})

test_that("get_nhdplushr layers and gpkg", {
  skip_on_cran()

  get_test_file(work_dir)

  out <- get_nhdplushr(work_dir, out_gpkg = out_gpkg)

  layers <- sf::st_layers(out_gpkg)
  expect_equal(layers$name, c("NHDFlowline", "NHDPlusCatchment"))
  expect_equal(layers$features, c(2691, 2603))
  expect_equal(layers$features, c(nrow(out[[1]]), nrow(out[[2]])))
  expect_equal(layers$name, names(out))

  out <- get_nhdplushr(work_dir, out_gpkg = out_gpkg,
                       layers = NULL, overwrite = TRUE)

  layers <- sf::st_layers(out_gpkg)

  expect_equal(length(layers$name), 7)
  expect_equal(layers$fields[which(layers$name == "NHDFlowline")], 57)

  out <- get_nhdplushr(work_dir, layers = NULL)

  expect(length(names(out)), 7)
})

test_that("get_nhdplushr overwrite gpkg and pattern", {
  skip_on_cran()

  layer <- c("NHDFlowline")
  out_sub <- get_nhdplushr(work_dir, out_gpkg = out_gpkg,
                           layers = layer, overwrite = FALSE)

  expect_equal(names(out_sub), layer)
  layers <- sf::st_layers(out_gpkg)
  expect_equal(length(layers$name), 7)

  fl <- sf::read_sf(out_gpkg, layer)

  out_sub <- get_nhdplushr(work_dir, out_gpkg = out_gpkg,
                           layers = layer, min_size_sqkm = 10,
                           overwrite = TRUE)

  layers <- sf::st_layers(out_gpkg)
  expect_equal(length(layers$name), 7)

  fl2 <- sf::read_sf(out_gpkg, layer)

  expect_true(nrow(fl2) < nrow(fl))

  devnull <- file.copy(file.path(work_dir, "03_sub.gpkg"),
            file.path(work_dir, "04_sub.gpkg"))

  fl <- read_sf(file.path(work_dir, "04_sub.gpkg"), "NHDFlowline")
  fl$NHDPlusID <- fl$NHDPlusID + max(fl$NHDPlusID)
  write_sf(fl, file.path(work_dir, "04_sub.gpkg"), "NHDFlowline")

  out_sub <- get_nhdplushr(work_dir, pattern = ".*sub.gpkg$")

  expect_equal(nrow(out_sub$NHDFlowline), 2*nrow(fl))

  unlink(file.path(work_dir, "04_sub.gpkg"))
})

test_that("get_nhdplushr simp and proj", {
  out <- get_nhdplushr(work_dir)

  out_sub <- get_nhdplushr(work_dir, proj = 5070)

  expect_equal(st_crs(out_sub$NHDFlowline),
               st_crs(5070))

  expect_equal(st_crs(out_sub$NHDPlusCatchment),
               st_crs(5070))

  out_sub2 <- get_nhdplushr(work_dir, proj = 5070, simp = 20)
  expect_true(length(st_geometry(out_sub$NHDFlowline)[[1]]) >
                length(st_geometry(out_sub2$NHDFlowline)[[1]]))

  expect_true(nrow(st_geometry(out_sub$NHDPlusCatchment)[[1]][[1]][[1]]) >
                nrow(st_geometry(out_sub2$NHDPlusCatchment)[[1]][[1]]))

})

test_that("get_nhdplushr rename and keep_cols", {
  out_sub <- get_nhdplushr(work_dir,
                           keep_cols = c("COMID", "FEATUREID", "StreamOrde", "AreaSqKM"),
                           check_terminals = FALSE)

  expect_equal(names(out_sub$NHDFlowline), c("COMID", "StreamOrde", "AreaSqKM", "geom"))
  expect_equal(names(out_sub$NHDPlusCatchment), c("FEATUREID", "AreaSqKM", "geom"))

  out_sub <- get_nhdplushr(work_dir, rename = FALSE, check_terminals = FALSE)
  expect_true("NHDPlusID" %in% names(out_sub$NHDFlowline))
})

test_that("make_standalone", {
  fl <- get_nhdplushr(work_dir, check_terminals = FALSE)$NHDFlowline

  sa <- make_standalone(fl)

  sa_check <- get_nhdplushr(work_dir, check_terminals = TRUE)$NHDFlowline

  expect_true(all(sa$LevelPathI == sa_check$LevelPathI))
  expect_true(!all(fl$LevelPathI == sa_check$LevelPathI))

  sa <- st_drop_geometry(sa)

  expect_true(!unique(fl$TerminalPa) %in% sa$TerminalPa)

  outlet <- sa$COMID[sa$Hydroseq == min(sa$Hydroseq)]

  sa_UT <- get_UT(sa, outlet)
  fl_UT <- get_UT(fl, outlet)
  expect_equal(sa_UT, fl_UT)

  expect_true(all(sa[sa$Hydroseq == min(sa$Hydroseq), ][c("DnLevel", "DnLevelPat", "DnHydroseq")] == 0))
})
