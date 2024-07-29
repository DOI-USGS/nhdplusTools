
test_that("we get urls for nhdplushr and base", {
  skip_on_cran()

  sw <- setup_workdir()

  urls <- download_nhdplushr(sw$wd, c("01", "0203"), download_files = FALSE)

  expect_equal(length(urls), 11)

  urls <- download_nhdplushr(sw$wd, c("01", "0203"), download_files = FALSE, archive = TRUE)

  expect_true(all(grepl("Archive", urls)))

  urls <- download_nhd(sw$wd, c("01", "0203"), download_files = FALSE)

  expect_equal(length(urls), 11)

  teardown_workdir(sw$wd)
})

test_that("get_nhdplushr layers and gpkg", {
  skip_on_cran()

  sw <- setup_workdir()

  get_test_file(sw$wd)

  out <- get_nhdplushr(sw$wd, out_gpkg = sw$og)

  layers <- sf::st_layers(sw$og)
  expect_equal(layers$name, c("NHDFlowline", "NHDPlusCatchment"))
  expect_equal(layers$features, c(2691, 2603))
  expect_equal(layers$features, c(nrow(out[[1]]), nrow(out[[2]])))
  expect_equal(layers$name, names(out))

  out <- get_nhdplushr(sw$wd, out_gpkg = sw$og,
                       layers = NULL, overwrite = TRUE)

  layers <- sf::st_layers(sw$og)

  expect_equal(length(layers$name), 7)
  expect_equal(layers$fields[which(layers$name == "NHDFlowline")], 57)

  out <- get_nhdplushr(sw$wd, layers = NULL)

  expect(length(names(out)), 7)

  teardown_workdir(sw$wd)
})

test_that("get_nhdplushr duplicate vpus", {
  skip_on_cran()

  sw <- setup_workdir()

  get_test_file(sw$wd)

  f <- file.path(sw$wd, "03_sub.gpkg")
  ftemp <- file.path(sw$wd, "03.gpkg")
  f1 <- file.path(sw$wd, "0303_sub.gpkg")
  f2 <- file.path(sw$wd, "0303_2sub.gpkg")

  file.copy(f, f1)
  file.copy(f, f2)

  file.rename(f, ftemp)

  expect_warning(out <- get_nhdplushr(sw$wd, out_gpkg = sw$og),
               "Found duplicate HU04s")

  file.rename(ftemp, f)
  unlink(f1)
  unlink(f2)

  teardown_workdir(sw$wd)
})

test_that("nhdplus hr waterbody", {
  skip_on_cran()

  sw <- setup_workdir()

  get_test_file(sw$wd)

  out <- get_nhdplushr(sw$wd, out_gpkg = sw$og)

  out <- get_nhdplushr(sw$wd, layers = c("NHDFlowline",
                                            "NHDWaterbody"),
                       out_gpkg = sw$og)

  wb <- out$NHDWaterbody[out$NHDWaterbody$Permanent_Identifier == 46376571,]

  expect_equal(get_wb_outlet(wb$Permanent_Identifier, out$NHDFlowline)$Permanent_Identifier,
               "46338320")

  teardown_workdir(sw$wd)
})

test_that("get_nhdplushr overwrite gpkg and pattern", {
  skip_on_cran()

  sw <- setup_workdir()

  get_test_file(sw$wd)

  out <- get_nhdplushr(sw$wd, out_gpkg = sw$og)

  layer <- c("NHDFlowline")
  out_sub <- get_nhdplushr(sw$wd, out_gpkg = sw$og,
                           layers = layer, overwrite = FALSE)

  expect_equal(names(out_sub), layer)
  layers <- sf::st_layers(sw$og)
  expect_equal(length(layers$name), 2)

  fl <- sf::read_sf(sw$og, layer)

  out_sub <- get_nhdplushr(sw$wd, out_gpkg = sw$og,
                           layers = layer, min_size_sqkm = 10,
                           overwrite = TRUE)

  layers <- sf::st_layers(sw$og)
  expect_equal(length(layers$name), 2)

  fl2 <- sf::read_sf(sw$og, layer)

  expect_true(nrow(fl2) < nrow(fl))

  devnull <- file.copy(file.path(sw$wd, "03_sub.gpkg"),
            file.path(sw$wd, "04_sub.gpkg"))

  fl <- read_sf(file.path(sw$wd, "04_sub.gpkg"), "NHDFlowline")
  fl$NHDPlusID <- fl$NHDPlusID + max(fl$NHDPlusID)
  write_sf(fl, file.path(sw$wd, "04_sub.gpkg"), "NHDFlowline")

  out_sub <- get_nhdplushr(sw$wd, pattern = ".*sub.gpkg$")

  expect_equal(nrow(out_sub$NHDFlowline), 2*nrow(fl))

  unlink(file.path(sw$wd, "04_sub.gpkg"))

  teardown_workdir(sw$wd)
})

test_that("get_nhdplushr simp and proj", {
  skip_on_cran()

  sw <- setup_workdir()

  get_test_file(sw$wd)

  out <- get_nhdplushr(sw$wd)

  out_sub <- get_nhdplushr(sw$wd, proj = 5070)

  expect_equal(st_crs(out_sub$NHDFlowline),
               st_crs(5070))

  expect_equal(st_crs(out_sub$NHDPlusCatchment),
               st_crs(5070))

  out_sub2 <- get_nhdplushr(sw$wd, proj = 5070, simp = 20)
  expect_true(length(st_geometry(out_sub$NHDFlowline)[[1]]) >
                length(st_geometry(out_sub2$NHDFlowline)[[1]]))

  expect_true(nrow(st_geometry(out_sub$NHDPlusCatchment)[[1]][[1]][[1]]) >
                nrow(st_geometry(out_sub2$NHDPlusCatchment)[[1]][[1]]))

  teardown_workdir(sw$wd)
})

test_that("get_nhdplushr rename and keep_cols", {
  skip_on_cran()

  sw <- setup_workdir()

  get_test_file(sw$wd)

  out <- get_nhdplushr(sw$wd, out_gpkg = sw$og)

  out_sub <- get_nhdplushr(sw$wd,
                           keep_cols = c("COMID", "FEATUREID", "StreamOrde", "AreaSqKM"),
                           check_terminals = FALSE)

  expect_equal(names(out_sub$NHDFlowline), c("COMID", "StreamOrde", "AreaSqKM", "geom"))
  expect_equal(names(out_sub$NHDPlusCatchment), c("FEATUREID", "AreaSqKM", "geom"))

  out_sub <- get_nhdplushr(sw$wd, rename = FALSE, check_terminals = FALSE)
  expect_true("NHDPlusID" %in% names(out_sub$NHDFlowline))

  teardown_workdir(sw$wd)
})

test_that("make_standalone", {
  skip_on_cran()

  sw <- setup_workdir()

  get_test_file(sw$wd)

  fl <- get_nhdplushr(sw$wd, check_terminals = FALSE)$NHDFlowline

  sa <- make_standalone(fl)

  sa_check <- get_nhdplushr(sw$wd, check_terminals = TRUE)$NHDFlowline

  expect_true(all(sa$LevelPathI == sa_check$LevelPathI))
  expect_true(!all(fl$LevelPathI == sa_check$LevelPathI))

  sa <- st_drop_geometry(sa)

  expect_true(!unique(fl$TerminalPa) %in% sa$TerminalPa)

  outlet <- sa$COMID[sa$Hydroseq == min(sa$Hydroseq)]

  sa_UT <- get_UT(sa, outlet)
  fl_UT <- get_UT(fl, outlet)
  expect_equal(sa_UT, fl_UT)

  expect_true(all(sa[sa$Hydroseq == min(sa$Hydroseq), ][c("DnLevel", "DnLevelPat", "DnHydroseq")] == 0))

  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  sample_flines <- get_tocomid(sample_flines)

  sample_flines[sample_flines$tocomid == 0] <- "12345"

  sample_flines <- make_standalone(sample_flines)

  expect_true(0 %in% sample_flines$toCOMID)

  teardown_workdir(sw$wd)
})
