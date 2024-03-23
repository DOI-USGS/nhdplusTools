library(sf)
library(httr)
library(dplyr)
library(archive)
library(rmapshaper)
library(nhdplusTools)

epa_path <- "https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/GlobalData/NHDPlusV21_NHDPlusGlobalData_03.7z"
zip <- file.path("data/nhdglobal.7z")

GET(epa_path, write_disk(zip))
archive_extract(zip, dir = "data")

b <- st_make_valid(st_read('data/NHDPlusGlobalData/BoundaryUnit.shp'))

vpu = b[b$UnitType == "VPU", c("DrainageID", 'UnitID')]

names(vpu) = c("DrainageID", "VPUID", "geometry")

vpu = ms_simplify(vpu, keep = .001)

######## ----
usethis::use_data(vpu_boundaries, overwrite = TRUE)
######## ----

tmp = b[b$UnitType == "RPU", c("DrainageID", 'UnitID')]

names(tmp) = c("DrainageID", "RPUID", "geometry")

rpu = get_vaa(c("rpuid", "vpuid")) |>
  select(-comid) |>
  rename(RPUID = rpuid, VPUID = vpuid) |>
  filter(!is.na(RPUID)) |>
  distinct() |>
  right_join(tmp) |>
  sf::st_as_sf() |>
  ms_simplify(keep = .001)

######## ----
usethis::use_data(rpu_boundaries, overwrite =TRUE)
######## ----


files = list.files("data", recursive = TRUE, full.names = TRUE)
bad_files = files[!grepl(".rda$", files)]

unlink('data/NHDPlusGlobalData', recursive = TRUE)
unlink(zip)
