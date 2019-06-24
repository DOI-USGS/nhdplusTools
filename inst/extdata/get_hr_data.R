library(sf)
library(RSQLite)
library(dplyr)
get_nhdplushr <- function(hr_dir, out_gpkg, layers, ids = NULL) {

  gdb_files <- list.files(hr_dir, pattern = "GDB.gdb", full.names = TRUE)

  if(is.null(layers)) {
    layers <- st_layers(gdb_files[1])

    layers <- layers$name[!is.na(layers$geomtype) & layers$features > 0]
  }

  for(layer in layers) {
    layer_set <- lapply(gdb_files, read_sf, layer = layer)

    out <- do.call(rbind, layer_set)

    out <- st_sf(out)

    write_sf(out, layer = layer, dsn = out_gpkg)
  }

  layer_set <- lapply(gdb_files, read_sf, layer = "NHDPlusFlowlineVAA")
  out <- do.call(rbind, layer_set)
  con = dbConnect(SQLite(), dbname = out_gpkg)
  dbWriteTable(con,"NHDPlusFlowlineVAA", out)
  dbDisconnect(con)

  return(out_gpkg)
}

hr <- get_nhdplushr("03/", "0303.gpkg", c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody", "NHDArea", "NHDLine", "NHDPlusSink", "NHDPoint"))

hr_fline <- sf::read_sf(hr, "NHDFlowline")
hr_vaa <- sf::read_sf(hr, "NHDPlusFlowlineVAA")

hr_fline <- dplyr::left_join(hr_fline, select(hr_vaa, -ReachCode, -VPUID), by = "NHDPlusID")

hr_fline <- dplyr::rename(hr_fline, COMID = NHDPlusID, LENGTHKM = LengthKM, FTYPE = FType,
                           TotDASqKM = TotDASqKm, Hydroseq = HydroSeq, Pathlength = PathLength,
                          DnHydroseq = DnHydroSeq)

outlet <- 15000500005634

ut <- get_UT(hr_fline, outlet)

hr_fline <- filter(hr_fline, COMID %in% ut)
clipper <- st_bbox(hr_fline) %>%
  st_as_sfc() %>%
  st_transform(5070) %>%
  st_buffer(1000) %>%
  st_transform(st_crs(hr_fline))

out <- "03_sub.gpkg"

for(l in st_layers("0303.gpkg")$name) {
  l_data <- read_sf(hr, l)
  if(l == "NHDFlowline" | l == "NHDPlusCatchment" | l == "NHDPlusFlowlineVAA") {
    l_data <- filter(l_data, NHDPlusID %in% ut)
  } else {
    inters <- st_intersects(st_zm(l_data), clipper)
    l_data <- filter(l_data, lengths(inters) > 0)
  }
  if(l == "NHDPlusFlowlineVAA") {
    con = dbConnect(SQLite(), dbname = out)
    dbWriteTable(con, "NHDPlusFlowlineVAA", l_data)
    dbDisconnect(con)
  } else {
    if(!st_geometry_type(l_data)[1] == "POINT") {
      l_data <- st_zm(l_data) %>%
        st_transform(5070) %>%
        rmapshaper::ms_simplify(keep = 0.5) %>% #  st_simplify(dTolerance = 10)
        st_transform(st_crs(l_data))
    }
    write_sf(l_data, out, l)
  }
}

