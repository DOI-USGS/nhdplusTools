library(dplyr)

pa_data <- readRDS("padep_nhd_0204.rds")

pa_data <- sf::st_cast(pa_data, "MULTILINESTRING")

multilines <- pa_data[lengths(sf::st_geometry(pa_data)) > 1, ]
lines <- pa_data[lengths(sf::st_geometry(pa_data)) == 1, ]

lines <- lines[!sf::st_is_empty(lines),]

lines <- filter(lines, is.na(GNIS_NAME) |
                  (!is.na(GNIS_NAME) & GNIS_NAME != "Delaware River"))

mapview::mapview(lines, color = "blue") +
  mapview::mapview(multilines, color = "red")

check <- dplyr::filter(lines[1:1000, ], FLOWDIR == 1)
ends <- nhdplusTools::get_node(check)
mapview::mapview(check) + mapview::mapview(ends)

check <- dplyr::filter(lines[1:1000, ], FLOWDIR == 0)
ends <- nhdplusTools::get_node(check)
mapview::mapview(check) + mapview::mapview(ends)

sf::st_geometry(lines[lines$FLOWDIR == 0,]) <-
  sf::st_reverse(sf::st_geometry(lines[lines$FLOWDIR == 0,]))

check <- dplyr::filter(lines[1:1000, ], FLOWDIR == 0)
ends <- nhdplusTools::get_node(check)
mapview::mapview(check) + mapview::mapview(ends)

lines$FLOWDIR[lines$FLOWDIR == 0] <- rep(1, sum(lines$FLOWDIR == 0))

lines <- select(lines, COMID, GNIS_ID, GNIS_NAME, REACHCODE, FLOWDIR, FTYPE, FCODE)

# Can we figure out how to convert this data into a connected network?!?
nodes <- as.data.frame(cbind(
  sf::st_coordinates(nhdplusTools::get_node(lines, "start")),
  sf::st_coordinates(nhdplusTools::get_node(lines, "end"))))

nodes$ID <- c(lines$COMID)

names(nodes) <- c("sx", "sy", "ex", "ey", "ID")

nodes$row <- seq_len(nrow(nodes))

lines$row <- seq_len(nrow(nodes))

closest <- pbapply::pblapply(1:nrow(nodes), function(x, nodes) {
  d <- sqrt((nodes$ex[x] - nodes$sx)^2 + (nodes$ey[x] - nodes$sy)^2)

  if(min(d) > 100) {
    0
  } else {
    which(d == min(d, na.rm = TRUE))
  }

}, nodes = nodes)

nodes$closest <- closest

c_nodes <- select(nodes, row, closest) |>
  tidyr::unnest(cols = "closest") |>
  filter(row != closest) |>
  group_by(row) |>
  mutate(g_size = n()) |>
  ungroup()

lines_w_closest <- dplyr::left_join(lines, c_nodes, by = "row")

sf::write_sf(lines_w_closest, "closest_ds.gpkg", "closest_ds")

dedup <- left_join(
  distinct(select(sf::st_drop_geometry(lines_w_closest), row, closest, us = GNIS_ID)),
  distinct(select(sf::st_drop_geometry(lines), row, ds = GNIS_ID)),
  by = c("closest" = "row")) |>
  distinct() |>
  group_by(row) |>
  mutate(main = NA, group_size = n()) |>
  mutate(main = case_when(closest == 0 ~ 1,
                          n() == 1 ~ 1,
                          n() > 1 & (!is.na(us) & !is.na(ds)) &
                            us == ds ~ 2,
                          n() > 1 & (is.na(us) & !is.na(ds)) ~ 3,
                          n() > 1 &
                            (is.na(us) & is.na(ds)) ~ 4,
                          TRUE ~ 0)) |>
  ungroup()

lines_w_dedup <- dplyr::left_join(select(lines_w_closest, -g_size),
                                select(dedup, closest, main),
                                by = c("row" = "closest")) |>
  mutate(main = ifelse(is.na(main), 0, main)) |>
  group_by(row) |> mutate(group_size = n()) |> ungroup()

sf::write_sf(lines_w_dedup, "closest_ds.gpkg", "last_dups")

lines_dedup <- select(sf::st_drop_geometry(lines_w_dedup),
                      comid = COMID, REACHCODE, GNIS_ID, GNIS_NAME,
                      ID = row, toID = closest, main, group_size) |>
  distinct() |>
  group_by(ID) |>
  arrange(main) |>
  filter(row_number() == 1) |>
  ungroup() |>
  select(-main) |>
  left_join(select(lines, row), by = c("ID" = "row"))

sf::write_sf(lines_dedup, "closest_ds.gpkg", "dedup")

flines <- sf::st_sf(lines_dedup)

flines$lengthkm <- as.numeric(units::set_units(sf::st_length(flines), "km"))

flines$areasqkm <- NA

flines <- left_join(flines, select(sf::st_drop_geometry(flines),
                                   tocomid = comid, ID),
                    by = c("toID" = "ID"))

flines <- select(flines, comid, tocomid, lengthkm, areasqkm, nameID = GNIS_ID, REACHCODE, GNIS_NAME)

flines <- nhdplusTools::add_plus_network_attributes(flines, override = 5, status = TRUE)

flines <- rename(flines, GNIS_ID = nameID, arbolatesu = weight)

sf::write_sf(flines, "closest_ds.gpkg", "flines")


