#' @title split flowlines
#' @description Splits flowlines longer than a given threshold into the minimum number of pieces to all be under the given threshold.
#' @return all the flowlines with long ones replaced with split ones.
#' @importFrom dplyr group_by ungroup filter left_join select rename mutate
#' @export
#'
split_flowlines <- function(flines, max_length) {
  flines <- sf::st_transform(flines, 5070)

  # Need to get the geometry name from attributes.
  flines$geom_LENGTHKM <- st_length(flines$geometry) / 1000

  attr(flines$geom_LENGTHKM, "units") <- NULL
  flines$geom_LENGTHKM <- as.numeric(flines$geom_LENGTHKM)

  too_long <- filter(flines, LENGTHKM >= max_length)

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_LENGTHKM / max_length),
                     piece_length = (geom_LENGTHKM / pieces) * 1000,
                     fID = 1:nrow(too_long))

  split_flines <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),]
  split_flines$new_fID <- row.names(split_flines)

  split_flines <- select(split_flines, -LENGTHKM, -geom_LENGTHKM, -pieces, -TotDASqKM)

  split_points <- group_by(split_flines, fID) %>%
    mutate(ideal_dist = cumsum(piece_length)) %>%
    ungroup()

  coords <- data.frame(st_coordinates(too_long$geometry))
  coords <- rename(coords, fID = L1) %>% mutate(nID = 1:nrow(coords))

  split_nodes <- group_by(coords, fID) %>%
    mutate(inc_dist  = sqrt(((X - (lag(X)))^2) + (((Y - (lag(Y)))^2)))) %>%
    mutate(inc_dist = ifelse(is.na(inc_dist), 0, inc_dist)) %>%
    mutate(cum_dist = cumsum(inc_dist)) %>%
    left_join(select(split_points, fID, ideal_dist, new_fID), by = "fID") %>%
    mutate(diff_dist = abs(cum_dist - ideal_dist)) %>%
    ungroup() %>% group_by(new_fID) %>%
    filter(!is.na(diff_dist)) %>%
    filter(diff_dist == min(diff_dist)) %>%
    ungroup() %>%
    mutate(start_nID = lag(nID)) %>%
    select(fID, new_fID, start_nID, stop_nID = nID, -diff_dist, -ideal_dist, -cum_dist, -inc_dist, -X, -Y) %>%
    mutate(new_feature = fID - lag(fID, default = -1)) %>%
    mutate(start_nID = ifelse(new_feature == 1, start_nID + 1, start_nID))

  split_nodes$start_nID[1] <- 1

  split_points <- left_join(split_points, select(split_nodes, new_fID, start_nID, stop_nID), by = "new_fID")

  new_line <- function(start_stop, coords) {
    sf::st_linestring(as.matrix(coords[start_stop[1]:start_stop[2], c("X", "Y")]))
  }

  split_flines <- apply(as.matrix(split_points[c("start_nID", "stop_nID")]),
                        MARGIN = 1, FUN = new_line, coords = coords)

  split_flines <- st_sf(split_points[c("ID", "toID", "new_fID")], geometry = st_sfc(split_flines, crs = 5070)) %>%
    rename(orig_ID = ID, ID = new_fID, orig_toID = toID) %>%
    mutate(toID = NA)

  orig_flines <- select(filter(flines, LENGTHKM < max_length), ID, toID) %>%
    mutate(orig_ID = ID, orig_toID = toID)

  split_flines <- rbind(split_flines, orig_flines) %>%
    select(ID, toID, orig_ID, orig_toID)

  return(split_flines)
}
