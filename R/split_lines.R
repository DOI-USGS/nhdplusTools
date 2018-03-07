#' @title split lines
#' @description Splits lines longer than a given threshold into the minimum number of pieces to all be under the given threshold.
#' @param lines data.frame of class sf with LINESTRING sfc column.
#' @param max_length maximum segment length to return
#' @param id name of ID column in data.frame
#' @return only the split lines.
#' @importFrom dplyr group_by ungroup filter left_join select rename mutate
#' @export
#'
split_lines <- function(input_lines, max_length, id = "ID") {
  geom_column <- attr(input_lines, "sf_column")

  input_crs <- sf::st_crs(input_lines)

  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- filter(select(input_lines, id, geom_column, geom_len), geom_len >= max_length)

  rm(input_lines) # just to control memory usage in case this is big.

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_len / max_length),
                     piece_len = (geom_len / pieces),
                     fID = 1:nrow(too_long))

  split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),]

  split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
    select(-geom_len, -pieces) %>%
    group_by(fID) %>%
    mutate(ideal_len = cumsum(piece_len)) %>%
    ungroup()

  coords <- data.frame(sf::st_coordinates(too_long[[geom_column]]))
  rm(too_long)

  coords <- rename(coords, fID = L1) %>% mutate(nID = 1:nrow(coords))

  split_nodes <- group_by(coords, fID) %>%
    # First calculate cumulative length by feature.
    mutate(len  = sqrt(((X - (lag(X)))^2) + (((Y - (lag(Y)))^2)))) %>%
    mutate(len = ifelse(is.na(len), 0, len)) %>%
    mutate(len = cumsum(len)) %>%
    # Now join nodes to split points -- this generates all combinations.
    left_join(select(split_points, fID, ideal_len, split_fID), by = "fID") %>%
    # Calculate the difference between node-wise distance and split-point distance.
    mutate(diff_len = abs(len - ideal_len)) %>%
    # regroup by the new split features.
    group_by(split_fID) %>%
    # filter out na then grab the min distance
    filter(!is.na(diff_len) & diff_len == min(diff_len)) %>%
    ungroup() %>%
    # Grab the start node for each geometry -- the end node of the geometry before it.
    mutate(start_nID = lag(nID),
           # need to move the start node one for new features.
           new_feature = fID - lag(fID, default = -1),
           start_nID = ifelse(new_feature == 1, start_nID + 1, start_nID)) %>%
    # Clean up the mess
    select(fID, split_fID, start_nID, stop_nID = nID, -diff_len, -ideal_len, -len, -X, -Y)

  split_nodes$start_nID[1] <- 1

  split_points <- left_join(split_points, select(split_nodes, split_fID, start_nID, stop_nID), by = "split_fID")

  new_line <- function(start_stop, coords) {
    sf::st_linestring(as.matrix(coords[start_stop[1]:start_stop[2], c("X", "Y")]))
  }

  split_lines <- apply(as.matrix(split_points[c("start_nID", "stop_nID")]),
                        MARGIN = 1, FUN = new_line, coords = coords)

  split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))

  return(split_lines)
}
