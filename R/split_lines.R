#' @title split flowlines
#' @description A wrapper for split_lines that works on nhdplus attributes
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM and LINESTRING sf column in "meters" projection
#' @param max_length maximum segment length to return
#' @param para numeric how many threads to use in parallel computation
#' @return All the flowlines with some split apart.
#' @importFrom dplyr group_by ungroup filter select mutate
#' @export
#'
split_flowlines <- function(flines, max_length, para = 0) {
  split <- split_lines(flines, max_length, id = "COMID", para = para)

  split <- left_join(split, sf::st_set_geometry(flines, NULL), by = "COMID")

  split <- group_by(split, COMID)

  split$part <- unlist(lapply(strsplit(split$split_fID, "\\."), function(x) x[2]))
  split <- mutate(split, part = (ifelse(is.na(part), 0, as.integer(part)) + 1))

  split <- ungroup(mutate(split, toCOMID = ifelse(part == max(part), # Assume flowdir is with digitized for now -- need to check in prep code.
                                          as.character(toCOMID),
                                          paste(lead(COMID), lead(part), sep = "."))))
  split <- mutate(split, COMID = paste(COMID, part, sep = "."),
                  LENGTHKM = sf::st_length(geometry)/1000,
                  TotDASqKM = TotDASqKM) # THIS IS WRONG BUT CAN'T BE NA!!!

  split <- sf::st_as_sf(select(split, -part, -split_fID))

  attr(split$LENGTHKM, "units") <- NULL
  split$LENGTHKM <- as.numeric(split$LENGTHKM)

  remove_COMID <- unique(as.integer(split$COMID))

  not_split <- filter(flines, !(COMID %in% remove_COMID))

  flines <- rbind(not_split, split)

  # Rows with COMID like this need to be updated
  redirect_toCOMID <- flines$COMID[which(grepl("\\.1$", flines$COMID))]

  old_toCOMID <- gsub("\\.1$", "", redirect_toCOMID)

  mutate(flines,
         toCOMID = ifelse(toCOMID %in% old_toCOMID, paste0(toCOMID, ".1"), toCOMID))
}




#' @title split lines
#' @description Splits lines longer than a given threshold into the minimum number of pieces to all be under the given threshold.
#' @param input_lines data.frame of class sf with LINESTRING sfc column.
#' @param max_length maximum segment length to return
#' @param id name of ID column in data.frame
#' @param para how many cores to use
#' @return only the split lines.
#' @importFrom dplyr group_by ungroup filter select mutate
#' @export
#'
split_lines <- function(input_lines, max_length, id = "ID", para = 0) {
  if(max_length < 50) warning("short max length detected, do you have your units right?")

  geom_column <- attr(input_lines, "sf_column")

  input_crs <- sf::st_crs(input_lines)

  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- filter(select(input_lines, id, geom_column, geom_len), geom_len >= max_length)

  rm(input_lines) # just to control memory usage in case this is big.

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_len / max_length),
                     fID = 1:nrow(too_long)) %>%
    select(-geom_len)

  split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),] %>%
    select(-pieces)

  split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
    group_by(fID) %>%
    mutate(piece = 1:n()) %>%
    mutate(start = (piece - 1) / n(),
           end = piece / n()) %>%
    ungroup()

  new_line <- function(i, f, t) {
    lwgeom::st_linesubstring(x = too_long[[geom_column]][i], from = f, to = t)[[1]]
  }

  if(para > 0) {

    cl <- parallel::makeCluster(rep('localhost',2), type = "SOCK")

    split_lines <- parallel::parApply(cl, split_points[c("fID", "start", "end")], 1,
                                      function(x) new_line(i = x[["fID"]], f = x[["start"]], t = x[["end"]]))

    parallel::stopCluster(cl)
  } else {
    split_lines <- apply(split_points[c("fID", "start", "end")], 1,
                         function(x) new_line(i = x[["fID"]], f = x[["start"]], t = x[["end"]]))
  }

  rm(too_long)

  split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))

  return(split_lines)
}

# Non lwgeom method
# split_lines_2 <- function(input_lines, max_length, id = "ID") {
#   if(max_length < 50) warning("short max length detected, do you have your units right?")
#
#   geom_column <- attr(input_lines, "sf_column")
#
#   input_crs <- sf::st_crs(input_lines)
#
#   input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])
#
#   attr(input_lines[["geom_len"]], "units") <- NULL
#   input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])
#
#   too_long <- filter(select(input_lines, id, geom_column, geom_len), geom_len >= max_length)
#
#   rm(input_lines) # just to control memory usage in case this is big.
#
#   too_long <- mutate(too_long,
#                      pieces = ceiling(geom_len / max_length),
#                      piece_len = (geom_len / pieces),
#                      fID = 1:nrow(too_long))
#
#   split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),]
#
#   split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
#     select(-geom_len, -pieces) %>%
#     group_by(fID) %>%
#     mutate(ideal_len = cumsum(piece_len)) %>%
#     ungroup()
#
#   coords <- data.frame(sf::st_coordinates(too_long[[geom_column]]))
#   rm(too_long)
#
#   coords <- rename(coords, fID = L1) %>% mutate(nID = 1:nrow(coords))
#
#   split_nodes <- group_by(coords, fID) %>%
#     # First calculate cumulative length by feature.
#     mutate(len  = sqrt(((X - (lag(X)))^2) + (((Y - (lag(Y)))^2)))) %>%
#     mutate(len = ifelse(is.na(len), 0, len)) %>%
#     mutate(len = cumsum(len)) %>%
#     # Now join nodes to split points -- this generates all combinations.
#     left_join(select(split_points, fID, ideal_len, split_fID), by = "fID") %>%
#     # Calculate the difference between node-wise distance and split-point distance.
#     mutate(diff_len = abs(len - ideal_len)) %>%
#     # regroup by the new split features.
#     group_by(split_fID) %>%
#     # filter out na then grab the min distance
#     filter(!is.na(diff_len) & diff_len == min(diff_len)) %>%
#     ungroup() %>%
#     # Grab the start node for each geometry -- the end node of the geometry before it.
#     mutate(start_nID = lag(nID), ## There's an issue here where start_nID and stop_nID are equal for some flowlines
#            # need to move the start node one for new features.
#            new_feature = fID - lag(fID, default = -1),
#            start_nID = ifelse(new_feature == 1, start_nID + 1, start_nID)) %>%
#     # Clean up the mess
#     select(fID, split_fID, start_nID, stop_nID = nID, -diff_len, -ideal_len, -len, -X, -Y)
#
#   split_nodes$start_nID[1] <- 1
#
#   split_points <- left_join(split_points, select(split_nodes, split_fID, start_nID, stop_nID), by = "split_fID")
#
#   new_line <- function(start_stop, coords) {
#     sf::st_linestring(as.matrix(coords[start_stop[1]:start_stop[2], c("X", "Y")]))
#   }
#
#   split_lines <- apply(as.matrix(split_points[c("start_nID", "stop_nID")]),
#                        MARGIN = 1, FUN = new_line, coords = coords)
#
#   split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))
#
#   return(split_lines)
# }
