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

#' @title split lines 2
#' @description Splits lines longer than a given threshold into the
#' minimum number of pieces to all be under the given threshold.
#' Does not use lwgeom and may not work in all cases.
#' @param input_lines data.frame of class sf with LINESTRING sfc column.
#' @param max_length maximum segment length to return
#' @param id name of ID column in data.frame
#' @return only the split lines.
#' @importFrom dplyr group_by ungroup filter select mutate
#' @importFrom stats setNames
#'
# Non lwgeom method
split_lines_2 <- function(input_lines, max_length, id = "ID") {
  if(max_length < 50) warning("short max length detected, do you have your units right?")

  geom_column <- attr(input_lines, "sf_column")

  input_crs <- sf::st_crs(input_lines)

  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- filter(select(input_lines, id, geom_column, geom_len),
                     geom_len >= max_length)

  rm(input_lines)

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_len / max_length),
                     piece_len = (geom_len / pieces),
                     fID = 1:nrow(too_long))

  split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),]

  split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
    select(-geom_len, -pieces) %>%
    group_by(fID) %>%
    mutate(ideal_len = cumsum(piece_len)) %>%
    mutate(start = dplyr::lag(ideal_len, default = 0, order_by = split_fID)) %>%
    ungroup()

  get_coords <- function(too_long) {
    coords <- data.frame(sf::st_coordinates(too_long[[geom_column]]))

    rename(coords, fID = L1) %>% mutate(nID = 1:nrow(coords))
  }

  coords <- get_coords(too_long)

  node_count <- select(coords, fID) %>%
    group_by(fID) %>%
    summarize(count = n()) %>%
    left_join(select(sf::st_set_geometry(too_long, NULL), fID, pieces), by = "fID")

  problem_geoms <- (node_count$pieces*5) > node_count$count
  if(any(problem_geoms)) {
    warning(paste("Found", length(which(problem_geoms)),
                  "geometries without very many vertices. Densifying"))
    for(g in which(problem_geoms)) {
      too_long$geometry[g] <- sf::st_segmentize(too_long$geometry[g], max_length / (5*too_long$pieces[g]))
    }
    coords <- get_coords(too_long)
  }

  rm(too_long)

  split_nodes <- group_by(coords, fID) %>%
    # First calculate cumulative length by feature.
    mutate(len  = sqrt(((X - (lag(X)))^2) + (((Y - (lag(Y)))^2)))) %>%
    mutate(len = ifelse(is.na(len), 0, len)) %>%
    mutate(len = cumsum(len)) %>%
    # Now join nodes to split points -- this generates all combinations.
    left_join(select(split_points, fID, start, ideal_len, split_fID), by = "fID") %>%
    group_by(split_fID) %>%
    # Get rid of the ones we don't want to keep
    filter(len >= start & len <= ideal_len) %>%
    arrange(split_fID, len) %>%
    mutate(new_index = 1, new_index = c(new_index[-n()], 3)) %>%
    ungroup() %>% group_by(fID) %>% arrange(fID, split_fID, len) %>%
    mutate(new_index = c(new_index[-n()], 1)) %>%
    ungroup() %>%
    mutate(new_index = cumsum(lag(new_index, default = 0)) + 1) %>%
    right_join(setNames(data.frame(1:max(.$new_index)), "new_index"), by = c("new_index")) %>%
    mutate(split_fID = ifelse(is.na(split_fID), lag(split_fID), split_fID),
           fID = ifelse(is.na(fID), lag(fID), fID)) %>%
    mutate(split_fID = ifelse(is.na(split_fID), lead(split_fID), split_fID),
           fID = ifelse(is.na(fID), lead(fID), fID)) %>%
    mutate(len = ifelse(is.na(len), lag(ideal_len), len)) %>%
    mutate(len = ifelse(is.na(len), lead(start), len)) %>%
    select(-start, -ideal_len, -nID) %>% rename(nID = new_index) %>%
    # https://math.stackexchange.com/questions/175896/
    # finding-a-point-along-a-line-a-certain-distance-away-from-another-point
    mutate(dist_ratio = ifelse(is.na(X) & is.na(lead(X)),
                               (len-lag(len)) /
                                 (lead(len,2)-lag(len,1)), NA)) %>%
    mutate(X = ifelse((is.na(X) & is.na(lead(X))), (1 - dist_ratio)*lag(X) + dist_ratio*lead(X, 2), X),
           Y = ifelse((is.na(Y) & is.na(lead(Y))), (1 - dist_ratio)*lag(Y) + dist_ratio*lead(Y, 2), Y)) %>%
    mutate(X = ifelse(is.na(X), lag(X), X), Y = ifelse(is.na(Y), lag(Y), Y)) %>%
    mutate(breaks = ifelse(!is.na(lag(dist_ratio)), 1, fID- lag(fID, default = 0))) %>%
    mutate(breaks = ifelse(lead(breaks == 1), 2, breaks))

  split_nodes$breaks[nrow(split_nodes)] <- 2

  starts_stops <- data.frame(start = which(split_nodes$breaks == 1), stop = which(split_nodes$breaks == 2))

  starts_stops <- cbind(split_nodes[,c("fID", "split_fID")][starts_stops[["start"]], ], starts_stops)

  if(nrow(split_points) != nrow(starts_stops)) {
    stop("After splitting, some fIDs were lost. Can't continue. Not enough nodes?")
  }

  new_line <- function(start_stop, coords) {
    sf::st_linestring(as.matrix(coords[start_stop[1]:start_stop[2], c("X", "Y")]))
  }

  split_lines <- apply(as.matrix(starts_stops[c("start", "stop")]),
                       MARGIN = 1, FUN = new_line, coords = split_nodes)

  split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))

  return(split_lines)
}
