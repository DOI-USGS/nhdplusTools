#' @importFrom sf st_sf st_coordinates st_as_sf st_crs
#' @importFrom dplyr left_join select filter group_by ungroup bind_cols
mr_hw_cat_out <- function(mr_fline) {
  mr_fline <- prepare_nhdplus(mr_fline, 0, 0, 0, warn = FALSE) %>%
    left_join(select(mr_fline, COMID), by = "COMID") %>%
    st_sf() %>%
    filter(!COMID %in% toCOMID)

  outlets <-  mr_fline %>%
    st_coordinates() %>%
    as.data.frame() %>%
    group_by(L2) %>%
    filter(row_number() == round(n()/2)) %>%
    ungroup() %>%
    select(X, Y) %>%
    st_as_sf(coords = c("X", "Y"))

  bind_cols(outlets, select(st_set_geometry(mr_fline, NULL), COMID)) %>%
    st_sf(crs = st_crs(mr_fline))
}

clean_geom <- function(x) {
  if("sf" %in% class(x)) {
    st_set_geometry(x, NULL)
  } else {
    x
  }
}

#' Match Flowpaths
#' @description Implements a flowpath-matching algorithm that traces downstream along
#' the target flowline network and determines which levelpath from the source flowlines
#' best matches the resulting downstream traces. The algorithm starts from the outlet
#' location of the upstream most catchment in the source flowlines to stay away from
#' complexity that occurs near drainage divides.
#'
#' For preformance reasons, network navigation only considers the network of levelpaths.
#' As a result, the response includes all flowlines along headwater levelpaths,
#' including those upstream of the outlet of the headwater catchment.
#'
#' @param source_flowline sf data.frame with source flowlines and flowline attributes:
#' COMID, LENGTHKM, DnHydroseq, and Hydroseq, and LevelPathI or NHDPlusHR equivalents.
#' @param target_catchment sf data.frame with catchment polygons and FEATUREID or
#' NHDPlusHR equivalent.
#' @param target_flowline sf data.frame with target flowlines and COMID or NHDPlusHR
#' equivalent.
#' @param hr_pair (advanced use) data.frame as output by get_hr_pair internal function.
#' @param cores Will run search in parallel
#' @export
#' @importFrom sf st_join st_set_geometry st_within
#' @importFrom tidyr unnest
#' @importFrom dplyr select distinct  left_join bind_rows
#' @examples
#' library(dplyr)
#' library(sf)
#' source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))
#' source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#'
#' lp_df_df <- match_flowpaths(source_flowline = new_hope_flowline,
#'                            target_catchment = hr_catchment,
#'                            target_flowline = hr_flowline)
#' matched <- left_join(select(hr_flowline, NHDPlusID),
#'                      select(lp_df_df, NHDPlusID,
#'                             MR_LevelPathI = LevelPathI), by = "NHDPlusID")
#'
#' lp <- min(matched$MR_LevelPathI, na.rm = TRUE)
#' mr_lp <- filter(new_hope_flowline, LevelPathI <= lp)
#' hr_lp <- filter(matched, MR_LevelPathI <= lp)
#' plot(st_geometry(matched), col = "blue", lwd = 0.5)
#' plot(mr_lp$geom, col = "red", lwd = 3, add = TRUE)
#' plot(hr_lp$geom, col = "black", add = TRUE)
#'
match_flowpaths <- function(source_flowline, target_catchment, target_flowline,
                            hr_pair = NULL,
                            cores = NULL) {

  source_flowline <- rename_nhdplus(source_flowline)
  check_names(source_flowline, "match_flowpaths")

  required_names <- unique(c(get("match_flowpaths_attributes",
                                 nhdplusTools_env),
                             get("prepare_nhdplus_attributes",
                                 nhdplusTools_env)))

  source_flowline <- select(source_flowline, required_names)

  target_flowline <- clean_geom(target_flowline)

  if(is.null(hr_pair)) {
    target_catchment <- rename_nhdplus(target_catchment)
    hr_pair <- get_hr_pair(mr_hw_cat_out(source_flowline), target_catchment)
  }

  source_flowline <- clean_geom(source_flowline)
  mr_lp <- distinct(select(source_flowline, COMID, LevelPathI))
  rm(source_flowline)

  gc()

  hr_pair <- filter(hr_pair, FEATUREID %in% target_flowline$NHDPlusID) %>%
    left_join(select(target_flowline, NHDPlusID, LevelPathI),
              by = c("FEATUREID" = "NHDPlusID"))

  target_fp <- select(target_flowline, "DnLevelPat", "LevelPathI", "HydroSeq") %>%
    group_by(LevelPathI) %>%
    filter(HydroSeq == min(HydroSeq)) %>%
    select(-HydroSeq) %>%
    ungroup()

  target_flowline <- select(target_flowline, NHDPlusID, LevelPathI)

  get_dm_lp_rec <- function(in_lp, fp, log_file) {
    cat(paste(Sys.time(), ": ", in_lp, "\n"), file = log_file, append = TRUE)

    dn_lp <- fp$DnLevelPat[fp$LevelPathI %in% in_lp]

    if(!is.null(dn_lp) && length(dn_lp) > 0 && in_lp != dn_lp) {
      c(in_lp, get_dm_lp(dn_lp, fp))
    } else {
      in_lp
    }
  }

  # # graph approach -- slower
  # get_dm_lp <- function(in_lp, fp, log_file) {
  #   cat(paste(in_lp$LevelPathI, "\n"), file = log_file, append = TRUE)
  #   names(igraph::shortest_paths(fp,
  #                          in_lp$LevelPathI,
  #                          in_lp$TerminalPa,
  #                          mode = "in")$vpath[[1]])
  # }
  #
  # temp_hr <- split(dplyr::mutate_all(hr_pair[, 3:4], as.character), seq(nrow(hr_pair)))
  # temp_graph <- graph_from_data_frame(target_fp, directed = TRUE)
  #
  # cl <- parallel::makeCluster(rep("localhost", 2), type = "SOCK")
  # mr_lps <- parLapply(cl, temp_hr[1:200],
  #                     get_dm_lp,
  #                     fp = temp_graph, log_file = "partest.log")
  # parallel::stopCluster(cl)
  #
  #
  # ti <- Sys.time()
  # ### Trace down HR network for each.
  # mr_lps <- lapply(temp_hr[1:10],
  #                  get_dm_lp,
  #                  fp = temp_graph, log_file = "partest.log")
  # print(Sys.time() - ti)

  ### Trace down HR network for each.
  mr_lps <- lapply(hr_pair$LevelPathI,
                   get_dm_lp_rec,
                   fp = target_flowline,
                   log_file = "partest.log")

  # Expand into data.frame
  lp_df <- data.frame(FEATUREID = hr_pair$FEATUREID)
  lp_df["member_hr_lp"] <- list(mr_lps)

  rm(mr_lps)

  lp_df <- unnest(lp_df)

  # Get MR levelpaths for headwater HR ids
  hr_pair <- left_join(select(hr_pair, -LevelPathI), mr_lp, by = "COMID")

  # Join so we have HR FEATUREID and MR LevelPath
  lp_df <- left_join(lp_df, select(hr_pair, -COMID), by = "FEATUREID")

  group_by(lp_df, member_hr_lp) %>%
    filter(LevelPathI == min(LevelPathI)) %>%
    ungroup() %>%
    left_join(target_flowline,
              by = c("member_hr_lp" = "LevelPathI"))
}

get_hr_pair <- function(mr_hw_outlets, target_catchment) {
  ### Find HR catchment of outlet of headwater MR catchments
  hr_pair <- st_join(mr_hw_outlets,
                     select(target_catchment, FEATUREID),
                     join = st_within) %>%
    st_set_geometry(NULL)
}
