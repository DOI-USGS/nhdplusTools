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
#' @param source_flowline sf data.frame with source flowlines and flowline attributes:
#' COMID, LENGTHKM, DnHydroseq, and Hydroseq, and LevelPathI or NHDPlusHR equivalents.
#' @param target_catchment sf data.frame with catchment polygons and FEATUREID or
#' NHDPlusHR equivalent.
#' @param target_flowline sf data.frame with target flowlines and COMID or NHDPlusHR
#' equivalent.
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
#'                      select(lp_df_df, NHDPlusID = members,
#'                             MR_LevelPathI = LevelPathI), by = "NHDPlusID")
#'
#' lp <- min(matched$MR_LevelPathI, na.rm = TRUE)
#' mr_lp <- filter(new_hope_flowline, LevelPathI <= lp)
#' hr_lp <- filter(matched, MR_LevelPathI <= lp)
#' plot(st_geometry(matched), col = "blue", lwd = 0.5)
#' plot(mr_lp$geom, col = "red", lwd = 3, add = TRUE)
#' plot(hr_lp$geom, col = "black", add = TRUE)
#'
match_flowpaths <- function(source_flowline, target_catchment, target_flowline) {
  flowline <- rename_nhdplus(source_flowline)

  check_names(flowline, "match_flowpaths")

  catchment <- rename_nhdplus(target_catchment)
  target_flowline <- clean_geom(target_flowline)

  ### First find outlet of MR catchments
  mr_hw_outlets <- mr_hw_cat_out(flowline)

  ### Find HR catchment of outlet of headwater MR catchments
  hr_pair <- st_join(mr_hw_outlets,
                     select(catchment, FEATUREID),
                     join = st_within) %>%
    st_set_geometry(NULL)

  ### Trace down HR network for each.
  mr_lps <- lapply(hr_pair$FEATUREID,
                   function(x, fa) get_DM(fa, x),
                   fa = target_flowline)

  # Expand into data.frame
  lp_df <- data.frame(FEATUREID = hr_pair$FEATUREID)
  lp_df["members"] <- list(mr_lps)

  lp_df <- unnest(lp_df)

  # Get MR levelpaths for headwater HR ids
  mr_lp <- distinct(select(st_set_geometry(flowline, NULL), COMID, LevelPathI))
  hr_pair <- left_join(hr_pair, mr_lp, by = "COMID")

  # Join so we have HR FEATUREID and MR LevelPath
  lp_df <- left_join(lp_df, select(hr_pair, -COMID), by = "FEATUREID")

  # discriminate which HR headwater belongs with which MR levelpath
  # Iterate over sorted unique list of MR levelpaths.
  lps <- sort(unique(lp_df$LevelPathI))
  lp_list <- setNames(rep(list(list()), length(lps)), lps)

  for(lp in lps) { # destructive loop
    # Find the HR headwater that has the most downstream members on that levelpath.
    lp_list[[as.character(lp)]] <- filter(lp_df, LevelPathI == lp)
    # Remove the match from the set and continue.
    lp_df <- filter(lp_df, !members %in% lp_list[[as.character(lp)]]$members)
  }

  return(bind_rows(lp_list))

}
