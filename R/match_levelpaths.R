#' Match Level Paths
#' @description Attempts to match dendritic hydrologic unit networks to NHDPlus
#' flowline level paths.
#' @param fline_hu sf data.frame flowlines intersected with hydrologic units
#' @param start_comid integer COMID to start search from.
#' @param add_checks boolean if TRUE, checks for toHUC errors are added.
#' @details ...
#'
#' @export
#' @importFrom sf st_cast st_union st_geometry st_sfc st_sf st_crs st_set_geometry st_line_merge st_geometry_type
#' @importFrom dplyr filter mutate left_join select distinct case_when
#' @importFrom tidyr unnest
#' @examples
#' #todo
#'
match_levelpaths <- function(fline_hu, start_comid, add_checks = FALSE) {
  check <- TRUE

  out_comid <- start_comid
  lp_hu <- list()

  next_lp <- c()

  nlp <- unique(filter(fline_hu, COMID == out_comid)[["LevelPathI"]])
  nlp_tracker <- c()
  none_count <- 0

  count <- 0

  fline_hu_save <- fline_hu
  HUC12_TOHUC <- distinct(select(fline_hu_save, HUC12, TOHUC))

  while(check == TRUE & count < 100000) {
    # get the HUC12s that intersect the nlp we are looking for.
    lp_hu_temp <- unique(fline_hu$HUC12[fline_hu$LevelPathI == nlp])# list(lp_intersect(wbd, lp))

    if(length(lp_hu_temp) == 1) if(is.na(lp_hu_temp)) lp_hu_temp <- character(0)

    if(length(lp_hu_temp) > 0) { # if hu12s are found
      # save that list for the nhdplus level path.
      lp_hu[as.character(nlp)] <- list(lp_hu_temp)

      # filter the found hu12s out of the set we search next time.
      fline_hu <- filter(fline_hu, !HUC12 %in% lp_hu[[as.character(nlp)]]) # faster with mutate to null?
      # record this one to zoom in on later.
      nlp_tracker <- c(nlp_tracker, nlp)
      # reset no match counter
      none_count <- 0
    } else {
      # increment no match counter
      none_count <- none_count + 1
      # print(none_count)
    }

    # If on the last nlp next_lp will be empty.
    if(length(next_lp) == 0 | none_count > 5) {
      next_lp <- filter(fline_hu,
                        fline_hu$DnLevelPat == nlp_tracker[1] &
                          !LevelPathI == nlp_tracker[1]) %>%
        group_by(LevelPathI) %>%
        filter(Hydroseq == min(Hydroseq)) %>%
        arrange(desc(denTotalAreaSqKM))
      next_lp <- next_lp[["LevelPathI"]]

      # nlp_tracker is the backlog that needs to be worked through.
      if(length(nlp_tracker) > 1) {
        nlp_tracker <- nlp_tracker[2:length(nlp_tracker)]
      } else {
        nlp_tracker <- c()
      }
    }

    nlp <- next_lp[1]

    if(length(next_lp) > 1) {
      next_lp <- next_lp[2:length(next_lp)]
    } else {
      next_lp <- c()
    }

    if(length(next_lp) == 0 & length(nlp_tracker) == 0) check <- FALSE
    count <- count + 1
    # print(length(nlp_tracker))
  }

  lp_hu_df <- data.frame(LevelPathI = names(lp_hu),
                         stringsAsFactors = FALSE)

  lp_hu_df[["HUC12"]] <- lp_hu
  lp_hu_df <- tidyr::unnest(lp_hu_df)

  head_hu <- lp_hu_df %>%
    left_join(select(fline_hu_save, Hydroseq,
                     nhd_LevelPath = LevelPathI, HUC12), by = "HUC12") %>%
    filter(LevelPathI == nhd_LevelPath) %>%
    group_by(LevelPathI) %>%
    filter(Hydroseq == max(Hydroseq)) %>%
    ungroup() %>%
    select(-Hydroseq, -nhd_LevelPath, head_HUC12 = HUC12)

  hu <- lp_hu_df %>%
    right_join(HUC12_TOHUC, by = "HUC12") %>%
    left_join(head_hu, by = "LevelPathI") %>%
    mutate(LevelPathI = as.numeric(LevelPathI)) %>%
    group_by(LevelPathI, TOHUC) %>%
    ungroup()

  outlet_hus <- fline_hu_save[which(fline_hu_save$COMID == start_comid),]$TOHUC
  hu_destructive <- filter(hu, !HUC12 %in% outlet_hus)

  hu[["main_LevelPathI"]] <- 0

  lps <- sort(unique(hu$LevelPathI))

  for(lp in lps) {
    # print(lp)
    lp <- filter(hu, LevelPathI == lp)
    # could gather_ds levelpath instead of hu but some toHUCs go outside the levelpath intersection!
    main_stem <- gather_ds(hu_destructive, lp$head_HUC12[1])
    hu <- mutate(hu, main_LevelPathI = ifelse(HUC12 %in% main_stem &
                                                main_LevelPathI == 0,
                                              lp$LevelPathI[1],
                                              main_LevelPathI))
    hu_destructive <- filter(hu_destructive, !HUC12 %in% main_stem)
  }

  hu_trib <- hu %>%
    left_join(distinct(select(fline_hu_save, HUC12,
                              check_LevelPathI = LevelPathI)),
              by = "HUC12") %>%
    # only modify ones not found to be on the main path in the loop above
    # and where the LevelPathI assigned is not equal to the original one assigned
    filter(main_LevelPathI == 0 & LevelPathI != check_LevelPathI) %>%
    group_by(HUC12) %>%
    # This grabs the biggest trib in the HU after filtering out
    # the originally assigned one.
    filter(check_LevelPathI == min(check_LevelPathI))

  hu <- hu %>%
    left_join(select(hu_trib, HUC12, check_LevelPathI), by = "HUC12") %>%
    mutate(LevelPathI = as.numeric(ifelse(main_LevelPathI == 0,
                                          check_LevelPathI,
                                          main_LevelPathI))) %>%
    select(-check_LevelPathI)

  hu_trib2 <- left_join(hu, select(fline_hu_save, HUC12,
                            check_LevelPathI = LevelPathI),
                 by = "HUC12") %>%
    group_by(HUC12) %>%
    # When nothing in the group was found intersecting the levelpath
    # it should actually me on another level path.
    filter(!any(LevelPathI == check_LevelPathI) &
             check_LevelPathI == min(check_LevelPathI)) %>%
    distinct() %>%
    ungroup()

  hu <- hu %>%
    left_join(select(hu_trib2, HUC12, check_LevelPathI), by = "HUC12") %>%
    mutate(LevelPathI = as.numeric(ifelse(!is.na(check_LevelPathI),
                                          check_LevelPathI,
                                          LevelPathI))) %>%
    select(HUC12, TOHUC, LevelPathI, head_HUC12)

  if(add_checks) {
    hu <- mutate(hu, trib_intersect = ifelse(HUC12 %in% hu_trib$HUC12, TRUE, FALSE),
                 trib_no_intersect = ifelse(HUC12 %in% hu_trib2$HUC12, TRUE, FALSE))
  }
  return(hu)
}

gather_ds <- function(hu_data, huc12) {
  next_huc12 <- unique(hu_data[["TOHUC"]][hu_data[["HUC12"]] %in% huc12])
  if(length(next_huc12) == 0) {
    return()
  }
  return(c(huc12, gather_ds(hu_data, next_huc12)))
}
