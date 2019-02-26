#' Match Level Paths
#' @description Attempts to match dendritic hydrologic unit networks to level paths.
#' @param fline_hu sf data.frame flowlines intersected with hydrologic units containing
#' COMID, Hydroseq, LevelPathI, DnLevelPat, denTotalAreaSqKM, HUC12, TOHUC attributes.
#' @param start_comid integer COMID to start search from.
#' @param add_checks boolean if TRUE, checks for toHUC errors are added.
#' @details ...
#'
#' @export
#' @importFrom sf st_cast st_union st_geometry st_sfc st_sf st_crs st_set_geometry st_line_merge st_geometry_type
#' @importFrom dplyr filter mutate left_join select distinct case_when rename arrange
#' @importFrom tidyr unnest
#' @examples
#' #todo
#'
match_levelpaths <- function(fline_hu, start_comid, add_checks = FALSE) {

  check_names(names(fline_hu), "match_levelpaths")

  #############################################################################
  # get_lp_hu gets levelpath / hydrologic unit pairs from the intersection set.
  #############################################################################
  hu <- get_lp_hu(fline_hu, start_comid)

  #############################################################################
  # create hu with head_hu for each levelpath. label levelpath as "intersected"
  #############################################################################
  hu <- hu %>%
    right_join(distinct(select(fline_hu, HUC12, TOHUC)), by = "HUC12") %>%
    left_join(get_head_hu(hu, fline_hu), by = "LevelPathI") %>%
    mutate(LevelPathI = as.numeric(LevelPathI)) %>%
    rename(intersected_LevelPathI = LevelPathI)

  # In case the outlet flowline spans multiple HUs near the outlet.
  outlet_hu <- sort(fline_hu[which(fline_hu$COMID == start_comid),]$TOHUC,
                     decreasing = FALSE)[1]


  #################################################################
  # trace from head_hu to outlet, match paths and reconcile issues.
  #################################################################
  hu <- trace_hu_network(hu, outlet_hu, fline_hu)
  funky_headwaters <- hu$funky_headwaters
  hu <- hu$hu

  #############################################################################
  # Need to fix head_HUC12 where mainstem outlets were changed in trace_hu_network.
  # These are the ones that are part of a tributary so main_LeveLPath got updated above.
  # main_LevelPath == 0 is a different case.
  #############################################################################
  correct_heads <- hu %>%
    filter(intersected_LevelPathI == main_LevelPathI & main_LevelPathI != 0) %>%
    select(main_LevelPathI, correct_head_HUC12 = head_HUC12) %>%
    distinct()

  hu <- left_join(hu, correct_heads, by = "main_LevelPathI") %>%
    mutate(head_HUC12 = ifelse(intersected_LevelPathI != main_LevelPathI & main_LevelPathI != 0,
                               correct_head_HUC12,
                               head_HUC12)) %>%
    select(-correct_head_HUC12) %>%
    distinct()

  #############################################################################
  # Odd situation where a single headwater catchment intersects two HUs causes duplicate HUC12s
  #############################################################################
  hu <- group_by(hu, HUC12) %>%
    arrange(head_HUC12) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup()

  #############################################################################
  # main_levelpath was found by tracing toHUC downstream.
  # intersected_LevelPath was passed in from spatial intersection
  # Not all "main_levelpaths" got filled in.
  # The goal of the following code is to get a "corrected_levelpath"
  #############################################################################

  #############################################################################
  # For outlets, just set main_levelpath to intersected.
  #############################################################################
  hu <- mutate(hu, main_LevelPathI = ifelse(HUC12 == outlet_hu, intersected_LevelPathI, main_LevelPathI))

  #############################################################################
  # Run corrections on results
  #############################################################################
  return(correct_hu(hu, fline_hu, funky_headwaters, add_checks))
}

gather_ds <- function(hu_data, huc12) {
  next_huc12 <- unique(hu_data[["TOHUC"]][hu_data[["HUC12"]] %in% huc12])
  if(length(next_huc12) == 0) {
    return()
  }
  return(c(huc12, gather_ds(hu_data, next_huc12)))
}

get_head_hu <- function(lp_hu, fline_hu) {
  lp_hu %>%
    left_join(select(fline_hu, Hydroseq,
                     nhd_LevelPath = LevelPathI, HUC12), by = "HUC12") %>%
    filter(LevelPathI == nhd_LevelPath) %>%
    group_by(LevelPathI) %>%
    filter(Hydroseq == max(Hydroseq)) %>%
    ungroup() %>%
    select(-Hydroseq, -nhd_LevelPath, head_HUC12 = HUC12)
}

get_lp_hu <- function(fline_hu, start_comid) {

  nlp <- unique(filter(fline_hu, COMID == start_comid)[["LevelPathI"]])

  check <- TRUE
  lp_hu <- list() # List to store levelpath/HU pairs
  next_lp <- c() # Vector for sets of levelpaths that are at the next level from current.
  nlp_tracker <- c() # Tracker for levelpaths that need to be descended into later.
  count <- 0 # Stop checker for while loop.
  none_count <- 0 # performance improvement to not check too much stuff.

  # There's a chance that this search could be done with an artfully crafted
  # grouped filter but I've not been able to wrap my head around getting it
  # right in all cases.
  while(check == TRUE & count < 100000) {
    # get the HUC12s that intersect the nlp we are looking for.
    lp_hu_temp <- unique(fline_hu$HUC12[fline_hu$LevelPathI == nlp])

    if(length(lp_hu_temp) == 1) if(is.na(lp_hu_temp)) lp_hu_temp <- character(0)

    if(length(lp_hu_temp) > 0) { # if hu12s are found
      lp_hu[as.character(nlp)] <- list(lp_hu_temp) # save that list.

      # filter the found hu12s out of the set we search next time.
      fline_hu <- filter(fline_hu, !HUC12 %in% lp_hu[[as.character(nlp)]])

      nlp_tracker <- c(nlp_tracker, nlp) # record this one to zoom in on later.

      # reset no match counter
      none_count <- 0
    } else {
      none_count <- none_count + 1
    }

    if(length(next_lp) == 0 | none_count > 5) { # If on the last nlp next_lp will be empty.
      # Grab all the levelpaths that intersect the one we are on.
      next_lp <- filter(fline_hu,
                        fline_hu$DnLevelPat == nlp_tracker[1] &
                          !LevelPathI == nlp_tracker[1]) %>%
        group_by(LevelPathI) %>%
        # Pick only the outlet catchment/flowline
        filter(Hydroseq == min(Hydroseq)) %>%
        # Sort from biggest drainage area to smallest.
        arrange(desc(denTotalAreaSqKM))

      next_lp <- next_lp[["LevelPathI"]]


      if(length(nlp_tracker) > 1) { # maintain backlog that needs to be worked through.
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
  }

  return(data.frame(LevelPathI = names(lp_hu),
                    HUC12 = I(lp_hu),
                    stringsAsFactors = FALSE) %>%
           tidyr::unnest())
}

trace_hu_network <- function(hu, outlet_hu, fline_hu) {
  # Make sure we don't have the outlet in scope so the network breaks where we want.
  hu_destructive <- filter(hu, !HUC12 %in% outlet_hu)

  hu[["main_LevelPathI"]] <- 0
  hu[["outlet_HUC12"]] <- ""

  lps <- sort(unique(hu$intersected_LevelPathI))
  funky_headwaters <- c()
  broken_path <- c()

  count <- 0
  for(lp in lps) {
    lp <- filter(hu, intersected_LevelPathI == lp)
    # could gather_ds levelpath instead of hu but some toHUCs go outside the levelpath intersection!
    main_stem <- tryCatch(gather_ds(hu_destructive, lp$head_HUC12[1]),
                          error = function(e) {
                            warning(paste("something real bad happened with levelpath",
                                          lp$intersected_LevelPathI[1]))
                            funky_headwaters <- c(funky_headwaters, lp$head_HUC12[1])})

    # If any of the HUs found are still available to be allocated...
    if(any(main_stem %in% hu_destructive$HUC12)) {
      # If the main stem found doesn't follow the expected path at all.
      # Then something is wrong with the head_HUC12 and we need to try a different one.
      candidates <- main_stem[!main_stem == lp$head_HUC12[1]]
      if(!any(candidates %in% lp$HUC12) &
         length(main_stem) > 1) {

        checker <- filter(fline_hu, HUC12 %in% candidates & LevelPathI == lp$intersected_LevelPathI[1])
        if(!any(candidates %in% checker$HUC12)) {
          # Need to find the actual top HU12 first grab all that intersect this LP.
          top_cat <- filter(fline_hu, LevelPathI == lp$intersected_LevelPathI[1]) %>%
            # In case there is a catchment completely outside the HU
            # We have to find the one that overlaps the boundary!!
            group_by(COMID) %>% filter(n() > 1) %>% ungroup() %>%
            # Now grab the top most row that isn't the one we found above.
            filter(Hydroseq == max(Hydroseq) & HUC12 != lp$head_HUC12[1])

          # No test for this, but it shouldn't happen so throw error!!
          if(nrow(top_cat) > 1) {
            warning(paste("something very wrong with headwaters around HUC",
                          lp$head_HUC12[1], "and levelpath", lp$intersected_LevelPathI[1]))
            funky_headwaters <- c(funky_headwaters, top_cat$HUC12)
            main_stem <- c()
          } else if(nrow(top_cat) == 0) {
            warning(paste("broken path along levelpath", lp$intersected_LevelPathI, "passing by."))
            broken_path <- c(broken_path, lp$head_HUC12)
          } else {
            # set and rerun.
            lp$head_HUC12 <- top_cat$HUC12
            funky_headwaters <- c(funky_headwaters, top_cat$HUC12)
            main_stem <- gather_ds(hu_destructive, lp$head_HUC12[1])
          }
        }
      }

      hu <- mutate(hu, main_LevelPathI = ifelse(HUC12 %in% main_stem &
                                                  main_LevelPathI == 0,
                                                lp$intersected_LevelPathI[1],
                                                main_LevelPathI),
                   outlet_HUC12 = ifelse(HUC12 %in% main_stem,
                                         main_stem[length(main_stem)],
                                         outlet_HUC12))
      hu_destructive <- filter(hu_destructive, !HUC12 %in% main_stem)
    }
    count <- count + 1
    if (count %% 100 == 0) {
      message(paste(count, "of", length(lps), "levelpaths"))
    }
  }
  return(list(hu = hu, funky_headwaters = funky_headwaters, broken_path = broken_path))
}

correct_hu <- function(hu, fline_hu, funky_headwaters, add_checks) {
  ################################################################################
  # HUs found through intersection but not through main path trace belong with trib
  ################################################################################
  hu_trib <- hu %>%
    left_join(distinct(select(fline_hu, HUC12, check_LevelPathI = LevelPathI)), by = "HUC12") %>%
    # only modify ones not found to be on the main path in the loop above
    # and where the LevelPathI assigned is not equal to the original one assigned
    filter(main_LevelPathI == 0 & intersected_LevelPathI != check_LevelPathI) %>%
    group_by(HUC12) %>%
    # This grabs the biggest trib in the HU after filtering out the originally assigned one.
    filter(check_LevelPathI == min(check_LevelPathI))

  hu <- hu %>%
    left_join(select(hu_trib, HUC12, check_LevelPathI), by = "HUC12") %>%
    # Kill head_HUC12 which is now wrong.
    mutate(head_HUC12 = ifelse(main_LevelPathI == 0, "", head_HUC12),
           # The change to main_LevelPath makes this one an outlet.
           outlet_HUC12 = ifelse(main_LevelPathI == 0 & !is.na(check_LevelPathI), HUC12, outlet_HUC12),
           # Update the main_levelpath where needed per hu_trib from above.
           main_LevelPathI = as.numeric(ifelse(main_LevelPathI == 0, check_LevelPathI, main_LevelPathI)),
           main_LevelPathI = ifelse(is.na(main_LevelPathI), intersected_LevelPathI, main_LevelPathI)) %>%
    select(-check_LevelPathI)

  ################################################################################
  # HUs found along trace but not along intersection need to be flagged and checked
  ################################################################################
  hu_trib2 <- left_join(hu, select(fline_hu, HUC12, check_LevelPathI = LevelPathI), by = "HUC12") %>%
    group_by(HUC12) %>%
    # When nothing in the group was found intersecting the levelpath
    # it should actually me on another level path.
    filter(!any(main_LevelPathI == check_LevelPathI) & check_LevelPathI == min(check_LevelPathI)) %>%
    distinct() %>%
    ungroup()

  hu <- hu %>%
    left_join(select(hu_trib2, HUC12, check_LevelPathI), by = "HUC12") %>%
    mutate(main_LevelPathI = as.numeric(ifelse(!is.na(check_LevelPathI), check_LevelPathI, main_LevelPathI))) %>%
    select(HUC12, TOHUC, intersected_LevelPathI, corrected_LevelPathI = main_LevelPathI, head_HUC12, outlet_HUC12)

  hu <- filter(hu, !is.na(intersected_LevelPathI))

  ################################################################################
  # Update head_hu where they were broken before
  ################################################################################
  head_hu <- hu %>%
    filter(head_HUC12 == "") %>%
    select(LevelPathI = corrected_LevelPathI, HUC12) %>%
    get_head_hu(fline_hu) %>%
    select(LevelPathI, update_head_HUC12 = head_HUC12)

  hu <- hu %>%
    left_join(head_hu, by = c("corrected_LevelPathI" = "LevelPathI")) %>%
    mutate(head_HUC12 = ifelse(head_HUC12 == "", update_head_HUC12, head_HUC12)) %>%
    select(-update_head_HUC12)

  # need to deduplicate resulting head_HUC12 in some edge cases
  lp_head <- select(hu, corrected_LevelPathI, head_HUC12) %>%
    distinct() %>%
    group_by(corrected_LevelPathI) %>%
    arrange(head_HUC12) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup()

  hu <- hu %>%
    left_join(select(lp_head, corrected_LevelPathI, updated_head_HUC12 = head_HUC12),
              by = "corrected_LevelPathI") %>%
    select(-head_HUC12) %>% rename(head_HUC12 = updated_head_HUC12)

  ################################################################################
  # Fix up outlets that got broken in edge cases above.
  ################################################################################
  if(any(hu$outlet_HUC12 == "")) {
    lp_outlet <- select(hu, corrected_LevelPathI, outlet_HUC12) %>%
      filter(outlet_HUC12 != "") %>%
      distinct()

    hu <- hu %>%
      left_join(select(lp_outlet, corrected_LevelPathI, updated_outlet_HUC12 = outlet_HUC12),
                by = "corrected_LevelPathI") %>%
      select(-outlet_HUC12) %>% rename(outlet_HUC12 = updated_outlet_HUC12) %>%
      filter(outlet_HUC12 != "")
  }

  lp_outlet <- select(hu, corrected_LevelPathI, outlet_HUC12) %>%
    distinct() %>%
    group_by(corrected_LevelPathI) %>%
    arrange(dplyr::desc(outlet_HUC12)) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup()

  hu <- hu %>%
    left_join(select(lp_outlet, corrected_LevelPathI, updated_outlet_HUC12 = outlet_HUC12),
              by = "corrected_LevelPathI") %>%
    select(-outlet_HUC12) %>% rename(outlet_HUC12 = updated_outlet_HUC12) %>%
    distinct()

  ################################################################################
  # Add checks
  ################################################################################
  if(add_checks) {
    hu <- mutate(hu, trib_intersect = HUC12 %in% hu_trib$HUC12,
                 trib_no_intersect = HUC12 %in% hu_trib2$HUC12,
                 headwater_error = HUC12 %in% funky_headwaters)
  }
  return(hu)
}
