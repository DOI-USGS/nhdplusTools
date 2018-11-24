#' @title Collapse NHDPlus Network
#' @description Refactors the NHDPlus flowline network, eliminating short and
#' non-cconfluence flowlines.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, Hydroseq, and LevelPathI columns
#' @param thresh numeric a length threshold (km). Flowlines shorter than this
#' will be eliminated
#' @param add_category boolean if combination category is desired in output, set
#' to TRUE
#' @param mainstem_thresh numeric threshold for combining inter-confluence
#' mainstems
#' @param exclude_cats integer vector of COMIDs to be excluded from collapse modifications.
#' @param warn boolean controls whether warning an status messages are printed
#' @return A refactored network with merged up and down flowlines.
#' @importFrom dplyr filter left_join select mutate group_by ungroup summarise
#' distinct
#' @seealso The \code{\link{refactor_nhdplus}} function implements a complete
#' workflow using `collapse_flowlines()`.
#' @export
#'
collapse_flowlines <- function(flines, thresh, add_category = FALSE,
                               mainstem_thresh = NULL, exclude_cats = NULL,
                               warn = TRUE) {

  check_names(names(flines), "collapse_flowlines")

  # very large thresh
  if (is.null(mainstem_thresh)) {
    mainstem_thresh_use <- max(flines$LENGTHKM)
  } else {
    mainstem_thresh_use <- mainstem_thresh
  }

  original_fline_atts <- flines

  short_outlets_tracker <- c()
  headwaters_tracker <- c()
  removed_mainstem <- data.frame(removed_COMID = c(), stringsAsFactors = FALSE)

  #######################################################
  # Short Outlets
  #######################################################
  # Need to clean up the short outlets first so they don't get broken
  # by the method below.

  flines <- collapse_outlets(flines, thresh, original_fline_atts, exclude_cats, warn)

  short_outlets_tracker <- flines[["short_outlets_tracker"]]
  flines <- flines[["flines"]]

  #######################################################
  # Short Headwaters
  #######################################################
  # Next merge headwaters that don't cross confluences downstream

  if (!"joined_toCOMID" %in% names(flines)) flines$joined_toCOMID <- NA
  if (!"joined_fromCOMID" %in% names(flines)) flines$joined_fromCOMID <- NA

  # Clean up short headwaters that aren't handled by the next downstream
  # logic above.
  remove_headwaters <- function(flines) {
    !(flines$COMID %in% flines$toCOMID) & # a headwater (nothing flows to it)
      !flines$COMID %in% exclude_cats & # Not in exclude list.
      flines$LENGTHKM < thresh & # shorter than threshold
      is.na(flines$joined_fromCOMID) &
      is.na(flines$joined_toCOMID) &
      !is.na(flines$toCOMID) &
      flines$toCOMID != -9999
  }

  flines <- reconcile_downstream(flines, remove_headwaters,
                                 remove_problem_headwaters = TRUE)

  headwaters_tracker <- flines[["removed_tracker"]]
  flines <- flines[["flines"]]

  #######################################################
  # Short Interconfluence Flowpath Top Flowlines
  #######################################################
  # If mainstems are being collapsed at a threshold, these need to be
  # treated seperately
  mainstem_top_tracker <- NULL
  if (!is.null(mainstem_thresh)) {
    flines$num_upstream <- get_num_upstream(flines)
    flines$ds_num_upstream <- get_ds_num_upstream(flines)

    # At the top of a mainstem
    # shorter than mainstem threshold
    # is still in scope
    remove_mainstem_top <- function(flines) {
      (flines$num_upstream > 1 &
         flines$ds_num_upstream == 1) &
        flines$LENGTHKM < mainstem_thresh_use &
        !is.na(flines$toCOMID) &
        !flines$COMID %in% exclude_cats
    }

    flines <- reconcile_downstream(flines, remove_mainstem_top,
                                   remove_problem_headwaters = FALSE)
    mainstem_top_tracker <- flines[["removed_tracker"]]
    flines <- flines[["flines"]]

    flines <- select(flines, -num_upstream, -ds_num_upstream)
  }

  #######################################################
  # Short Interconfluence Flowpath Flowlines
  #######################################################
  # Combine along main stems with no confluences.

  flines <- mutate(flines,
                   dsLENGTHKM = get_dsLENGTHKM(flines),
                   ds_num_upstream = get_ds_num_upstream(flines))

                            # is still in scope
  reroute_mainstem_set <- (flines$dsLENGTHKM > 0 &
                             # wasn't already collapsed as a headwater
                             is.na(flines$joined_toCOMID) &
                             # is not upstream of a confluence
                             flines$ds_num_upstream == 1 &
                             # is shorter than the mainstem threshold
                             flines$dsLENGTHKM < mainstem_thresh_use &
                             !flines$toCOMID %in% exclude_cats)


  # This is the set that is going to get skipped in the rerouting.
  removed_mainstem <- data.frame(removed_COMID =
                                   flines[["toCOMID"]][reroute_mainstem_set],
                                 joined_fromCOMID =
                                   flines[["COMID"]][reroute_mainstem_set],
                                 stringsAsFactors = FALSE)

  flines <- reconcile_removed_flowlines(flines,
                                        reroute_mainstem_set,
                                        removed_mainstem,
                                        original_fline_atts)

  if (nrow(removed_mainstem) > 0) removed_mainstem$joined_toCOMID <- NA

  if (!is.null(mainstem_thresh) & !is.null(mainstem_top_tracker)) {
    removed_mainstem_top <-
      left_join(data.frame(removed_COMID = mainstem_top_tracker,
                           joined_fromCOMID = NA, stringsAsFactors = FALSE),
                select(flines, COMID, joined_toCOMID),
                by = c("removed_COMID" = "COMID"))
    removed_mainstem <- rbind(removed_mainstem, removed_mainstem_top)
  }
  #######################################################
  # Short Single Confluence to Confluence Flowpaths
  #######################################################
  # Combine accross confluences next -
  # slightly different logic from "mainstems"

  flines <- mutate(flines,
                   dsLENGTHKM = get_dsLENGTHKM(flines),
                   ds_num_upstream = get_ds_num_upstream(flines))

  # This is the set that is going to get
  # rerouted removing their next downstream.
  # removes all terminal and already removed
  # flowlines from consideration.
  reroute_confluence_set <- (flines$dsLENGTHKM < thresh &
                               flines$dsLENGTHKM > 0 &
                               flines$ds_num_upstream > 1 &
                               !flines$toCOMID %in% exclude_cats)

  flines <- select(flines, -ds_num_upstream)

  # This is the set that is going to get skipped in the rerouting.
  removed_confluence <-
    data.frame(removed_COMID = flines[["toCOMID"]][reroute_confluence_set],
               joined_fromCOMID = flines[["COMID"]][reroute_confluence_set],
               stringsAsFactors = FALSE)

  # Need to deduplicate the upstream that removed will get collapsed into.
  removed_confluence <-
    group_by(left_join(removed_confluence,
                       select(original_fline_atts, COMID,
                              # Get upstream area and length
                              usLevelPathI = LevelPathI),
                       by = c("joined_fromCOMID" = "COMID")),
             removed_COMID)

  removed_confluence <-
    select(ungroup(filter(removed_confluence,
                                 # Deduplicate by largest trib path
                                 usLevelPathI == min(usLevelPathI))),
           -usLevelPathI) # clean

  flines <- reconcile_removed_flowlines(flines,
                                        reroute_confluence_set,
                                        removed_confluence,
                                        original_fline_atts)

  ####################################
  # Cleanup and prepare output
  ####################################
  # Catchments that joined downstream but are
  # referenced as a joined fromCOMID.
  # These need to be adjusted to be joined_toCOMID the same
  # as the one they reference in joined_fromCOMID.
  potential_bad_rows <-
    flines$COMID[which(!is.na(flines$joined_toCOMID) &
                         flines$joined_toCOMID != -9999)]
  bad_joined_fromcomid <-
    potential_bad_rows[potential_bad_rows %in%
                         flines$joined_fromCOMID]

  if (length(bad_joined_fromcomid > 0)) {
    # This is the row we will update
    bad_joined_fromcomid_index <- match(bad_joined_fromcomid,
                                        flines$joined_fromCOMID)

    # This is the row we will get our update from
    new_joined_tocomid_index <- match(bad_joined_fromcomid,
                                      flines$COMID)

    flines$joined_toCOMID[bad_joined_fromcomid_index] <-
      flines$joined_toCOMID[new_joined_tocomid_index]
    .
    flines$joined_fromCOMID[bad_joined_fromcomid_index] <- NA
  }

  # And the opposite direction
  potential_bad_rows <-
    flines$COMID[which(!is.na(flines$joined_fromCOMID) &
                         flines$joined_fromCOMID != -9999)]
  bad_joined_tocomid <-
    potential_bad_rows[potential_bad_rows %in%
                         flines$joined_toCOMID]

  if (length(bad_joined_tocomid > 0)) {
    # This is the row we will update
    bad_joined_tocomid_index <- match(bad_joined_tocomid,
                                      flines$joined_toCOMID)

    # This is the row we will get our update from
    new_joined_fromcomid_index <- match(bad_joined_tocomid, flines$COMID)

    flines$joined_fromCOMID[bad_joined_tocomid_index] <-
      flines$joined_fromCOMID[new_joined_fromcomid_index]

    flines$joined_toCOMID[bad_joined_tocomid_index] <- NA
  }

  if (add_category) {
    if (!"join_category" %in% names(flines)) {
      flines$join_category <- NA
    }
    flines <-
      mutate(flines,
       join_category = ifelse(COMID %in% short_outlets_tracker, "outlet",
        ifelse(COMID %in% removed_mainstem$removed_COMID, "mainstem",
          ifelse(COMID %in% removed_confluence$removed_COMID, "confluence",
            ifelse(COMID %in% headwaters_tracker, "headwater",
                   join_category)))))
  }

  # This takes care of na toCOMIDs that result from join_toCOMID pointers.
  flines <- left_join(flines,
                      select(flines,
                             COMID,
                             new_toCOMID = joined_toCOMID),
                      by = c("toCOMID" = "COMID"))

  flines <- mutate(flines, toCOMID = ifelse(!is.na(new_toCOMID),
                                            new_toCOMID, toCOMID))

  flines <- select(flines, -new_toCOMID)

  bad_rows_to <- function(flines) is.na(flines$toCOMID) &
    flines$LENGTHKM == 0 &
    flines$COMID %in% flines$joined_toCOMID &
    flines$joined_toCOMID != -9999

  bad_rows_from <- function(flines) is.na(flines$toCOMID) &
    flines$LENGTHKM == 0 &
    flines$COMID %in% flines$joined_fromCOMID &
    flines$joined_fromCOMID != -9999

  count <- 0

  # Should investigate why NAs are coming out of the splitter.
  while (any(bad_rows_to(flines), na.rm = TRUE) |
         any(bad_rows_from(flines), na.rm = TRUE)) {

    flines <- left_join(flines,
                        select(flines,
                               new_joined_toCOMID = joined_toCOMID,
                               new_joined_fromCOMID = joined_fromCOMID,
                               COMID), by = c("joined_toCOMID" = "COMID"))

    flines <-
      mutate(flines,
             joined_toCOMID =
               ifelse( (!is.na(new_joined_toCOMID) &
                          new_joined_toCOMID != -9999),
                       new_joined_toCOMID,
                       ifelse( (!is.na(new_joined_fromCOMID) &
                                  new_joined_fromCOMID != -9999),
                               new_joined_fromCOMID,
                               joined_toCOMID)))

    flines <- select(flines, -new_joined_toCOMID, -new_joined_fromCOMID)

    flines <- left_join(flines,
                        select(flines,
                               new_joined_toCOMID = joined_toCOMID,
                               new_joined_fromCOMID = joined_fromCOMID,
                               COMID), by = c("joined_fromCOMID" = "COMID"))

    flines <-
      mutate(flines,
             joined_fromCOMID =
               ifelse( (!is.na(new_joined_fromCOMID) &
                          new_joined_fromCOMID != -9999),
                       new_joined_fromCOMID,
                       ifelse( (!is.na(new_joined_toCOMID) &
                                  new_joined_toCOMID != -9999),
                               new_joined_toCOMID,
                               joined_fromCOMID)))

    flines <- select(flines, -new_joined_toCOMID, -new_joined_fromCOMID)

    count <- count + 1
    if (count > 20) {
      stop("stuck in final clean up loop")
    }
  }


  return(distinct(flines))
}

#' @title Collapse Outlets
#' @description Collapses outlet flowlines upstream.
#' Returns an flines dataframe.
#' @param flines data.frame with
#' COMID, toCOMID, LENGTHKM, and LevelPathI columns
#' @param thresh numeric a length threshold (km). Flowlines shorter than
#' this will be eliminated
#' @param original_fline_atts data.frame containing original fline attributes.
#' @param warn boolean controls whether warning an status messages are printed
#' @return flines data.frame with outlets collapsed to the given threshold.
#' @importFrom dplyr filter left_join select mutate group_by ungroup
#' @noRd
#'
collapse_outlets <- function(flines, thresh,
                             original_fline_atts,
                             exclude_cats = NULL,
                             warn = TRUE) {

  if ( ("joined_toCOMID" %in% names(flines) |
        "joined_fromCOMID" %in% names(flines))) {
    if (warn) {
      warning("collapse outllets must be used with un modified flines. \n",
              "returning unmodified flines from collapse outlets. \n")
    }
    return(list(flines = flines, short_outlets_tracker = c()))
  }

  flines$joined_fromCOMID <- NA

  short_outlets <- function(flines) {
    is.na(flines$toCOMID) & # No toCOMID,
      !flines$COMID %in% exclude_cats & # Not in exclude list
      flines$LENGTHKM < thresh & # too short,
      is.na(flines$joined_fromCOMID) # and hasn't been collapsed yet.
  }

  #######################################################

  count <- 0
  short_outlets_tracker <- c()

  while (any(short_outlets(flines))) {
    short_flines_next <- filter(flines, short_outlets(flines))

    if (!exists("short_flines")) {
      short_flines <- short_flines_next
    } else if (all(short_flines_next$COMID %in% short_flines$COMID)) {
      stop("Stuck in short flines loop")
    } else {
      short_flines <- short_flines_next
    }

    short_flines_index <- which(short_outlets(flines))

    headwaters <- !short_flines$COMID %in% original_fline_atts$toCOMID

    if (any(headwaters)) {
      headwater_COMIDs <- short_flines[["COMID"]][which(headwaters)]
      flines[["joined_fromCOMID"]][match(headwater_COMIDs,
                                         flines$COMID)] <- -9999
      short_flines <- short_flines[!headwaters, ]
      short_flines_index <- short_flines_index[!headwaters]
    }

    # Get from length and from area with a join on toCOMID.
    short_flines <- left_join(short_flines,
                              select(original_fline_atts, toCOMID,
                                     fromCOMID = COMID,
                                     fromLevelPathI = LevelPathI),
                              by = c("COMID" = "toCOMID")) %>%
      group_by(COMID) %>%  # deduplicate with level path then length.
      filter(is.na(joined_fromCOMID) | joined_fromCOMID != -9999) %>%
      filter(fromLevelPathI == min(fromLevelPathI)) %>%
      ungroup()

    # This is a pointer to the flines rows that need to
    # absorb a downstream short flowline.
    flines_to_update_index <- match(short_flines$fromCOMID, flines$COMID)

    # Adjust the length of the set we need to update.
    flines[["LENGTHKM"]][flines_to_update_index] <-
      flines[["LENGTHKM"]][flines_to_update_index] +
      flines[["LENGTHKM"]][short_flines_index]

    # Set LENGTHKM to 0 since this one's gone from the network.
    flines[["LENGTHKM"]][short_flines_index] <- 0
    flines[["toCOMID"]][flines_to_update_index] <- NA

    # Check if the eliminated COMID had anything going to it.
    need_to_update_index <-
      which(flines$joined_fromCOMID %in% short_flines$COMID)

    flines[["joined_fromCOMID"]][need_to_update_index] <-
      short_flines$fromCOMID

    # Mark the ones that are being removed with
    # which comid they got joined with.
    flines[["joined_fromCOMID"]][short_flines_index] <-
      short_flines$fromCOMID

    short_outlets_tracker <- c(short_outlets_tracker,
                               flines[["COMID"]][short_flines_index])

    count <- count + 1
    if (count > 100) stop("stuck in short outlet loop")
  }
  return(list(flines = flines, short_outlets_tracker = short_outlets_tracker))
}

#' @title Reconcile removed flowlines
#' @description Reconciles removed/missing flowlines from the network.
#' @param flines data.frame ...
#' @param reroute_set boolean ...
#' @param removed data.frame ...
#' @param original_fline_atts data.frame ...
#' @param normalize_mainstems boolean ...
#' @return reconciled flines
#' @importFrom dplyr filter left_join select rename mutate arrange distinct
#' @noRd
#'
reconcile_removed_flowlines <- function(flines, reroute_set, removed,
                                        original_fline_atts,
                                        normalize_mainstems = FALSE) {

  # Everything that is rerouted gets its length adjusted.
  # e.g. both tribs and main stem get lengthened the same.
  flines[["LENGTHKM"]][reroute_set] <-
    flines[["LENGTHKM"]][reroute_set] + flines[["dsLENGTHKM"]][reroute_set]

  # Adjust all the ToNodes to go to the next downstream.
  # This is ONLY working on the reroute set!!
  downstream_index <- match(flines[["toCOMID"]][reroute_set], flines$COMID)
  flines[["toCOMID"]][reroute_set] <- flines[["toCOMID"]][downstream_index]

  # stale_tocomid is true if the toCOMID is one of the removed comids.
  # We need to change those toCOMIDs to the toCOMID of the removed flowline
  # Need to keep doing it until no toCOMIDs are pointing to removed catchmetns
  stale_tocomid <- function() which(flines$toCOMID %in% removed$removed_COMID)

  count <- 0

  while (length(stale_tocomid()) > 0) {
    bad_tocomid <- flines[["toCOMID"]][stale_tocomid()]

    # downstream_index is a pointer to flowline that was
    # removed that we want the toCOMID of.
    downstream_index <- match(bad_tocomid, flines$COMID)

    if (any(flines[["toCOMID"]][stale_tocomid()] ==
            flines[["toCOMID"]][downstream_index], na.rm = TRUE)) {
      stop("found a loop while culling stale toCOMIDs!!!")
    }

    flines[["LENGTHKM"]][stale_tocomid()] <-
      flines[["LENGTHKM"]][stale_tocomid()] +
      flines[["LENGTHKM"]][downstream_index]

    # This is the bad_tocomid that we are making good.
    flines[["toCOMID"]][stale_tocomid()] <-
      flines[["toCOMID"]][downstream_index]

    count <- count + 1
    if (count > 100) {
      stop("stuck in stale toCOMID loop")
    }
  }

  already_removed <- filter(flines, (!is.na(joined_fromCOMID) &
                                       joined_fromCOMID != -9999)) %>%
    select(removed_COMID = COMID, joined_fromCOMID)

  # Need to sort removed_COMID by drainage area to get
  # the right "first matches" below.
  removed <- left_join(rbind(removed, already_removed),
                       select(original_fline_atts, COMID, Hydroseq),
                       by = c("removed_COMID" = "COMID")) %>%
    arrange(Hydroseq) %>% select(-Hydroseq) %>% distinct()

  # This is a pointer to joined_fromCOMID records
  # that point to already removed COMIDs.
  joined_from_to_replace <- function(removed) {
    which(removed$joined_fromCOMID %in% removed$removed_COMID)
  }

  count <- 0

  while (length(joined_from_to_replace(removed)) > 0) {
  # left join to its self in the joined_from direction. Copy over and repeat.
    removed <- left_join(removed,
                         rename(removed,
                                joined_fromCOMID_new = joined_fromCOMID),
                         by = c("joined_fromCOMID" = "removed_COMID")) %>%
      mutate(joined_fromCOMID = ifelse(!is.na(joined_fromCOMID_new),
                                       joined_fromCOMID_new,
                                       joined_fromCOMID)) %>%
      select(-joined_fromCOMID_new)

    count <- count + 1
    if (count > 100) stop("stuck in joined from to replace loop")
  }

  # Actually join the removed dataframe to the flines dataframe.
  flines <- left_join(flines, rename(removed,
                                     joined_fromCOMID_new = joined_fromCOMID),
                      by = c("COMID" = "removed_COMID")) %>%
    # get the outlet_joinedCOMID set into the joined_fromCOMID set too.
    mutate(joined_fromCOMID = ifelse(!is.na(joined_fromCOMID_new),
                                     joined_fromCOMID_new,
                                     joined_fromCOMID),
           LENGTHKM = ifelse( (!is.na(joined_fromCOMID) &
                                 joined_fromCOMID != -9999), 0, LENGTHKM),
           toCOMID = ifelse( (!is.na(joined_fromCOMID) &
                                joined_toCOMID != -9999), NA, toCOMID)) %>%
    select(-joined_fromCOMID_new, -dsLENGTHKM)

  return(flines)
}

#' @title Reconcile removed flowlines in the downstream direction
#' @description ... TBD ...
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, Hydroseq, and LevelPathI columns
#' @param remove_fun a function that will return boolean indicating which flines should be removed
#' @param remove_problem_headwaters whether to mark headwaters upstream of confluences as unable to remove
#' @return flines data.frame with short flowlines merged downstream
#' @noRd
#'
reconcile_downstream <- function(flines, remove_fun,
                                 remove_problem_headwaters = FALSE) {
  removed_tracker <- c()
  count <- 0
  rfl <- remove_fun(flines)
  while (any(rfl, na.rm = TRUE)) {

    flines$ds_num_upstream <- get_ds_num_upstream(flines)

    if (remove_problem_headwaters) {
      flines$ds_joined_fromCOMID <- get_ds_joined_fromCOMID(flines)

      # problem headwaters
      ph <- rfl & (flines$ds_num_upstream > 1 |
                     !is.na(flines$ds_joined_fromCOMID))

      # remove problems from remove
      rfl <- rfl & !ph
      ph_index <- which(ph)

      # Set broken ones to -9999 to indicate they can't be collapsed.
      flines[["joined_toCOMID"]][ph_index] <- -9999

      flines <- select(flines, -ds_joined_fromCOMID)
    }

    rfl_index <- which(rfl)

    # Set joined_toCOMID for remove set.
    flines[["joined_toCOMID"]][rfl_index] <- flines[["toCOMID"]][rfl_index]

    # Find index of next downstream to add length to.
    adjust_headwater_index <-
      match(flines[["toCOMID"]][rfl_index], flines$COMID)

    flines[["LENGTHKM"]][adjust_headwater_index] <-
      flines[["LENGTHKM"]][adjust_headwater_index] +
      flines[["LENGTHKM"]][rfl_index]

    # Set removed so they won't get used later.
    flines[["LENGTHKM"]][rfl_index] <- 0
    flines[["toCOMID"]][rfl_index] <- NA

    if (count > 0) {
      # look for instances of COMID that are about to
      # get skipped over -- fix them.
      flines <- left_join(flines,
                          filter(select(flines, COMID,
                                        new_joined_toCOMID = joined_toCOMID),
                                 !is.na(new_joined_toCOMID)),
                          by = c("joined_toCOMID" = "COMID"))

      flines <- mutate(flines,
                       joined_toCOMID =
                         ifelse( (!is.na(new_joined_toCOMID) &
                                    !new_joined_toCOMID == -9999),
                                 new_joined_toCOMID,
                                 joined_toCOMID))
      flines <- select(flines, -new_joined_toCOMID)
    }

    removed_tracker <- c(removed_tracker, flines[["COMID"]][rfl_index])

    count <- count + 1
    if (count > 100) {
      stop("stuck in headwaters while loop")
    }

    rfl <- remove_fun(flines)

  }
  return(list(flines = flines, removed_tracker = removed_tracker))
}
