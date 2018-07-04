#' @title Reconcile collapsed flowlines
#' @description Reconciles output of collapse_flowlines giving a unique ID to each new unit and
#' providing a mapping to NHDPlus COMIDs.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM columns
#' @param geom sf data.frame for flines
#' @param id character id collumn name.
#' @return reconciled flines
#' @importFrom dplyr group_by ungroup filter left_join select rename mutate distinct summarise
#' @noRd
#'
reconcile_collapsed_flowlines <- function(flines, geom = NULL, id = "COMID") {

  new_flines <- mutate(flines, becomes = ifelse( (is.na(joined_fromCOMID) | joined_fromCOMID == -9999),
                                                 ifelse( (is.na(joined_toCOMID) | joined_toCOMID == -9999),
                                                         COMID, joined_toCOMID),
                                                 joined_fromCOMID)) %>%
    group_by(becomes) %>%
    mutate(TotDASqKM = max(TotDASqKM), LENGTHKM = max(LENGTHKM)) %>%
    select(-joined_fromCOMID, -joined_toCOMID)

  new_flines <- ungroup(new_flines)

  new_flines <- left_join(new_flines,
                          data.frame(becomes = unique(new_flines$becomes),
                                     ID = 1:length(unique(new_flines$becomes)),
                                     stringsAsFactors = FALSE),
                          by = "becomes")

  tocomid_updater <- filter(select(new_flines, becomes, toCOMID), !is.na(toCOMID))

  new_flines <- distinct(left_join(select(new_flines, -toCOMID), tocomid_updater, by = "becomes"))

  new_flines <- left_join(new_flines,
                          select(new_flines, becomes, toID = ID),
                          by = c("toCOMID" = "becomes"))

  new_flines <- distinct(new_flines) %>%
    select(ID, toID, LENGTHKM, TotDASqKM, member_COMID = COMID)

  if (!is.null(geom)) {
    geom_column <- attr(geom, "sf_column")

    if (is.null(geom_column)) stop("geom must contain an sf geometry column")

    new_flines <- left_join(new_flines, select(geom, id, geom_column), by = c("member_COMID" = "COMID")) %>%
      sf::st_as_sf() %>%
      group_by(ID) %>%
      summarise(toID = toID[1], LENGTHKM = LENGTHKM[1], TotDASqKM = TotDASqKM[1],
                member_COMID = list(unique(member_COMID))) %>%
      sf::st_cast("MULTILINESTRING") %>%
      ungroup() %>%
      sf::st_line_merge()
  }
  return(new_flines)
}

#' @title Reconcile removed flowlines in the downstream direction
#' @description ... TBD ...
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM columns
#' @param remove_fun a function that will return boolean indicating which flines should be removed
#' @param remove_problem_headwaters whether to mark headwaters upstream of confluences as unable to remove
#' @return flines data.frame with short flowlines merged downstream
#' @noRd
#'
reconcile_downstream <- function(flines, remove_fun, remove_problem_headwaters = FALSE) {
  removed_tracker <- c()
  count <- 0
  rfl <- remove_fun(flines)
  while (any(rfl, na.rm = TRUE)) {

    flines$ds_num_upstream <- get_ds_num_upstream(flines)

    if (remove_problem_headwaters) {
      flines$ds_joined_fromCOMID <- get_ds_joined_fromCOMID(flines)

      # problem headwaters
      ph <- rfl & (flines$ds_num_upstream > 1 | !is.na(flines$ds_joined_fromCOMID))

      # remove problems from remove
      rfl <- rfl & !ph
      ph_index <- which(ph)

      # Set broken ones to -9999 to indicate they can't be combined.
      flines[["joined_toCOMID"]][ph_index] <- -9999

      flines <- select(flines, -ds_joined_fromCOMID)
    }

    rfl_index <- which(rfl)

    # Set joined_toCOMID for remove set.
    flines[["joined_toCOMID"]][rfl_index] <- flines[["toCOMID"]][rfl_index]

    # Find index of next downstream to add length to.
    adjust_headwater_index <- match(flines[["toCOMID"]][rfl_index], flines$COMID)

    flines[["LENGTHKM"]][adjust_headwater_index] <-
      flines[["LENGTHKM"]][adjust_headwater_index] + flines[["LENGTHKM"]][rfl_index]

    # Set removed so they won't get used later.
    flines[["LENGTHKM"]][rfl_index] <- 0
    flines[["toCOMID"]][rfl_index] <- NA

    if (count > 0) {
      # look for instances of COMID that are about to get skipped over -- fix them.
      flines <- left_join(flines,
                          filter(select(flines, COMID,
                                        new_joined_toCOMID = joined_toCOMID),
                                 !is.na(new_joined_toCOMID)),
                          by = c("joined_toCOMID" = "COMID"))

      flines <- mutate(flines, joined_toCOMID = ifelse( (!is.na(new_joined_toCOMID) &
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
                                        original_fline_atts, normalize_mainstems = FALSE) {

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

    # downstream_index is a pointer to flowline that was removed that we want the toCOMID of.
    downstream_index <- match(bad_tocomid, flines$COMID)

    if (any(flines[["toCOMID"]][stale_tocomid()] == flines[["toCOMID"]][downstream_index], na.rm = TRUE)) {
      stop("found a loop while culling stale toCOMIDs!!!")
    }

    flines[["LENGTHKM"]][stale_tocomid()] <-
      flines[["LENGTHKM"]][stale_tocomid()] + flines[["LENGTHKM"]][downstream_index]

    # This is the bad_tocomid that we are making good.
    flines[["toCOMID"]][stale_tocomid()] <- flines[["toCOMID"]][downstream_index]

    count <- count + 1
    if (count > 100) {
      stop("stuck in stale toCOMID loop")
    }
  }

  already_removed <- filter(flines, (!is.na(joined_fromCOMID) & joined_fromCOMID != -9999)) %>%
    select(removed_COMID = COMID, joined_fromCOMID)

  # Need to sort removed_COMID by drainage area to get the right "first matches" below.
  removed <- left_join(rbind(removed, already_removed), select(original_fline_atts, COMID, TotDASqKM),
                       by = c("removed_COMID" = "COMID")) %>%
    arrange(desc(TotDASqKM)) %>% select(-TotDASqKM) %>% distinct()

  # This is a pointer to joined_fromCOMID records that point to already removed COMIDs.
  joined_from_to_replace <- function(removed) which(removed$joined_fromCOMID %in% removed$removed_COMID)

  count <- 0

  while (length(joined_from_to_replace(removed)) > 0) {
    # left join to its self in the joined_from direction. Copy over and repeat.
    removed <- left_join(removed,
                         rename(removed, joined_fromCOMID_new = joined_fromCOMID),
                         by = c("joined_fromCOMID" = "removed_COMID")) %>%
      mutate(joined_fromCOMID = ifelse(!is.na(joined_fromCOMID_new),
                                       joined_fromCOMID_new, joined_fromCOMID)) %>%
      select(-joined_fromCOMID_new)

    count <- count + 1
    if (count > 100) stop("stuck in joined from to replace loop")
  }

  # Actually join the removed dataframe to the flines dataframe.
  flines <- left_join(flines, rename(removed, joined_fromCOMID_new = joined_fromCOMID),
                      by = c("COMID" = "removed_COMID")) %>%
    # get the outlet_joinedCOMID set into the joined_fromCOMID set too.
    mutate(joined_fromCOMID = ifelse(!is.na(joined_fromCOMID_new),
                                     joined_fromCOMID_new,
                                     joined_fromCOMID),
           LENGTHKM = ifelse( (!is.na(joined_fromCOMID) & joined_fromCOMID != -9999), 0, LENGTHKM),
           toCOMID = ifelse( (!is.na(joined_fromCOMID) & joined_toCOMID != -9999), NA, toCOMID)) %>%
    select(-joined_fromCOMID_new, -dsLENGTHKM)

  return(flines)
}
