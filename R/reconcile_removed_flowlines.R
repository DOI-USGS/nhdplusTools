#' @title Reconcile removed flowlines
#' @description Reconciles removed/missing flowlines from the network.
#' @param flines data.frame ...
#' @param reroute_set boolean ...
#' @param removed data.frame ...
#' @param original_fline_atts data.frame ...
#' @param normalize_mainstems boolean ...
#' @return reconciled flines
#' @importFrom dplyr filter left_join select rename mutate arrange distinct
#' @export
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
