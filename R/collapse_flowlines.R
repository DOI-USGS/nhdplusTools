#' @title Collapse NHDPlus Network
#' @description Refactors the NHDPlus flowline network, eliminating short and non-cconfluence flowlines.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM columns
#' @param thresh numeric a length threshold (km). Flowlines shorter than this will be eliminated
#' @return A refactored network with merged up and down flowlines.
#' @importFrom dplyr filter left_join select mutate group_by ungroup summarise
#' @export
#'
collapse_flowlines <- function(flines, thresh) {

  original_fline_atts <- flines

  # Need to clean up the short outlets first so they don't get broken by the method below.
  # Could move this to a stand alone function.
  flines$joined_fromCOMID <- NA

  short_outlets <- function(flines) {
    is.na(flines$toCOMID) & # No toCOMID,
      flines$LENGTHKM < thresh & # too short,
      is.na(flines$joined_fromCOMID) # and hasn't been combined yet.
  }

  count <- 0

  while(any(short_outlets(flines))) {
    short_flines_next <- filter(flines, short_outlets(flines))

    if(!exists("short_flines")) {
      short_flines <- short_flines_next
    } else if(all(short_flines_next$COMID %in% short_flines$COMID)) {
      stop("Stuck in short flines loop")
    } else {
      short_flines <- short_flines_next
    }

    short_flines_index <- which(short_outlets(flines))

    headwaters <- !short_flines$COMID %in% original_fline_atts$toCOMID

    if(any(headwaters)) {
      headwater_COMIDs <- short_flines[["COMID"]][which(headwaters)]
      flines[["joined_fromCOMID"]][match(headwater_COMIDs, flines$COMID)] <- -9999
      short_flines <- short_flines[!headwaters,]
      short_flines_index <- short_flines_index[!headwaters]
    }

    # Get from length and from area with a join on toCOMID.
    short_flines <- left_join(short_flines,
                                      select(original_fline_atts, toCOMID,
                                             fromCOMID = COMID,
                                             fromLENGTHKM = LENGTHKM,
                                             fromTotDASqKM = TotDASqKM),
                                      by = c("COMID" = "toCOMID")) %>%
      group_by(COMID) %>%  # deduplicate with area then length.
      filter(fromTotDASqKM == max(fromTotDASqKM)) %>%
      filter(fromLENGTHKM == max(fromLENGTHKM)) %>%
      filter(fromCOMID == max(fromCOMID)) %>% # This is dumb, but happens if DA and Length are equal...
      ungroup()
    # This is a pointer to the flines rows that need to absorb a downstream short flowline.
    flines_to_update_index <- match(short_flines$fromCOMID, flines$COMID)

    flines[["LENGTHKM"]][flines_to_update_index] <- # Adjust the length of the set we need to update.
      flines[["LENGTHKM"]][flines_to_update_index] + flines[["LENGTHKM"]][short_flines_index]
    # Set LENGTHKM to 0 since this one's gone from the network.
    flines[["LENGTHKM"]][short_flines_index] <- 0

    # Check if the eliminated COMID had anything going to it.
    need_to_update_index <- which(flines$joined_fromCOMID %in% short_flines$COMID)
    flines[["joined_fromCOMID"]][need_to_update_index] <- short_flines$fromCOMID

    # Mark the ones that are being removed with which comid they got joined with.
    flines[["joined_fromCOMID"]][short_flines_index] <- short_flines$fromCOMID

    flines[["toCOMID"]][flines_to_update_index] <- NA

    count <- count + 1
    if(count > 100) stop("stuck in short outlet loop")
  }

  # Combine along main stems with no confluences.
  flines <- mutate(flines, dsLENGTHKM = get_dsLENGTHKM(flines),
                   ds_num_upstream = get_ds_num_upstream(flines))

  # this is the set that will get routed over short main stem flowlines.
  # it should not include flowlines just upstream of a confluence.
  reroute_mainstem_set <- function(flines) {
    (flines$dsLENGTHKM > 0 & # is still in scope
       flines$ds_num_upstream == 1) # is not upstream of a confluence
  }

  # This is the set that is going to get skipped in the rerouting.
  removed_mainstem <- data.frame(removed_COMID = flines[["toCOMID"]][reroute_mainstem_set(flines)],
                        joined_fromCOMID = flines[["COMID"]][reroute_mainstem_set(flines)])

  flines <- reconcile_removed_flowlines(select(flines, -ds_num_upstream), reroute_mainstem_set(flines), removed_mainstem, original_fline_atts)

  # Combine accross confluences next slightly different logic from mainstems

  # This is the set that is going to get rerouted.
  # Note that dsLengthKM > 0 removes all terminal and already removed flowlines from consideration.
  reroute_confluence_set <- (flines$dsLENGTHKM < thresh & flines$dsLENGTHKM > 0)

  # This is the set that is going to get skipped in the rerouting.
  removed_confluence <- data.frame(removed_COMID = flines[["toCOMID"]][reroute_confluence_set],
                                   joined_fromCOMID = flines[["COMID"]][reroute_confluence_set]) %>%
    left_join(select(original_fline_atts, COMID,
                     usTotDASqKM = TotDASqKM, # Get upstream area and length
                     usLENGTHKM = LENGTHKM), # by joining fromCOMID to COMID
              by = c("joined_fromCOMID" = "COMID")) %>%
    group_by(removed_COMID) %>% # Deduplicate by area first then length.
    filter(usTotDASqKM == max(usTotDASqKM)) %>%
    filter(usLENGTHKM == max(usLENGTHKM)) %>% # Clean up.
    ungroup() %>% select(-usLENGTHKM, -usTotDASqKM) %>% data.frame()

  flines <- reconcile_removed_flowlines(flines, reroute_confluence_set, removed_confluence, original_fline_atts)

  # Clean up short headwaters that aren't handled by the next downstream logic above.

  remove_headwaters_index <- which(!(flines$COMID %in% flines$toCOMID) & # a headwater (nothing flows to it)
                                     flines$LENGTHKM < thresh & # shorter than threshold
                                     is.na(flines$joined_fromCOMID)) # hasn't already been removed
  flines$joined_toCOMID <- NA

  flines[["joined_toCOMID"]][remove_headwaters_index] <- flines[["toCOMID"]][remove_headwaters_index]

  adjust_headwater_index <- match(flines[["toCOMID"]][remove_headwaters_index], flines$COMID)

  flines[["LENGTHKM"]][adjust_headwater_index] <-
    flines[["LENGTHKM"]][adjust_headwater_index] + flines[["LENGTHKM"]][remove_headwaters_index]

  flines <- mutate(flines, toCOMID = ifelse(is.na(joined_fromCOMID),
                                            toCOMID,
                                            NA)) %>%
    select(-dsLENGTHKM)

  return(flines)
}
