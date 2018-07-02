#' @title Collapse Outlets
#' @description Collapses outlet flowlines upstream. Returns an flines dataframe.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM columns
#' @param thresh numeric a length threshold (km). Flowlines shorter than this will be eliminated
#' @param original_fline_atts data.frame containing original fline attributes.
#' @return flines data.frame with outlets collapsed to the given threshold.
#' @importFrom dplyr filter left_join select mutate group_by ungroup
#' @export
#'
collapse_outlets <- function(flines, thresh, original_fline_atts) {

  if ("joined_toCOMID" %in% names(flines) | "joined_fromCOMID" %in% names(flines)) {
    warning("collapse outllets must be used with un modified flines. \n",
            "returning unmodified flines from collapse outlets. \n")
    return(list(flines = flines, short_outlets_tracker = c()))
  }

  flines$joined_fromCOMID <- NA

  short_outlets <- function(flines) {
    is.na(flines$toCOMID) & # No toCOMID,
      flines$LENGTHKM < thresh & # too short,
      is.na(flines$joined_fromCOMID) # and hasn't been combined yet.
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
      flines[["joined_fromCOMID"]][match(headwater_COMIDs, flines$COMID)] <- -9999
      short_flines <- short_flines[!headwaters, ]
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
      filter(is.na(joined_fromCOMID) | joined_fromCOMID != -9999) %>%
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
    flines[["toCOMID"]][flines_to_update_index] <- NA

    # Check if the eliminated COMID had anything going to it.
    need_to_update_index <- which(flines$joined_fromCOMID %in% short_flines$COMID)
    flines[["joined_fromCOMID"]][need_to_update_index] <- short_flines$fromCOMID

    # Mark the ones that are being removed with which comid they got joined with.
    flines[["joined_fromCOMID"]][short_flines_index] <- short_flines$fromCOMID

    short_outlets_tracker <- c(short_outlets_tracker, flines[["COMID"]][short_flines_index])

    count <- count + 1
    if (count > 100) stop("stuck in short outlet loop")
  }
  return(list(flines = flines, short_outlets_tracker = short_outlets_tracker))
}
