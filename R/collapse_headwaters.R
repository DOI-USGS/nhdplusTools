#' @title Collapse Short Headwaters
#' @description Refactors the NHDPlus flowline network, eliminating short and non-cconfluence flowlines.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM columns
#' @param thresh numeric a length threshold (km). Flowlines shorter than this will be eliminated
#' @return flines data.frame with short headwaters merged downsrteam
#' @importFrom dplyr filter left_join select mutate group_by ungroup summarise
#' @export
#'
collapse_headwaters <- function(flines, thresh, original_fline_atts) {

  if(!"joined_toCOMID" %in% names(flines)) flines$joined_toCOMID <- NA
  if(!"joined_fromCOMID" %in% names(flines)) flines$joined_fromCOMID <- NA

  # Clean up short headwaters that aren't handled by the next downstream logic above.
  remove_headwaters <- function(flines) {
    !(flines$COMID %in% flines$toCOMID) & # a headwater (nothing flows to it)
      flines$LENGTHKM < thresh & # shorter than threshold
      is.na(flines$joined_fromCOMID) &
      is.na(flines$joined_toCOMID)
  }

  headwaters_tracker <- c()
  count <- 0

  while(any(remove_headwaters(flines))) {

    flines$ds_num_upstream <- get_ds_num_upstream(flines)

    # remove headwaters
    rh <- remove_headwaters(flines)

    # problem headwaters
    ph <- rh & flines$ds_num_upstream > 1

    # remove problems from remove
    rh <- rh & !ph

    # create indexes
    rh_index <- which(rh)
    ph_index <- which(ph)

    # Set broken ones to -9999 to indicate they can't be combined.
    flines[["joined_toCOMID"]][ph_index] <- -9999

    if(count > 0) {
      # look for instances of COMID that are about to get skipped over fix them.
      adjust_toCOMID_index <- match(flines[["COMID"]][rh_index], flines[["joined_toCOMID"]])
      flines[["joined_toCOMID"]][adjust_toCOMID_index] <- flines[["toCOMID"]][rh_index]
    }

    # Set joined_toCOMID for remove set.
    flines[["joined_toCOMID"]][rh_index] <- flines[["toCOMID"]][rh_index]

    # Find index of next downstream to add length to.
    adjust_headwater_index <- match(flines[["toCOMID"]][rh_index], flines$COMID)

    flines[["LENGTHKM"]][adjust_headwater_index] <-
      flines[["LENGTHKM"]][adjust_headwater_index] + flines[["LENGTHKM"]][rh_index]

    # Set removed so they won't get used later.
    flines[["LENGTHKM"]][rh_index] <- 0
    flines[["toCOMID"]][rh_index] <- NA

    headwaters_tracker <- c(headwaters_tracker, flines[["COMID"]][rh_index])

    count <- count + 1
    if(count > 100) {
      stop("stuck in headwaters while loop")
    }
  }
  return(list(flines = flines, headwaters_tracker = headwaters_tracker))
}
