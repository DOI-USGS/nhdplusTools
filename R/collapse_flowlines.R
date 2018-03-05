#' @title Collapse NHDPlus Network
#' @description Refactors the NHDPlus flowline network, eliminating short and non-cconfluence flowlines.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM columns
#' @param thresh numeric a length threshold (km). Flowlines shorter than this will be eliminated
#' @param add_category boolean if combination category is desired in output, set to TRUE
#' @param mainstem_thresh numeric threshold for combining inter-confluence mainstems
#' @return A refactored network with merged up and down flowlines.
#' @importFrom dplyr filter left_join select mutate group_by ungroup summarise
#' @export
#'
collapse_flowlines <- function(flines, thresh, add_category = FALSE, mainstem_thresh = NULL) {

  if(is.null(mainstem_thresh)) { # very large thresh
    mainstem_thresh_use <- max(flines$LENGTHKM)
  } else {
    mainstem_thresh_use <- mainstem_thresh
  }

  original_fline_atts <- flines

  #######################################################
  # Need to clean up the short outlets first so they don't get broken by the method below.

  flines <- collapse_outlets(flines, thresh, add_category, original_fline_atts)

  short_outlets_tracker <- flines[["short_outlets_tracker"]]
  flines <- flines[["flines"]]

  #######################################################

  flines$joined_toCOMID <- NA

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
  #######################################################

  # if(!is.null(mainstem_thresh)) {
  #   flines$num_upstream <- get_num_upstream(flines)
  #   flines$ds_num_upstream <- get_ds_num_upstream(flines)
  #
  #   remove_mainstem_top_index <- which((flines$num_upstream > 1 & flines$ds_num_upstream ==1) & # At the top of a mainstem
  #                                        flines$LENGTHKM < mainstem_thresh_use & # shorter than threshold
  #                                        !is.na(flines$toCOMID)) # is still in scope
  #
  #   flines[["joined_toCOMID"]][remove_mainstem_top_index] <- flines[["toCOMID"]][remove_mainstem_top_index]
  #
  #   adjust_mainstem_top_index <- match(flines[["toCOMID"]][remove_mainstem_top_index], flines$COMID)
  #
  #   flines[["LENGTHKM"]][adjust_mainstem_top_index] <-
  #     flines[["LENGTHKM"]][adjust_mainstem_top_index] + flines[["LENGTHKM"]][remove_mainstem_top_index]
  #
  #   flines[["toCOMID"]][remove_mainstem_top_index] <- NA
  #
  #   flines <- select(flines, -num_upstream)
  # }
  #######################################################
  # Combine along main stems with no confluences.
  flines <- mutate(flines, dsLENGTHKM = get_dsLENGTHKM(flines),
                   ds_num_upstream = get_ds_num_upstream(flines))

  # this is the set that will get routed over short main stem flowlines.
  # it should not include flowlines just upstream of a confluence.
  reroute_mainstem_set <- function(flines) {
    (flines$dsLENGTHKM > 0 & # is still in scope
       is.na(flines$joined_toCOMID) &
       flines$ds_num_upstream == 1 & # is not upstream of a confluence
       flines$dsLENGTHKM < mainstem_thresh_use) # is shorter than the mainstem threshold
  }

  # This is the set that is going to get skipped in the rerouting.
  removed_mainstem <- data.frame(removed_COMID = flines[["toCOMID"]][reroute_mainstem_set(flines)],
                        joined_fromCOMID = flines[["COMID"]][reroute_mainstem_set(flines)])

  flines <- reconcile_removed_flowlines(flines, reroute_mainstem_set(flines), removed_mainstem, original_fline_atts)

  # Combine accross confluences next slightly different logic from mainstems

  # This is the set that is going to get rerouted.
  # Note that dsLengthKM > 0 removes all terminal and already removed flowlines from consideration.
  reroute_confluence_set <- (flines$dsLENGTHKM < thresh &
                               flines$dsLENGTHKM > 0 &
                               flines$ds_num_upstream > 1)

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

  flines <- reconcile_removed_flowlines(select(flines, -ds_num_upstream), reroute_confluence_set, removed_confluence, original_fline_atts)

  flines <- mutate(flines, toCOMID = ifelse(!is.na(joined_fromCOMID) | !is.na(joined_toCOMID),
                                            NA,
                                            toCOMID)) %>%
    select(-dsLENGTHKM)

  if(add_category) {
    flines <- mutate(flines,
                     join_category = ifelse(COMID %in% short_outlets_tracker, "outlet",
                                            ifelse(COMID %in% removed_mainstem$removed_COMID, "mainstem",
                                                   ifelse(COMID %in% removed_confluence$removed_COMID, "confluence",
                                                          ifelse(COMID %in% headwaters_tracker, "headwater", NA)))))
  }

  return(flines)
}
