#' @title Reconcile removed flowlines in the downstream direction
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM columns
#' @param remove_fun a function that will return boolean indicating which flines should be removed
#' @param remove_problem_headwaters whether to mark headwaters upstream of confluences as unable to remove
#' @return flines data.frame with short flowlines merged downstream
#' @export
#'
reconcile_downstream <- function(flines, remove_fun, remove_problem_headwaters = FALSE) {
  removed_tracker <- c()
  count <- 0

  while(any(remove_fun(flines))) {

    flines$ds_num_upstream <- get_ds_num_upstream(flines)

    rfl <- remove_fun(flines)

    if(remove_problem_headwaters) {
      # problem headwaters
      ph <- rfl & flines$ds_num_upstream > 1

      # remove problems from remove
      rfl <- rfl & !ph
      ph_index <- which(ph)

      # Set broken ones to -9999 to indicate they can't be combined.
      flines[["joined_toCOMID"]][ph_index] <- -9999
    }

    rfl_index <- which(rfl)

    if(count > 0) {
      # look for instances of COMID that are about to get skipped over fix them.
      adjust_toCOMID_index <- match(flines[["COMID"]][rfl_index], flines[["joined_toCOMID"]])
      flines[["joined_toCOMID"]][adjust_toCOMID_index] <- flines[["toCOMID"]][rfl_index]
    }

    # Set joined_toCOMID for remove set.
    flines[["joined_toCOMID"]][rfl_index] <- flines[["toCOMID"]][rfl_index]

    # Find index of next downstream to add length to.
    adjust_headwater_index <- match(flines[["toCOMID"]][rfl_index], flines$COMID)

    flines[["LENGTHKM"]][adjust_headwater_index] <-
      flines[["LENGTHKM"]][adjust_headwater_index] + flines[["LENGTHKM"]][rfl_index]

    # Set removed so they won't get used later.
    flines[["LENGTHKM"]][rfl_index] <- 0
    flines[["toCOMID"]][rfl_index] <- NA

    removed_tracker <- c(removed_tracker, flines[["COMID"]][rfl_index])

    count <- count + 1
    if(count > 100) {
      stop("stuck in headwaters while loop")
    }
  }
  return(list(flines = flines, removed_tracker = removed_tracker))
}
