#' @title Reconcile collapsed flowlines
#' @description Reconciles output of collapse_flowlines giving a unique ID to each new unit and
#' providing a mapping to NHDPlus COMIDs.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, and TotDASqKM columns
#' @param geom sf data.frame for flines
#' @param id character id collumn name.
#' @return reconciled flines
#' @importFrom dplyr group_by ungroup filter left_join select rename mutate distinct summarise
#' @export
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
                                     ID = 1:length(unique(new_flines$becomes)), stringsAsFactors = FALSE),
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
