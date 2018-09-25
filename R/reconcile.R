#' @title Reconcile Collapsed Flowlines
#' @description Reconciles output of collapse_flowlines giving a unique ID to
#'  each new unit and providing a mapping to NHDPlus COMIDs.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM,
#' and TotDASqKM columns
#' @param geom sf data.frame for flines
#' @param id character id collumn name.
#' @return reconciled flines
#' @importFrom dplyr group_by ungroup filter left_join select rename
#' mutate distinct summarise
#' @seealso The \code{\link{refactor_nhdplus}} function implements a complete
#' workflow using `reconcile_collapsed_flowlines()`.
#' @export
#'
reconcile_collapsed_flowlines <- function(flines, geom = NULL, id = "COMID") {

  check_names(names(flines), "reconcile_collapsed_flowlines")

  new_flines <-
    mutate(flines,
           becomes =
             ifelse( (is.na(joined_fromCOMID) | joined_fromCOMID == -9999),
                     ifelse( (is.na(joined_toCOMID) | joined_toCOMID == -9999),
                             COMID, joined_toCOMID),
                     joined_fromCOMID)) %>%
    group_by(becomes) %>%
    mutate(TotDASqKM = max(TotDASqKM), LENGTHKM = max(LENGTHKM)) %>%
    select(-joined_fromCOMID, -joined_toCOMID)

  new_flines <- ungroup(new_flines)

  new_flines <-
    left_join(new_flines,
              data.frame(becomes = unique(new_flines$becomes),
                         ID = seq_len(length(unique(new_flines$becomes))),
                         stringsAsFactors = FALSE),
              by = "becomes")

  tocomid_updater <- filter(select(new_flines, becomes, toCOMID),
                            !is.na(toCOMID))

  new_flines <- distinct(left_join(select(new_flines, -toCOMID),
                                   tocomid_updater, by = "becomes"))

  new_flines <- left_join(new_flines,
                          select(new_flines, becomes, toID = ID),
                          by = c("toCOMID" = "becomes"))

  new_flines <- distinct(new_flines) %>%
    select(ID, toID, LENGTHKM, TotDASqKM, member_COMID = COMID)

  if (!is.null(geom)) {
    geom_column <- attr(geom, "sf_column")

    if (is.null(geom_column)) stop("geom must contain an sf geometry column")

    new_flines <- left_join(new_flines, select(geom, id, geom_column),
                            by = c("member_COMID" = "COMID")) %>%
      sf::st_as_sf() %>%
      group_by(ID) %>%
      summarise(toID = toID[1],
                LENGTHKM = LENGTHKM[1],
                TotDASqKM = TotDASqKM[1],
                member_COMID = list(unique(member_COMID))) %>%
      sf::st_cast("MULTILINESTRING") %>%
      ungroup() %>%
      sf::st_line_merge()
  }
  return(new_flines)
}

#' @title Reconcile Catchments
#' @description Reconciles catchments according to the output of
#' \code{\link{reconcile_collapsed_flowlines}} and \code{\link{refactor_nhdplus}}
#' @param fline_ref sf data.frame flowlines as returned by
#' \code{\link{refactor_nhdplus}}
#' @param catchment sf data.frame NHDPlus Catchment or CatchmentSP for included COMIDs
#' @param fdr raster D8 flow direction
#' @param fac raster flow accumulation
#' @param para integer numer of cores to use for parallel execution
#' @return Catchments that have been split and collapsed according to input flowlines
#' @seealso The \code{\link{refactor_nhdplus}} function implements a complete
#' workflow using `reconcile_collapsed_flowlines()` and can be used in prep
#' for this function.
#' @details Note that all inputs must be passed in the same projection.
#' @export
#'
reconcile_catchments <- function(catchment, fline_ref, fdr, fac, para = 2) {

  check_proj(catchment, fline_ref, fdr)

  to_split_bool <- as.numeric(fline_ref$COMID) !=
    as.integer(fline_ref$COMID)

  to_split_ids <- fline_ref$COMID[which(to_split_bool)]

  to_split_featureids <- unique(as.integer(to_split_ids))

  out <- sf::st_sf(ID = integer(),
               member_FEATUREID  = character(),
               geom = sf::st_sfc(crs = st_crs(catchment)))

  par_split_cat <- function(fid, to_split_ids, fline_ref, catchment, fdr, fac) {
    # nolint start
    library(nhdplusTools)
    # nolint end
    split_set <- to_split_ids[which(grepl(as.character(fid), to_split_ids))]
    to_split_flines <- dplyr::filter(fline_ref, COMID %in% split_set)
    to_split_cat <- dplyr::filter(catchment, FEATUREID == fid)

    split_cats <- nhdplusTools::split_catchment(catchment = to_split_cat,
                                                fline = to_split_flines,
                                                fdr = fdr,
                                                fac = fac)

    split_cats <- sf::st_sfc(split_cats, crs = sf::st_crs(to_split_cat))

    sf::st_sf(FEATUREID = to_split_flines$COMID,
              geom = split_cats)
  }

  cl <- parallel::makeCluster(rep("localhost", para), type = "SOCK")

  split_cats <- parallel::parLapply(cl, to_split_featureids, par_split_cat,
                                    to_split_ids = to_split_ids,
                                    fline_ref = fline_ref,
                                    catchment = catchment,
                                    fdr = fdr, fac = fac)

  split_cats <- do.call(rbind, split_cats)

  parallel::stopCluster(cl)

  dplyr::filter(catchment,
                       !catchment$FEATUREID %in% to_split_featureids) %>%
    dplyr::select(FEATUREID, geom) %>%
    dplyr::mutate(FEATUREID = as.character(FEATUREID)) %>%
    dplyr::left_join(dplyr::select(sf::st_set_geometry(fline_ref, NULL),
                                   FEATUREID = COMID),
                     by = "FEATUREID") %>%
    dplyr::select(FEATUREID, geom) %>%
    rbind(split_cats)
}
