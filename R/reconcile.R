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
#' \code{\link{refactor_nhdplus}} and \code{\link{reconcile_collapsed_flowlines}}
#' @param fline_rec sf data.frame flowlines as returned by
#' \code{\link{reconcile_collapsed_flowlines}} and
#' \code{\link{reconcile_collapsed_flowlines}}
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
reconcile_catchments <- function(catchment, fline_ref, fline_rec, fdr, fac, para = 2) {

  check_proj(catchment, fline_ref, fdr)

  reconciled <- sf::st_set_geometry(fline_rec, NULL) %>%
    dplyr::select(ID, member_COMID)

  comid <- fline_ref$COMID
  featureid <- catchment$FEATUREID
  comid_with_catchment <- comid[as.integer(comid) %in% featureid]

  # Not all catchments have flowlines. Remove the flowlines without.
  remove <- c()
  for (rec_cat in seq_along(reconciled$member_COMID)) {
    cats_vec <- unlist(strsplit(reconciled$member_COMID[rec_cat], ","))
    cats_vec <- cats_vec[cats_vec %in% comid_with_catchment]
    if (length(cats_vec) == 0) {
      remove <- c(remove, rec_cat)
    } else {
      reconciled$member_COMID[rec_cat] <- paste(cats_vec, collapse = ",")
    }
  }

  if (length(remove) > 0) reconciled <- reconciled[-c(remove), ]

  fline_ref <- fline_ref[as.integer(fline_ref$COMID) %in% catchment$FEATUREID, ]

  to_split_bool <- as.numeric(fline_ref$COMID) !=
    as.integer(fline_ref$COMID)

  to_split_ids <- fline_ref$COMID[which(to_split_bool)]

  to_split_featureids <- unique(as.integer(to_split_ids))

  par_split_cat <- function(fid, to_split_ids, fline_ref, catchment, fdr, fac) {
    out <- NULL
    try({
      # nolint start
      library(nhdplusTools)
      # nolint end
      split_set <- to_split_ids[which(grepl(as.character(fid), to_split_ids))]
      to_split_flines <- dplyr::filter(fline_ref, COMID %in% split_set)
      to_split_cat <- dplyr::filter(catchment, FEATUREID == fid)

      split_cats <- split_catchment(catchment = to_split_cat,
                                                  fline = to_split_flines,
                                                  fdr = fdr,
                                                  fac = fac)

      out <- sf::st_sf(FEATUREID = to_split_flines$COMID,
                       geom = split_cats, stringsAsFactors = FALSE)
    }
    , silent = FALSE, outFile = "split.log")
    return(out)
  }

  cl <- parallel::makeCluster(rep("localhost", para), type = "SOCK")

  split_cats <- parallel::parLapply(cl, to_split_featureids, par_split_cat,
                                    to_split_ids = to_split_ids,
                                    fline_ref = fline_ref,
                                    catchment = catchment,
                                    fdr = fdr, fac = fac)

  parallel::stopCluster(cl)

  split_cats <- do.call(rbind, split_cats[!sapply(split_cats, is.null)])

  split_cats <- filter(catchment, !catchment$FEATUREID %in% to_split_featureids) %>%
    dplyr::select(FEATUREID, geom) %>%
    mutate(FEATUREID = as.character(FEATUREID)) %>%
    left_join(dplyr::select(sf::st_set_geometry(fline_ref, NULL), FEATUREID = COMID),
              by = "FEATUREID") %>%
    dplyr::select(FEATUREID, geom) %>%
    rbind(split_cats)

  combinations <- reconciled$member_COMID[which(grepl(",", reconciled$member_COMID))]

  for (cats in combinations) {
    cats_vec <- unlist(strsplit(cats, ","))

    union_cats <- filter(split_cats, FEATUREID %in% cats_vec)

    if (nrow(union_cats) != length(cats_vec)) {
      stop("missing a split catchment for an expected split flowline.")
    }

    unioned <- sf::st_sf(FEATUREID = cats, geom = sf::st_cast(sf::st_union(union_cats), "MULTIPOLYGON"),
                          stringsAsFactors = FALSE)

    if (!any(grepl("MULTIPOLYGON", class(st_geometry(unioned))))) {
      warning("Somthing is wrong with the union of catchments.")
    }

    split_cats <- filter(split_cats, !FEATUREID %in% cats_vec) %>%
      rbind(unioned)
  }

  out <- st_sf(left_join(reconciled, split_cats,
                         by = c("member_COMID" = "FEATUREID")))

  missing <- is.na(sf::st_dimension(out$geom))

  if (any(missing)) {
    replace_cat <- dplyr::select(catchment, member_COMID = FEATUREID) %>%
      dplyr::filter(member_COMID %in% unique(as.integer(out$member_COMID[missing]))) %>%
      mutate(member_COMID = paste0(member_COMID, ".1")) %>%
      mutate(ID = out$ID[match(member_COMID, out$member_COMID)]) %>%
      dplyr::select(ID, member_COMID)

    out <- filter(out, !missing) %>%
      st_cast("MULTIPOLYGON")

    names(replace_cat)[which(names(replace_cat) ==
                               attr(replace_cat,
                                    "sf_column"))] <- attr(out,
                                                           "sf_column")
    attr(replace_cat, "sf_column") <- attr(out, "sf_column")

    return(rbind(out, replace_cat))
  } else {
    return(out)
  }
}
