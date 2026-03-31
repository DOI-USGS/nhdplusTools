#' @title Estimate drainage area from HUC and catchment data
#' @description Combines HUC12 areas upstream of HUC outlets with NHDPlusV2
#'   catchment areas for the portion of the basin between the outlet and HUC12
#'   outlets to produce a drainage area estimate. Non-contributing areas
#'   captured in HUC12 boundaries are included, making this more representative
#'   than purely network-derived totals.
#' @details
#'   HUC drainage area is used upstream of the nearest HUC12 outlet. Between
#'   the outlet (e.g. gage) and the HUC outlet(s), catchment areas are used.
#'   For large upstream areas the largest HUC level is used to define
#'   connectedness, but drainage estimates are derived from HUC12 because
#'   that is where the non-contributing area attribute lives.
#'
#'   Three pairs of drainage area estimates are returned: one using only
#'   NLDI-identified HUC12s (HUC12-level), one using HUC10-level queries,
#'   and one using HUC08-level queries for basins spanning multiple HUC08s.
#'   Each pair includes a total and a contributing-only estimate derived from
#'   the \code{ncontrb_a} (non-contributing acres) attribute on HUC12 features.
#'
#' @param start list with \code{featureSource} and \code{featureID} compatible
#'   with \code{\link{get_nldi_feature}}.
#' @param vaa data.frame of NHDPlusV2 Value Added Attributes as returned by
#'   \code{\link{get_vaa}}. Must include columns: comid, areasqkm, totdasqkm.
#' @return list with elements:
#'   \describe{
#'     \item{da_huc12_sqkm}{numeric. Total DA using NLDI-identified HUC12s only.}
#'     \item{da_huc10_sqkm}{numeric or NA. Total DA using HUC10-level queries.
#'       NA when basin is within a single HUC10.}
#'     \item{da_huc08_sqkm}{numeric or NA. Total DA using HUC08-level queries.
#'       NA when basin is within a single HUC08.}
#'     \item{contrib_da_huc12_sqkm}{numeric. Contributing DA (HUC12-only).}
#'     \item{contrib_da_huc10_sqkm}{numeric or NA. Contributing DA (HUC10-level).}
#'     \item{contrib_da_huc08_sqkm}{numeric or NA. Contributing DA (HUC08-level).}
#'     \item{network_da_sqkm}{numeric. Network-derived total DA for comparison.}
#'     \item{start_feature}{sf data.frame. The resolved NLDI start feature.}
#'     \item{hu12_by_huc12}{sf data.frame. NLDI-identified upstream HUC12 polygons (EPSG:5070).}
#'     \item{hu12_by_huc10}{sf data.frame or NULL. Upstream HUC12 polygons (HUC10 query).
#'       NULL when basin is within a single HUC10.}
#'     \item{hu12_by_huc08}{sf data.frame or NULL. Upstream HUC12 polygons (HUC08 query).
#'       NULL when basin is within a single HUC08.}
#'     \item{extra_catchments}{sf data.frame. Catchments between outlet and HUC12 outlets.}
#'     \item{split_catchment}{sf data.frame. Split catchment at HUC12 outlet(s).}
#'     \item{all_network}{data.frame. Full upstream network from VAA.}
#'     \item{hu12_outlet}{sf data.frame. HUC12 pour points upstream of outlet.}
#'   }
#'
#' @importFrom sf st_transform st_geometry st_area
#' @importFrom units set_units
#' @importFrom dplyr select
#' @importFrom hydroloom subset_network navigate_hydro_network
#'   navigate_network_dfs add_toids
#' @importFrom dataRetrieval findNLDI
#' @export
#' @examples
#' \donttest{
#' # Black Earth Creek
#' start <- list(featureSource = "nwissite", featureID = "USGS-05406500")
#' vaa <- get_vaa()
#' result <- get_drainage_area_estimates(start, vaa)
#' result$da_huc10_sqkm
#' result$network_da_sqkm
#' }
get_drainage_area_estimates <- function(start, vaa) {

  # resolve start feature via NLDI
  if(inherits(start, "sfc")) {
    message("Fetching waterbody...")

    # get nhdplusv2 waterbody
    wb <- get_waterbodies(start)

    start_feature <- wb
    
    if(!nrow(wb) == 1 | !inherits(wb, "sf")) {
      stop("point needs to fall in a waterbody!")
    }

    all_wb <- vaa[vaa$wbareacomi == wb$comid,]

    outlet_comids <- all_wb$comid[!all_wb$tonode %in% all_wb$fromnode]

    message("Found ", length(outlet_comids), " outlets for waterbody")

  } else {

    message("Resolving start feature via NLDI...")
    start_nldi <- get_nldi_feature(start)
    start_feature <- start_nldi
    outlet_comids <- start_nldi$comid
  }

  if(length(outlet_comids) == 0 || all(is.na(outlet_comids)))
    stop("No outlet COMIDs found for the provided start location. ",
      "Check that the input resolves to a valid NHDPlus feature.")

  # HUC12 pour points upstream via NLDI
  message("Finding HUC12 pour points upstream via NLDI...")
  huc12_outlets <- lapply(
    outlet_comids, \(x) {
      findNLDI(
        comid = x, nav = "UT",
        distance_km = 999, find = "huc12pp"
      )$UT_huc12pp
    }
  ) |> bind_rows()
  
  # full upstream network from VAA
  message("Subsetting upstream network from VAA for COMID ",
    outlet_comids, "...")
  all_net <- lapply(outlet_comids, \(x) {
    hydroloom::navigate_hydro_network(vaa, x, mode = "UT")
  })
  
  all_net <- filter(vaa, .data$comid %in% do.call(c, all_net))

  all_net <- add_toids(all_net, return_dendritic = TRUE)

  network_da <- max(all_net$totdasqkm)
  message("  ", nrow(all_net), " flowlines, totdasqkm = ",
    round(network_da, 2))

  message("  Found ", nrow(huc12_outlets), " HUC12 outlets")

  huc12_comids <- huc12_outlets$comid

  # find the immediately-upstream HUC12 outlets
  immediate_hu12_comids <- find_immediate_huc12_outlets(
    all_net, outlet_comids, huc12_comids
  )

  message("  ", length(immediate_hu12_comids),
    " immediately-upstream HUC12 outlets ",
    "(of ", length(huc12_comids), " total)")

  outlet_huc <- huc12_outlets[
    huc12_outlets$comid %in% immediate_hu12_comids,
  ]

  # fetch and compute areas for upstream HUC12s
  hu12_result <- get_upstream_huc12s(huc12_outlets, outlet_huc)

  # compute gap area between outlet and HUC12 outlets
  gap <- compute_gap_area(outlet_huc, all_net, vaa)

  # assemble DA estimates (NA when fewer than a whole HUC10/08 upstream)
  da_huc12_sqkm <- sum(hu12_result$hu12_by_huc12$dasqkm) +
    gap$extra_and_local
  contrib_da_huc12_sqkm <- sum(hu12_result$hu12_by_huc12$contrib_sqkm) +
    gap$extra_and_local

  if(!is.null(hu12_result$hu12_by_huc10)) {
    da_huc10_sqkm <- sum(hu12_result$hu12_by_huc10$dasqkm) +
      gap$extra_and_local
    contrib_da_huc10_sqkm <- sum(hu12_result$hu12_by_huc10$contrib_sqkm) +
      gap$extra_and_local
  } else {
    da_huc10_sqkm <- NA_real_
    contrib_da_huc10_sqkm <- NA_real_
  }

  if(!is.null(hu12_result$hu12_by_huc08)) {
    da_huc08_sqkm <- sum(hu12_result$hu12_by_huc08$dasqkm) +
      gap$extra_and_local
    contrib_da_huc08_sqkm <- sum(hu12_result$hu12_by_huc08$contrib_sqkm) +
      gap$extra_and_local
  } else {
    da_huc08_sqkm <- NA_real_
    contrib_da_huc08_sqkm <- NA_real_
  }

  message("  HUC12 DA = ", round(da_huc12_sqkm, 2),
    ", contributing = ", round(contrib_da_huc12_sqkm, 2))
  if(!is.na(da_huc10_sqkm))
    message("  HUC10 DA = ", round(da_huc10_sqkm, 2),
      ", contributing = ", round(contrib_da_huc10_sqkm, 2))
  if(!is.na(da_huc08_sqkm))
    message("  HUC08 DA = ", round(da_huc08_sqkm, 2),
      ", contributing = ", round(contrib_da_huc08_sqkm, 2))
  message("  Network DA = ", round(network_da, 2))

  list(
    da_huc12_sqkm = da_huc12_sqkm,
    da_huc10_sqkm = da_huc10_sqkm,
    da_huc08_sqkm = da_huc08_sqkm,
    contrib_da_huc12_sqkm = contrib_da_huc12_sqkm,
    contrib_da_huc10_sqkm = contrib_da_huc10_sqkm,
    contrib_da_huc08_sqkm = contrib_da_huc08_sqkm,
    network_da_sqkm = network_da,
    start_feature = start_feature,
    hu12_by_huc12 = hu12_result$hu12_by_huc12,
    hu12_by_huc10 = hu12_result$hu12_by_huc10,
    hu12_by_huc08 = hu12_result$hu12_by_huc08,
    extra_catchments = gap$extra_catchments,
    split_catchment = gap$split_catchment,
    all_network = all_net,
    hu12_outlet = huc12_outlets
  )
}

#' Find immediately-upstream HUC12 outlets
#'
#' Given the full upstream network with toids, identifies HUC12 outlets
#' reachable from the start COMID without passing through another HUC12 outlet.
#'
#' @param all_net data.frame with comid and toid columns (from add_toids)
#' @param start_comid integer. The outlet COMID.
#' @param huc12_comids integer vector. COMIDs of all upstream HUC12 pour points.
#' @return integer vector of immediately-upstream HUC12 outlet COMIDs
#' @noRd
find_immediate_huc12_outlets <- function(all_net, start_comid, huc12_comids) {

  message("Finding immediately-upstream HUC12 outlets...")

  trim_comids <- setdiff(huc12_comids, start_comid)
  trimmed_net <- all_net[!all_net$comid %in% trim_comids, ] |>
    select(comid, toid)

  reachable <- unlist(
    navigate_network_dfs(trimmed_net, start_comid, direction = "up")
  )

  # immediate outlets are h12 comids whose downstream neighbor is in the

  # reachable set (or is the start comid itself)
  huc12_comids[huc12_comids %in%
    all_net$comid[all_net$toid %in% reachable |
      all_net$toid %in% start_comid]] |>
    unique()
}

#' Plan upstream HUC12 fetch operations
#'
#' Given NLDI-identified HUC12 IDs and the outlet HUC12 ID, determines
#' which HUC IDs to fetch for each of three drainage-area estimates:
#' HUC12-only, HUC10-level, and HUC08-level. Pure logic -- no web calls.
#'
#' @param all_huc12_ids character. NLDI-identified upstream HUC12 identifiers
#'   (12-digit strings).
#' @param outlet_huc12_ids character. The immediately-upstream HUC12 outlet
#'   identifier(s) (12-digit strings). May be length > 1 when the network
#'   branches upstream of the start COMID.
#' @return list with elements \code{huc12_est}, \code{huc10_est},
#'   \code{huc08_est}. Each estimate contains the IDs needed for fetching
#'   and a flag indicating whether it is the same as a simpler estimate.
#' @noRd
plan_upstream_huc12_fetches <- function(all_huc12_ids, outlet_huc12_ids) {

  outlet_huc10s <- unique(substr(outlet_huc12_ids, 1, 10))
  outlet_huc08s <- unique(substr(outlet_huc12_ids, 1, 8))
  all_huc10s <- unique(substr(all_huc12_ids, 1, 10))
  all_huc08s <- sort(unique(substr(all_huc12_ids, 1, 8)))

  # Estimate 1: HUC12-only -- just the NLDI IDs
  huc12_est <- list(fetch_ids = all_huc12_ids)

  # Estimate 2: HUC10-level -- complete upstream HUC10s + huc12pp in outlet HUC10(s)
  if(!all(all_huc10s %in% outlet_huc10s)) {
    upstream_huc10s <- all_huc10s[!all_huc10s %in% outlet_huc10s]
    local_huc12_ids <- all_huc12_ids[
      substr(all_huc12_ids, 1, 10) %in% outlet_huc10s &
        !all_huc12_ids %in% outlet_huc12_ids
    ]
    # Outlet HUC10 backfill: some HUC12s within the outlet HUC10 have no
    # huc12pp pour point (no flowline exits the HUC12 on the NHD network).
    # These are missed by NLDI navigation. The fix: query ALL HUC12s in
    # the outlet HUC10(s), then filter to those upstream of the outlet by
    # sort order (huc_12 < outlet huc_12).
    huc10_est <- list(
      bulk_huc10s = upstream_huc10s,
      local_huc12_ids = local_huc12_ids,
      outlet_backfill = list(
        outlet_huc10s = outlet_huc10s,
        outlet_huc12_ids = outlet_huc12_ids
      ),
      same_as_huc12 = FALSE
    )
  } else {
    huc10_est <- list(
      bulk_huc10s = character(0),
      local_huc12_ids = character(0),
      same_as_huc12 = TRUE
    )
  }

  # Estimate 3: HUC08-level -- complete upstream HUC08s + huc12pp in outlet HUC08(s)
  if(!all(all_huc08s %in% outlet_huc08s)) {
    upstream_huc08s <- all_huc08s[!all_huc08s %in% outlet_huc08s]
    local_huc12_ids_huc08 <- all_huc12_ids[
      substr(all_huc12_ids, 1, 8) %in% outlet_huc08s &
        !all_huc12_ids %in% outlet_huc12_ids
    ]
    huc08_est <- list(
      bulk_huc08s = upstream_huc08s,
      local_huc12_ids = local_huc12_ids_huc08,
      outlet_backfill = list(
        outlet_huc10s = outlet_huc10s,
        outlet_huc12_ids = outlet_huc12_ids
      ),
      same_as_huc10 = FALSE
    )
  } else {
    huc08_est <- list(
      bulk_huc08s = character(0),
      local_huc12_ids = character(0),
      same_as_huc10 = TRUE
    )
  }

  list(huc12_est = huc12_est, huc10_est = huc10_est, huc08_est = huc08_est)
}

#' Fetch upstream HUC12 polygons according to a fetch plan
#'
#' Executes the web service calls described by a plan from
#' \code{plan_upstream_huc12_fetches} and returns three sf data.frames
#' of HUC12 polygons with area columns.
#'
#' @param plan list. Output of \code{plan_upstream_huc12_fetches}.
#' @return list with \code{hu12_by_huc12}, \code{hu12_by_huc10}, and
#'   \code{hu12_by_huc08} sf objects, each with columns \code{dasqkm},
#'   \code{ncontrb_sqkm}, \code{contrib_sqkm}.
#' @noRd
fetch_upstream_huc12s <- function(plan) {

  empty_sf <- sf::st_sf(geometry = sf::st_sfc(crs = 5070))

  # Estimate 1: HUC12-only
  hu12_by_huc12 <- if(length(plan$huc12_est$fetch_ids) > 0) {
    get_huc(id = plan$huc12_est$fetch_ids, type = "_nhdplusv2") |> st_transform(5070)
  } else {
    empty_sf
  }

  # Estimate 2: HUC10-level (NA when basin is within a single HUC10)
  if(plan$huc10_est$same_as_huc12) {
    hu12_by_huc10 <- NULL
  } else {
    parts <- list()
    if(length(plan$huc10_est$bulk_huc10s) > 0)
      parts$bulk <- get_huc12_by_huc(plan$huc10_est$bulk_huc10s)
    # Backfill: fetch ALL HUC12s in outlet HUC10(s) and filter by sort
    # order. This replaces individual local_huc12_ids fetches and also
    # captures HUC12s with no huc12pp pour point.
    bf <- plan$huc10_est$outlet_backfill
    if(length(bf$outlet_huc10s) > 0) {
      outlet_hu12s <- get_huc12_by_huc(bf$outlet_huc10s)
      if(nrow(outlet_hu12s) > 0) {
        max_per_h10 <- tapply(
          bf$outlet_huc12_ids, substr(bf$outlet_huc12_ids, 1, 10), max
        )
        keep <- vapply(outlet_hu12s$huc_12, function(h12) {
          h10 <- substr(h12, 1, 10)
          h10 %in% names(max_per_h10) && h12 < max_per_h10[[h10]]
        }, logical(1))
        local <- outlet_hu12s[keep, ]
        # dedup against bulk
        if(!is.null(parts$bulk) && nrow(local) > 0)
          local <- local[!local$huc_12 %in% parts$bulk$huc_12, ]
        if(nrow(local) > 0) parts$local <- local
        message("  Backfill (HUC10): ", sum(keep), " HUC12s in outlet HUC10")
      }
    }
    hu12_by_huc10 <- if(length(parts) > 0) {
      sf::st_sf(dplyr::bind_rows(parts)) |> st_transform(5070)
    } else {
      empty_sf
    }
  }

  # Estimate 3: HUC08-level (NA when basin is within a single HUC08)
  if(plan$huc08_est$same_as_huc10) {
    hu12_by_huc08 <- NULL
  } else {
    parts <- list()
    if(length(plan$huc08_est$bulk_huc08s) > 0)
      parts$huc08 <- get_huc12_by_huc(plan$huc08_est$bulk_huc08s)
    # Backfill: same as HUC10 estimate — fetch ALL HUC12s in outlet
    # HUC10(s) to capture those missing from huc12pp.
    bf <- plan$huc08_est$outlet_backfill
    if(length(bf$outlet_huc10s) > 0) {
      outlet_hu12s <- get_huc12_by_huc(bf$outlet_huc10s)
      if(nrow(outlet_hu12s) > 0) {
        max_per_h10 <- tapply(
          bf$outlet_huc12_ids, substr(bf$outlet_huc12_ids, 1, 10), max
        )
        keep <- vapply(outlet_hu12s$huc_12, function(h12) {
          h10 <- substr(h12, 1, 10)
          h10 %in% names(max_per_h10) && h12 < max_per_h10[[h10]]
        }, logical(1))
        local <- outlet_hu12s[keep, ]
        # dedup against bulk
        if(!is.null(parts$huc08) && nrow(local) > 0)
          local <- local[!local$huc_12 %in% parts$huc08$huc_12, ]
        if(nrow(local) > 0) parts$local <- local
        message("  Backfill (HUC08): ", sum(keep), " HUC12s in outlet HUC10")
      }
    }
    hu12_by_huc08 <- if(length(parts) > 0) {
      sf::st_sf(dplyr::bind_rows(parts)) |> st_transform(5070)
    } else {
      empty_sf
    }
  }

  hu12_by_huc12 <- add_huc12_area_columns(hu12_by_huc12)
  if(!is.null(hu12_by_huc10))
    hu12_by_huc10 <- add_huc12_area_columns(hu12_by_huc10)
  if(!is.null(hu12_by_huc08))
    hu12_by_huc08 <- add_huc12_area_columns(hu12_by_huc08)

  message("  HUC12 query: ", nrow(hu12_by_huc12), " HUC12s, ",
    "total = ", round(sum(hu12_by_huc12$dasqkm), 2), " sq km, ",
    "contributing = ", round(sum(hu12_by_huc12$contrib_sqkm), 2),
    " sq km")
  if(!is.null(hu12_by_huc10))
    message("  HUC10 query: ", nrow(hu12_by_huc10), " HUC12s, ",
      "total = ", round(sum(hu12_by_huc10$dasqkm), 2), " sq km, ",
      "contributing = ", round(sum(hu12_by_huc10$contrib_sqkm), 2),
      " sq km")
  if(!is.null(hu12_by_huc08))
    message("  HUC08 query: ", nrow(hu12_by_huc08), " HUC12s, ",
      "total = ", round(sum(hu12_by_huc08$dasqkm), 2), " sq km, ",
      "contributing = ", round(sum(hu12_by_huc08$contrib_sqkm), 2),
      " sq km")

  list(
    hu12_by_huc12 = hu12_by_huc12,
    hu12_by_huc10 = hu12_by_huc10,
    hu12_by_huc08 = hu12_by_huc08
  )
}

#' Fetch upstream HUC12s and compute area columns
#'
#' Derives HUC08/HUC10 groupings from HUC12 identifiers and fetches
#' HUC12 polygons via the OGC API. Computes total, non-contributing,
#' and contributing area columns on the result.
#'
#' @param huc12_outlets list. NLDI result from findNLDI containing
#'   \code{UT_huc12pp} element.
#' @param outlet_huc sf data.frame. The immediately-upstream HUC12 outlet row.
#' @return list with \code{hu12_by_huc12}, \code{hu12_by_huc10}, and
#'   \code{hu12_by_huc08} sf objects, each with columns \code{dasqkm},
#'   \code{ncontrb_sqkm}, \code{contrib_sqkm}.
#' @noRd
get_upstream_huc12s <- function(huc12_outlets, outlet_huc) {

  all_huc12_ids <- huc12_outlets$identifier
  outlet_huc12_ids <- outlet_huc$identifier

  plan <- plan_upstream_huc12_fetches(all_huc12_ids, outlet_huc12_ids)
  fetch_upstream_huc12s(plan)
}

#' Add area columns to HUC12 sf object
#'
#' Computes \code{dasqkm} (total area from geometry), \code{ncontrb_sqkm}
#' (non-contributing area converted from acres), and \code{contrib_sqkm}
#' (total minus non-contributing).
#'
#' @param hu12 sf data.frame of HUC12 features with \code{ncontrb_a} column.
#' @return sf data.frame with added area columns
#' @noRd
add_huc12_area_columns <- function(hu12) {
  acres_to_sqkm <- 0.00404686
  hu12$dasqkm <- as.numeric(set_units(st_area(hu12), "km^2"))
  hu12$ncontrb_sqkm <- hu12$ncontrb_a * acres_to_sqkm
  hu12$contrib_sqkm <- hu12$dasqkm - hu12$ncontrb_sqkm
  hu12
}

#' Compute gap area between outlet and HUC12 outlets
#'
#' Splits the catchment at each HUC12 outlet, navigates upstream from each
#' outlet, and identifies catchments in the gap between the gage
#' and the HUC12 outlets.
#'
#' @param outlet_huc sf data.frame. The immediately-upstream HUC12 outlet(s).
#'   May have multiple rows when the network branches.
#' @param all_net data.frame. Full upstream network with comid, toid, areasqkm.
#' @param vaa data.frame. VAA table for navigation.
#' @return list with \code{extra_net} (data.frame), \code{extra_catchments}
#'   (sf data.frame), \code{split_catchment} (sf data.frame),
#'   \code{extra_and_local} (numeric area in sq km).
#' @noRd
compute_gap_area <- function(outlet_huc, all_net, vaa) {

  n_outlets <- nrow(outlet_huc)
  message("Splitting catchments at ", n_outlets, " HUC12 outlet(s)...")

  all_hu12_outlet_ut <- integer(0)
  total_local_dasqkm <- 0
  split_catches <- vector("list", n_outlets)

  for(i in seq_len(n_outlets)) {
    oh <- outlet_huc[i, ]

    split_catch <- get_split_catchment(
      st_geometry(oh),
      upstream = FALSE
    ) |> st_transform(5070)

    split_catch$dasqkm <- as.numeric(set_units(st_area(split_catch), "km^2"))
    split_catches[[i]] <- split_catch

    local_dasqkm <-
      split_catch$dasqkm[split_catch$id == "catchment"] -
      split_catch$dasqkm[split_catch$id == "splitCatchment"]
    total_local_dasqkm <- total_local_dasqkm + local_dasqkm

    ut_comids <- navigate_hydro_network(vaa, oh$comid, mode = "UT")
    all_hu12_outlet_ut <- union(all_hu12_outlet_ut, ut_comids)
  }

  split_catch <- do.call(rbind, split_catches)

  # catchments between the gage and the HUC12 outlets
  message("Navigating network upstream of HUC12 outlets...")
  extra_net <- all_net[!all_net$comid %in% all_hu12_outlet_ut, ]
  message("  ", nrow(extra_net),
    " extra catchments between outlet and HUC12 outlets")

  extra_and_local <- sum(c(extra_net$areasqkm, total_local_dasqkm))

  # fetch catchment geometries for the extra portion
  if(nrow(extra_net) > 0) {
    message("Fetching extra catchment geometries...")
    extra_cat <- get_nhdplus(
      comid = extra_net$comid, realization = "catchment"
    )
  } else {
    extra_cat <- sf::st_sf(geometry = sf::st_sfc(crs = 5070))
  }

  list(
    extra_net = extra_net,
    extra_catchments = extra_cat,
    split_catchment = split_catch,
    extra_and_local = extra_and_local
  )
}
