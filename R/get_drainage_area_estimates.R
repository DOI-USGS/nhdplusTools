#' @title Estimate drainage area from HUC and catchment data
#' @description Combines HUC12 areas upstream of HUC outlets with NHDPlusV2
#'   catchment areas for the portion of the basin between the outlet and HUC12
#'   outlets to produce a drainage area estimate. Non-contributing areas
#'   captured in HUC12 boundaries are included, making this more representative
#'   than purely network-derived totals.
#' @details
#'   By default, network navigation is performed via the NLDI web service and
#'   flowline attributes are retrieved from the NHDPlusV2 OGC API. When
#'   \code{local_navigation = TRUE}, the NHDPlusV2 Value Added Attributes
#'   (\code{\link{get_vaa}}) are used for network navigation instead, and
#'   only HUC12 pour points are fetched from the NLDI.
#'
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
#' @param catchments logical. If TRUE, fetch and return NHDPlusV2 catchment
#'   polygons for the full upstream network. Default FALSE.
#' @param nhdplushr logical. If TRUE (the default), compute a drainage area
#'   estimate from NHDPlusHR catchments. Set to FALSE to skip this step, which
#'   avoids the HR web service calls and speeds up computation.
#' @param local_navigation logical. If TRUE, use \code{\link{get_vaa}} for
#'   network navigation and flowline attributes instead of NLDI/OGC API web
#'   services. Only HUC12 pour points are fetched from the NLDI. Default FALSE.
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
#'     \item{nhdplushr_network_dasqkm}{numeric or NA. Drainage area from
#'       NHDPlusHR catchments upstream of the matched HR flowline. NA with a
#'       warning when the HR web service is unavailable or fails.}
#'     \item{start_feature}{sf data.frame. The resolved NLDI start feature.}
#'     \item{hu12_by_huc12}{sf data.frame. NLDI-identified upstream HUC12 polygons (EPSG:5070).}
#'     \item{hu12_by_huc10}{sf data.frame or NULL. Upstream HUC12 polygons (HUC10 query).
#'       NULL when basin is within a single HUC10.}
#'     \item{hu12_by_huc08}{sf data.frame or NULL. Upstream HUC12 polygons (HUC08 query).
#'       NULL when basin is within a single HUC08.}
#'     \item{extra_catchments}{sf data.frame. Catchments between outlet and HUC12 outlets.}
#'     \item{split_catchment}{sf data.frame. Split catchment at HUC12 outlet(s).}
#'     \item{all_network}{data.frame. Full upstream flowline attributes.}
#'     \item{all_catchments}{sf data.frame or NULL. NHDPlusV2 catchment polygons
#'       for the full upstream network. NULL when \code{catchments = FALSE}.}
#'     \item{hu12_outlet}{sf data.frame. HUC12 pour points upstream of outlet.}
#'   }
#'
#' @importFrom sf st_transform st_geometry st_area
#' @importFrom units set_units
#' @importFrom dplyr select filter bind_rows
#' @importFrom hydroloom navigate_hydro_network
#'   navigate_network_dfs add_toids
#' @importFrom dataRetrieval findNLDI
#' @export
#' @examples
#' \donttest{
#' # Black Earth Creek
#' start <- list(featureSource = "nwissite", featureID = "USGS-05406500")
#' result <- get_drainage_area_estimates(start)
#' result$da_huc10_sqkm
#' result$network_da_sqkm
#' }
get_drainage_area_estimates <- function(start, catchments = FALSE,
  nhdplushr = TRUE, local_navigation = FALSE) {

  if(local_navigation) {
    message("Loading NHDPlusV2 VAA for local navigation...")
    vaa <- get_vaa()
  }

  # resolve start feature via NLDI
  if(inherits(start, "sfc")) {
    message("Fetching waterbody...")

    # get nhdplusv2 waterbody
    wb <- get_waterbodies(start)

    start_feature <- wb

    if(!nrow(wb) == 1 | !inherits(wb, "sf")) {
      stop("point needs to fall in a waterbody!")
    }

    if(local_navigation) {
      all_wb <- vaa[vaa$wbareacomi == wb$comid, ]
      outlet_comids <- all_wb$comid[!all_wb$tonode %in% all_wb$fromnode]
    } else {
      # fetch flowlines associated with this waterbody (attributes only)
      message("Finding flowlines for waterbody COMID ", wb$comid, "...")
      wb_flowlines <- query_usgs_oafeat(
        AOI = wb,
        type = "nhd",
        filter = paste0("wbareacomi%20=%20", wb$comid),
        properties = c("comid", "tonode", "fromnode"),
        skip_geometry = TRUE
      )

      if(is.null(wb_flowlines) || nrow(wb_flowlines) == 0)
        stop("No flowlines found for waterbody COMID ", wb$comid)

      outlet_comids <- wb_flowlines$comid[
        !wb_flowlines$tonode %in% wb_flowlines$fromnode
      ]
    }

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

  # HUC12 pour points via NLDI
  message("Finding HUC12 pour points upstream via NLDI...")
  if(local_navigation) {
    huc12_outlets <- lapply(outlet_comids, \(x) {
      findNLDI(
        comid = x, nav = "UT",
        distance_km = 9999, find = "huc12pp"
      )$UT_huc12pp
    }) |> bind_rows()

    # full upstream network from VAA
    message("Subsetting upstream network from VAA...")
    ut_comids <- lapply(outlet_comids, \(x) {
      hydroloom::navigate_hydro_network(vaa, x, mode = "UT")
    })
    all_net <- filter(vaa, .data$comid %in% do.call(c, ut_comids))
  } else {
    nldi_results <- lapply(outlet_comids, \(x) {
      findNLDI(
        comid = x, nav = "UT",
        distance_km = 9999, find = c("huc12pp", "flowline")
      )
    })

    huc12_outlets <- lapply(nldi_results, \(x) x$UT_huc12pp) |>
      bind_rows()

    upstream_comids <- unique(as.integer(unlist(
      lapply(nldi_results, \(x) x$UT_flowlines$nhdplus_comid)
    )))

    # fetch full flowline attributes (no geometry needed)
    message("Fetching flowline attributes for ",
      length(upstream_comids), " upstream COMIDs...")
    all_net <- get_nhdplus(comid = upstream_comids, realization = "flowline",
      skip_geometry = TRUE)
  }

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
  nav_net <- if(local_navigation) vaa else all_net
  gap <- compute_gap_area(outlet_huc, all_net, nav_net)

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

  # NHDPlusHR estimate -- standalone, warns on failure
  if(nhdplushr) {
    hu12_polys <- if(!is.null(hu12_result$hu12_by_huc08)) {
      hu12_result$hu12_by_huc08
    } else if(!is.null(hu12_result$hu12_by_huc10)) {
      hu12_result$hu12_by_huc10
    } else {
      hu12_result$hu12_by_huc12
    }

    message("Computing NHDPlusHR drainage area estimate...")
    hr_result <- get_nhdplushr_da_estimate(
      start_feature, network_da, hu12_polys
    )

    if(!is.na(hr_result$nhdplushr_network_dasqkm))
      message("  NHDPlusHR DA = ",
        round(hr_result$nhdplushr_network_dasqkm, 2))
  } else {
    hr_result <- list(nhdplushr_network_dasqkm = NA_real_)
  }

  # optional catchment retrieval
  if(catchments) {
    message("Fetching network catchment geometries...")
    all_catchments <- get_nhdplus(
      comid = all_net$comid, realization = "catchment"
    )
  } else {
    all_catchments <- NULL
  }

  list(
    da_huc12_sqkm = da_huc12_sqkm,
    da_huc10_sqkm = da_huc10_sqkm,
    da_huc08_sqkm = da_huc08_sqkm,
    contrib_da_huc12_sqkm = contrib_da_huc12_sqkm,
    contrib_da_huc10_sqkm = contrib_da_huc10_sqkm,
    contrib_da_huc08_sqkm = contrib_da_huc08_sqkm,
    network_da_sqkm = network_da,
    nhdplushr_network_dasqkm = hr_result$nhdplushr_network_dasqkm,
    start_feature = start_feature,
    hu12_by_huc12 = hu12_result$hu12_by_huc12,
    hu12_by_huc10 = hu12_result$hu12_by_huc10,
    hu12_by_huc08 = hu12_result$hu12_by_huc08,
    extra_catchments = gap$extra_catchments,
    split_catchment = gap$split_catchment,
    all_network = all_net,
    all_catchments = all_catchments,
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

#' Filter outlet backfill HUC12s by sort order
#'
#' Filters HUC12 features in the outlet HUC10(s) to those upstream of the
#' outlet HUC12(s) using lexicographic sort order.
#'
#' @param outlet_hu12s sf data.frame. All HUC12s in the outlet HUC10(s).
#' @param outlet_huc12_ids character. The outlet HUC12 identifier(s).
#' @param bulk sf data.frame or NULL. Bulk HUC12s to dedup against.
#' @param label character. Label for progress message.
#' @return sf data.frame of filtered backfill HUC12s (may have 0 rows).
#' @noRd
filter_outlet_backfill <- function(outlet_hu12s, outlet_huc12_ids,
  bulk = NULL, label = "backfill") {

  if(nrow(outlet_hu12s) == 0) return(outlet_hu12s)

  max_per_h10 <- tapply(
    outlet_huc12_ids, substr(outlet_huc12_ids, 1, 10), max
  )
  keep <- vapply(outlet_hu12s$huc_12, function(h12) {
    h10 <- substr(h12, 1, 10)
    h10 %in% names(max_per_h10) && h12 < max_per_h10[[h10]]
  }, logical(1))
  local <- outlet_hu12s[keep, ]

  if(!is.null(bulk) && nrow(local) > 0)
    local <- local[!local$huc_12 %in% bulk$huc_12, ]

  message("  Backfill (", label, "): ", sum(keep), " HUC12s in outlet HUC10")
  local
}

#' Union two HUC12 sf sets by huc_12 identifier
#'
#' Combines two sf data.frames of HUC12 features, deduplicating by
#' \code{huc_12}. Features in \code{target} take precedence when the same
#' HUC12 appears in both sets. Used to guarantee the superset property:
#' HUC10 estimate >= HUC12 estimate, HUC08 estimate >= HUC10 estimate.
#'
#' @param target sf data.frame. The estimate being extended.
#' @param base sf data.frame. The lower-level estimate to include.
#' @return sf data.frame containing all features from both sets, deduplicated.
#' @noRd
union_huc12_sets <- function(target, base) {
  if(is.null(base) || nrow(base) == 0) return(target)
  if(is.null(target) || nrow(target) == 0) return(base)

  missing <- base[!base$huc_12 %in% target$huc_12, ]
  if(nrow(missing) == 0) return(target)

  message("  Superset union: adding ", nrow(missing),
    " HUC12s from base estimate")
  dplyr::bind_rows(target, missing)
}

#' Fetch upstream HUC12 polygons according to a fetch plan
#'
#' Executes the web service calls described by a plan from
#' \code{plan_upstream_huc12_fetches} and returns three sf data.frames
#' of HUC12 polygons with area columns. When both HUC10 and HUC08
#' estimates are active, the HUC08 bulk query is fetched first and
#' the HUC10 subset is derived from it to avoid redundant requests.
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
    get_huc(id = plan$huc12_est$fetch_ids, type = "_nhdplusv2") |>
      st_transform(5070)
  } else {
    empty_sf
  }

  huc10_active <- !plan$huc10_est$same_as_huc12
  huc08_active <- huc10_active && !plan$huc08_est$same_as_huc10

  if(!huc10_active) {
    hu12_by_huc10 <- NULL
    hu12_by_huc08 <- NULL
  } else {
    # Outlet backfill: fetch once, shared by both estimates
    bf <- plan$huc10_est$outlet_backfill
    backfill_raw <- if(length(bf$outlet_huc10s) > 0) {
      get_huc12_by_huc(bf$outlet_huc10s)
    } else {
      empty_sf
    }

    if(huc08_active) {
      # Fetch the broadest query first: all HUC12s in upstream HUC08s
      huc08_bulk <- if(length(plan$huc08_est$bulk_huc08s) > 0) {
        get_huc12_by_huc(plan$huc08_est$bulk_huc08s)
      } else {
        empty_sf
      }

      # HUC10s covered by the HUC08 bulk vs those needing separate fetch
      covered_huc10s <- plan$huc10_est$bulk_huc10s[
        substr(plan$huc10_est$bulk_huc10s, 1, 8) %in%
          plan$huc08_est$bulk_huc08s
      ]
      extra_huc10s <- setdiff(plan$huc10_est$bulk_huc10s, covered_huc10s)

      # Derive HUC10 subset from HUC08 results
      huc10_from_huc08 <- if(nrow(huc08_bulk) > 0 &&
          length(covered_huc10s) > 0) {
        huc08_bulk[
          substr(huc08_bulk$huc_12, 1, 10) %in% covered_huc10s, ]
      } else {
        empty_sf
      }

      # Fetch remaining HUC10s not covered by HUC08 query
      huc10_extra <- if(length(extra_huc10s) > 0) {
        get_huc12_by_huc(extra_huc10s)
      } else {
        empty_sf
      }

      message("  HUC10 from HUC08: ", nrow(huc10_from_huc08),
        " HUC12s reused, ", nrow(huc10_extra), " fetched separately")

      # Assemble HUC08 estimate
      # Include: bulk HUC08s + non-outlet HUC10s in the outlet HUC08
      # (extra_huc10s) + backfill for the outlet HUC10
      huc08_parts <- list()
      if(nrow(huc08_bulk) > 0) huc08_parts$bulk <- huc08_bulk
      if(nrow(huc10_extra) > 0) huc08_parts$outlet_huc08_huc10s <- huc10_extra
      huc08_bulk_all <- if(length(huc08_parts) > 0) {
        dplyr::bind_rows(huc08_parts)
      } else {
        NULL
      }
      backfill_huc08 <- filter_outlet_backfill(
        backfill_raw, bf$outlet_huc12_ids,
        bulk = huc08_bulk_all, label = "HUC08")
      if(nrow(backfill_huc08) > 0) huc08_parts$backfill <- backfill_huc08

      hu12_by_huc08 <- if(length(huc08_parts) > 0) {
        sf::st_sf(dplyr::bind_rows(huc08_parts)) |> st_transform(5070)
      } else {
        empty_sf
      }

      # Assemble HUC10 estimate
      huc10_parts <- list()
      if(nrow(huc10_from_huc08) > 0)
        huc10_parts$from_huc08 <- huc10_from_huc08
      if(nrow(huc10_extra) > 0) huc10_parts$extra <- huc10_extra
      all_huc10_bulk <- if(length(huc10_parts) > 0) {
        dplyr::bind_rows(huc10_parts)
      } else {
        NULL
      }
      backfill_huc10 <- filter_outlet_backfill(
        backfill_raw, bf$outlet_huc12_ids,
        bulk = all_huc10_bulk, label = "HUC10")
      if(nrow(backfill_huc10) > 0)
        huc10_parts$backfill <- backfill_huc10

      hu12_by_huc10 <- if(length(huc10_parts) > 0) {
        sf::st_sf(dplyr::bind_rows(huc10_parts)) |> st_transform(5070)
      } else {
        empty_sf
      }

      # Filter disconnected HUC10s before superset union
      network_ids <- hu12_by_huc12$huc_12
      keep_10 <- filter_disconnected_huc12s(hu12_by_huc10$huc_12, network_ids)
      hu12_by_huc10 <- hu12_by_huc10[hu12_by_huc10$huc_12 %in% keep_10, ]
      keep_08 <- filter_disconnected_huc12s(
        hu12_by_huc08$huc_12, network_ids, parent_nchar = 8L
      )
      hu12_by_huc08 <- hu12_by_huc08[hu12_by_huc08$huc_12 %in% keep_08, ]

      # Enforce superset property: HUC10 >= HUC12, HUC08 >= HUC10
      hu12_by_huc10 <- union_huc12_sets(hu12_by_huc10, hu12_by_huc12)
      hu12_by_huc08 <- union_huc12_sets(hu12_by_huc08, hu12_by_huc10)

    } else {
      # Only HUC10 active, no HUC08
      hu12_by_huc08 <- NULL
      huc10_parts <- list()
      if(length(plan$huc10_est$bulk_huc10s) > 0)
        huc10_parts$bulk <- get_huc12_by_huc(plan$huc10_est$bulk_huc10s)
      backfill_huc10 <- filter_outlet_backfill(
        backfill_raw, bf$outlet_huc12_ids,
        bulk = huc10_parts$bulk, label = "HUC10")
      if(nrow(backfill_huc10) > 0)
        huc10_parts$backfill <- backfill_huc10

      hu12_by_huc10 <- if(length(huc10_parts) > 0) {
        sf::st_sf(dplyr::bind_rows(huc10_parts)) |> st_transform(5070)
      } else {
        empty_sf
      }

      # Filter disconnected HUC10s before superset union
      network_ids <- hu12_by_huc12$huc_12
      keep_10 <- filter_disconnected_huc12s(hu12_by_huc10$huc_12, network_ids)
      hu12_by_huc10 <- hu12_by_huc10[hu12_by_huc10$huc_12 %in% keep_10, ]

      # Enforce superset property: HUC10 >= HUC12
      hu12_by_huc10 <- union_huc12_sets(hu12_by_huc10, hu12_by_huc12)
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

#' Filter disconnected HUC12s from broader HUC queries
#'
#' When we fetch HUC12s by a broader HUC level (HUC10 or HUC08), we get all
#' HUC12s in each parent HUC group. The purpose of fetching by group is to
#' capture disconnected HUC12s -- those that are upstream in a landscape
#' sense but have no flowline exiting on the NHD network, so they are not
#' discovered by network navigation alone.
#'
#' However, this can also pull in HUC12s from groups that are not actually
#' upstream of the original outlet. To distinguish, we check whether the
#' outlet HUC12 of each group (the maximum HUC12 by sort order within the
#' group) is present in the on-network set discovered by NLDI navigation.
#' If the group outlet is on-network, the entire group is upstream and all
#' its HUC12s are kept -- including disconnected ones. If the group outlet
#' is not on-network, the group as a whole is not upstream, so only HUC12s
#' that were individually discovered on-network are retained.
#'
#' Groups are defined by \code{parent_nchar}: 10 for HUC10-level grouping
#' (used with the HUC10 query), 8 for HUC08-level grouping (used with the
#' HUC08 query). The grouping level should match the query level so that
#' entire HUC groups are kept or dropped as a unit.
#'
#' @param broader_huc12s character. HUC12 IDs from a broader HUC query
#'   (e.g. hu12_by_huc10 or hu12_by_huc08).
#' @param network_huc12s character. HUC12 IDs from the on-network (NLDI)
#'   hu12_by_huc12 set.
#' @param parent_nchar integer. Number of characters defining the parent
#'   group: 10 for HUC10, 8 for HUC08.
#' @return character vector of HUC12 IDs to keep.
#' @noRd
filter_disconnected_huc12s <- function(broader_huc12s, network_huc12s,
  parent_nchar = 10L) {

  if(length(broader_huc12s) == 0) return(broader_huc12s)

  parents <- substr(broader_huc12s, 1, parent_nchar)
  unique_parents <- unique(parents)

  keep <- logical(length(broader_huc12s))

  for(p in unique_parents) {
    in_p <- parents == p
    h12s_in_p <- broader_huc12s[in_p]
    outlet_h12 <- max(h12s_in_p)

    if(outlet_h12 %in% network_huc12s) {
      keep[in_p] <- TRUE
    } else {
      keep[in_p] <- h12s_in_p %in% network_huc12s
    }
  }

  n_dropped <- sum(!keep)
  if(n_dropped > 0)
    message("  Filtered ", n_dropped,
      " disconnected HUC12s from broader query")

  broader_huc12s[keep]
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
#' @param nav_net data.frame. Network used for upstream navigation. Defaults
#'   to \code{all_net}; pass the full VAA when using local navigation.
#' @return list with \code{extra_net} (data.frame), \code{extra_catchments}
#'   (sf data.frame), \code{split_catchment} (sf data.frame),
#'   \code{extra_and_local} (numeric area in sq km).
#' @noRd
compute_gap_area <- function(outlet_huc, all_net, nav_net = all_net) {

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

    ut_comids <- navigate_hydro_network(nav_net, oh$comid, mode = "UT")
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

#' Estimate drainage area from NHDPlusHR catchments
#'
#' Fetches NHDPlusHR flowlines, indexes the start point to the HR network
#' using drainage area to disambiguate, navigates upstream, and sums
#' catchment areas. Returns NA with a warning on any failure.
#'
#' @param start_feature sf data.frame. Resolved start feature with geometry.
#' @param network_da numeric. NHDPlusV2 network total drainage area (sq km)
#'   used to match the best HR flowline.
#' @param hu12_polys sf data.frame. Largest available HUC12 polygon set used
#'   to define the bounding box for fetching the full HR network.
#' @return list with \code{nhdplushr_network_dasqkm} (numeric or NA).
#' @noRd
get_nhdplushr_da_estimate <- function(start_feature, network_da, hu12_polys) {

  fail <- list(nhdplushr_network_dasqkm = NA_real_)

  tryCatch({

    # 1. extract start point (and full geometry for proximity filtering)
    start_geom <- sf::st_geometry(start_feature)
    is_point <- inherits(start_geom, "sfc_POINT")
    start_point <- if(is_point) start_geom else sf::st_centroid(start_geom)

    # 2. bounding box from HUC polygons + start geometry buffer for full HR network
    start_geom_5070 <- sf::st_transform(start_geom, 5070)
    start_buf <- sf::st_buffer(start_geom_5070, 500)
    aoi <- sf::st_union(
      sf::st_transform(sf::st_as_sfc(sf::st_bbox(hu12_polys)), 5070),
      start_buf
    )
    aoi <- sf::st_as_sfc(sf::st_bbox(sf::st_transform(aoi, 4326)))

    message("  Fetching NHDPlusHR network for full AOI...")
    hr_network <- get_nhdphr(AOI = aoi, type = "networknhdflowline")

    if(is.null(hr_network) || nrow(hr_network) == 0)
      stop("no HR flowlines in AOI")

    message("  Fetched ", nrow(hr_network), " HR flowlines")

    # 3. find HR outlet flowline(s) to start navigation from
    if(!is_point) {
      # waterbody: find HR flowlines within 500m of the lake polygon,
      # then identify outlets where tonode is not in fromnode
      wb_5070 <- sf::st_transform(start_geom, 5070)
      wb_buf <- sf::st_buffer(wb_5070, 500)
      hr_5070 <- sf::st_transform(hr_network, 5070)
      dists <- as.numeric(sf::st_distance(hr_5070, wb_buf))
      hr_wb <- hr_network[dists <= 0, ]

      if(nrow(hr_wb) == 0)
        stop("no HR flowlines within 500m of waterbody")

      message("  ", nrow(hr_wb),
        " HR flowlines within 500m of waterbody polygon")

      hr_wb_outlets <- hr_wb[
        !hr_wb$tonode %in% hr_wb$fromnode, ]

      if(nrow(hr_wb_outlets) == 0) hr_wb_outlets <- hr_wb

      hr_start_ids <- hr_wb_outlets$nhdplusid

      message("  Found ", length(hr_start_ids), " HR outlet flowline(s)")

    } else {
      # point: index to nearby HR flowlines, disambiguate by DA
      start_5070 <- sf::st_transform(start_point, 5070)
      hr_5070 <- sf::st_transform(hr_network, 5070)
      start_buf <- sf::st_buffer(start_5070, 500)
      dists <- as.numeric(sf::st_distance(hr_5070, start_buf))
      hr_local <- hr_network[dists <= 0, ]

      if(nrow(hr_local) == 0)
        stop("no HR flowlines within 500m of start feature")

      indexes <- hydroloom::index_points_to_lines(
        hr_local, start_5070,
        search_radius = units::set_units(500, "m"),
        max_matches = 10
      )

      if(is.null(indexes) || nrow(indexes) == 0)
        stop("no HR index matches found")

      best <- hydroloom::disambiguate_indexes(
        indexes,
        sf::st_drop_geometry(hr_local[, c("nhdplusid", "totdasqkm")]),
        data.frame(point_id = 1, totdasqkm = network_da)
      )
      hr_start_ids <- best$nhdplusid
    }

    for(sid in hr_start_ids) {
      message("  HR outlet NHDPlusID = ",
        format(sid, scientific = FALSE),
        " (TotDASqKM = ",
        hr_network$totdasqkm[hr_network$nhdplusid == sid], ")")
    }

    # 4. navigate upstream from each outlet on HR network
    message("  Navigating upstream on HR network...")
    ut_ids <- unique(unlist(lapply(hr_start_ids, \(sid) {
      hydroloom::navigate_hydro_network(hr_network, sid, mode = "UT")
    })))

    message("  Found ", length(ut_ids), " HR flowlines upstream")

    # 6. fetch HR catchments
    message("  Fetching NHDPlusHR catchments...")
    hr_catchments <- get_nhdphr(
      ids = as.character(ut_ids), type = "nhdpluscatchment"
    )

    if(is.null(hr_catchments) || nrow(hr_catchments) == 0)
      stop("no HR catchments returned")

    # 7. sum catchment areas
    if("areasqkm" %in% names(hr_catchments)) {
      da <- sum(hr_catchments$areasqkm, na.rm = TRUE)
    } else {
      hr_catch_5070 <- sf::st_transform(hr_catchments, 5070)
      da <- sum(as.numeric(sf::st_area(hr_catch_5070))) / 1e6
    }

    list(nhdplushr_network_dasqkm = da)

  }, error = function(e) {
    warning("NHDPlusHR estimate failed: ", conditionMessage(e),
      call. = FALSE)
    fail
  })
}
