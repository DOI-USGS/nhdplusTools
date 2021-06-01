if(!any(c(
  require(drake, quietly = TRUE),
  require(dplyr, warn.conflicts = FALSE),
  require(nhdplusTools),
  require(EGRET),
  require(dataRetrieval),
  require(intersectr))))
  {
  stop(paste("Please ensure all required packages are installed:",
             "`dataRetrieval`, `nhdplusTools`, `EGRET`, `intersectr`.",
             "See readme for more."))
}

source("R/intersectr.R")
source("R/wb_funs.R")
# https://mikejohnson51.github.io/AOI/

set_precision <- function(x, prec) {
  sf::st_precision(x) = prec
  x
}

sp_bbox <- function(g) {
  matrix(as.numeric(sf::st_bbox(g)),
         nrow = 2, dimnames = list(c("x", "y"),
                                   c("min", "max")))
}

plan <- drake_plan(
  start_location = AOI::geocode(location = "Great Salt Lake", pt = TRUE),
  start_nhdplusid = nhdplusTools::discover_nhdplus_id(start_location$pt$geometry),
  nldi_feature = list(featureSource = "comid",
                      featureID = start_nhdplusid),
  nldi_sources = dataRetrieval::get_nldi_sources(),
  UT = nhdplusTools::navigate_nldi(nldi_feature = nldi_feature,
                                   mode = "upstreamTributaries",
                                   data_source = ""),
  nwissite = nhdplusTools::navigate_nldi(nldi_feature = nldi_feature,
                                         mode = "upstreamTributaries",
                                         data_source = "nwissite"),
  wqpsite = nhdplusTools::navigate_nldi(nldi_feature = nldi_feature,
                                        mode = "upstreamTributaries",
                                        data_source = "wqp"),
  nhdp = nhdplusTools::subset_nhdplus(UT$nhdplus_comid,
                                      output_file = file_out("data/nhdp_subset.gpkg"),
                                      nhdplus_data = "download",
                                      status = TRUE,
                                      overwrite = TRUE),
  nhd_basin = nhdplusTools::get_nldi_basin(nldi_feature),
  all_nhd_fline = sf::read_sf(nhdp, "NHDFlowline_Network"),
  nhd_fline = sf::st_as_sf(dplyr::filter(all_nhd_fline,
                                         ftype != "ArtificialPath")),
  nhd_cat = sf::read_sf(nhdp, "CatchmentSP"),
  nhd_area = sf::read_sf(nhdp, "NHDArea"),
  nhd_wbody = sf::read_sf(nhdp, "NHDWaterbody"),
  nhd_bbox = sp_bbox(sf::st_transform(nhd_fline, 4326)),
  outlet_name = nhd_fline$gnis_name[which(nhd_fline$hydroseq == min(nhd_fline$hydroseq))],
  usgs_sites = gsub(pattern = "USGS-",
                    replacement = "",
                    x = nwissite$identifier),
  whatFlow = dataRetrieval::whatNWISdata(siteNumber = usgs_sites,
                                         parameterCd = "00060",
                                         statCd = "00003",
                                         service="dv") %>%
    filter(end_date >= Sys.Date()-1) %>%
    arrange(desc(count_nu)),
  flowData = dataRetrieval::readNWISdv(siteNumbers = whatFlow$site_no[1], "00060"),
  whatWQ = dataRetrieval::whatNWISdata(siteNumber = usgs_sites,
                                       service = "qw") %>%
    filter(!is.na(parm_cd),
           count_nu > 150),
  Sample = EGRET::readNWISSample(siteNumber = whatFlow$site_no[1], parameterCd = "00095") %>%
    filter(ConcHigh > 10,
           !duplicated(Date)),
  Daily = EGRET::readNWISDaily(siteNumber = whatFlow$site_no[1],
                               startDate = min(Sample$Date),
                               endDate = max(Sample$Date)),
  INFO = EGRET::readNWISInfo(whatFlow$site_no[1], parameterCd = "00095", interactive = FALSE),
  eList = EGRET::mergeReport(INFO = INFO, Daily = Daily, Sample = Sample) %>%
    EGRET::modelEstimation() %>%
    EGRET::blankTime(startBlank = "1991-10-01",
                     endBlank = "2008-01-01"),
  site_points = sf::st_as_sf(whatFlow, coords = c("dec_long_va", "dec_lat_va"),
                              crs = "+init=epsg:4326"),
  indexed_sites = nhdplusTools::get_flowline_index(
    points = sf::st_transform(site_points, ann_prj),
    flines = sf::st_transform(nhdplusTools::align_nhdplus_names(all_nhd_fline), ann_prj),
    search_radius = 100, # stop searching after (m)
    precision = 10), # Densify node geometry to atleast (m)
  simple_site = list(featureSource = "nwissite",
                     featureID = paste0("USGS-", whatFlow$site_no[1])),
  simple_UT = nhdplusTools::navigate_nldi(simple_site, "upstreamTributaries", ""),
  simple_UT_site = nhdplusTools::navigate_nldi(simple_site, "upstreamTributaries", "nwissite"),
  simple_nhdp = nhdplusTools::subset_nhdplus(simple_UT$nhdplus_comid,
                                             output_file = file_out("data/simple_nhdp.gpkg"),
                                             nhdplus_data = "download",
                                             status = TRUE,
                                             overwrite = TRUE),
  flowline = sf::read_sf(simple_nhdp, "NHDFlowline_Network"),
  catchment = sf::st_as_sf(dplyr::select(set_precision(sf::read_sf(simple_nhdp, "CatchmentSP"), 10000),
                                         ID = featureid,
                                         a = areasqkm)),
  boundary = sf::st_sf(ID = simple_site$featureID, sf::st_union(st_geometry(catchment))),
  plot_box = sp_bbox(sf::st_transform(catchment, 4326)),
  ### Intersections with historical weather.
  ann_prj = "+init=epsg:5070", # Albers equal area for CONUS
  buffer_dist = 1000, # units of ann_prj (m)
  # See https://cida.usgs.gov/thredds/ for source server
  # See https://cida.usgs.gov/thredds/dodsC/UofIMETDATA.html for metadata
  gridmet = "https://cida.usgs.gov/thredds/dodsC/UofIMETDATA",
  gridmet_var = "precipitation_amount",
  gridmet_data = run_intersection(gridmet, gridmet_var, catchment, ann_prj,
                                  buffer_dist = buffer_dist, status = TRUE,
                                  start_datetime = "2009-10-01 00:00:00",
                                  end_datetime = "2010-10-01 00:00:00",
                                  return_cell_geometry = TRUE),
  # See https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly.html for metadata
  sseb = "https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly",
  sseb_var = "et",
  sseb_data = run_intersection(sseb, sseb_var, catchment, ann_prj,
                               buffer_dist = buffer_dist, status = TRUE,
                               start_datetime = "2009-10-01 00:00:00",
                               end_datetime = "2010-10-01 00:00:00",
                               return_cell_geometry = TRUE),
  plot_gridmet = dplyr::left_join(catchment, get_plot_data(gridmet_data$intersection), by = "ID"),
  plot_sseb = dplyr::left_join(catchment, get_plot_data(sseb_data$intersection), by = "ID"),
  wb_summary = get_wb(catchment, flowData, sseb_data, gridmet_data)
)

options(drake_make_menu = FALSE)
make(plan)
