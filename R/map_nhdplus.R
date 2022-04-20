#' Check for a package
#' @param pkg package name
#' @noRd

check_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '",
         pkg,
         "' is required for this functionality, but is not installed. \nTry `install.packages('",
         pkg,
         "')`", call. = FALSE)
}


#' @title Make Interactive Map of NHDPlus
#' @description Given a list of outlets, get their basin boundaries and network and return a leaflet map in
#' EPSG:4326.
#' @inheritParams plot_nhdplus
#' @param return_map if FALSE (default), a data.frame of plot data is returned invisibly in NAD83 Lat/Lon, if TRUE the leaflet object is returned
#' @return data.frame or leaflet map (see return_map)
#' @details map_nhdplus supports several input specifications. An unexported function "as_outlet"
#' is used to convert the outlet formats as described below.
#' \enumerate{
#'   \item if outlets is omitted, the bbox input is required and all nhdplus data
#'   in the bounding box is plotted.
#'   \item If outlets is a list of integers, it is assumed to be NHDPlus IDs (comids)
#'   and all upstream tributaries are plotted.
#'   \item if outlets is an integer vector, it is assumed to be all NHDPlus IDs (comids)
#'   that should be plotted. Allows custom filtering.
#'   \item If outlets is a character vector, it is assumed to be NWIS site ids.
#'   \item if outlets is a list containing only characters, it is assumed to be a list
#'   of nldi features and all upstream tributaries are plotted.
#'   \item if outlets is a data.frame with point geometry, a point in polygon match
#'   is performed and upstream with tributaries from the identified catchments is plotted.
#' }
#'
#' See \link{plot_nhdplus} for details on plot configuration.
#' @export
#' @examples
#' \donttest{
#' map_nhdplus("05428500")
#'
#' map_nhdplus("05428500", streamorder = 2)
#'
#' map_nhdplus(list(13293970, 13293750))
#'
#' source(system.file("extdata/sample_data.R", package = "nhdplusTools"))
#'
#' map_nhdplus(list(13293970, 13293750), streamorder = 3, nhdplus_data = sample_data)
#'
#' #return leaflet object
#' map_nhdplus("05428500", return_map = TRUE)
#' }
#' @importFrom sf st_transform

map_nhdplus <- function(outlets = NULL, bbox = NULL, streamorder = NULL,
                        nhdplus_data = NULL, gpkg = NULL,
                        flowline_only = NULL, plot_config = NULL,
                        overwrite = TRUE, cache_data = NULL, return_map = FALSE) {

  check_pkg("leaflet")

  # I did try gt() and warnings are thrown
  lt <- function(x){ st_transform(x, '+proj=longlat +datum=WGS84') }

  # Work with cache data

 # TODO: fix duplication with plot_nhdplus
  save  <- FALSE
  fetch <- TRUE
  if(!isFALSE(cache_data)) {
    if(!is.null(cache_data)) {
      if(file.exists(cache_data)) {
        pd <- readRDS(cache_data)
        fetch <- FALSE
      } else {
        save <- TRUE
      }
    }
  }

  if(fetch){
    pd <- get_plot_data(outlets, bbox, streamorder,
                        nhdplus_data, gpkg, overwrite, flowline_only)
  }

  if(save) {
    saveRDS(pd, cache_data)
  }

  ########################

  m <- leaflet::leaflet()  %>%
    leaflet::addProviderTiles("Esri.NatGeoWorldMap", group = "Terrain")  %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Grayscale")  %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Imagery")  %>%
    leaflet::addScaleBar("bottomleft")  %>%
    leaflet::addMiniMap(
      toggleDisplay = TRUE,
      minimized = TRUE
    )  %>%
    leaflet::addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green"
    )  %>%
    leaflet::fitBounds(lng1 = pd$plot_bbox[1,1], lng2 = pd$plot_bbox[1,2],
                       lat1 = pd$plot_bbox[2,1], lat2 = pd$plot_bbox[2,2])


  st <- get_styles(plot_config)

  overlays <- NULL

  if(!is.null(pd$basin)){

    m  <- leaflet::addPolygons(m, data = lt(pd$basin),
                              weight = st$basin$lwd + 2,
                              fillColor = "transparent",
                              color = st$basin$border,
                              group = "basins")

    overlays <- c(overlays, "basins")
  }

  if(!is.null(pd$network_wtbd)){

    m  <- leaflet::addPolygons(m, data = lt(pd$network_wtbd),
                              weight = st$network_wtbd$lwd,
                              fillOpacity = .8,
                              fillColor = st$network_wtbd$col,
                              color = st$network_wtbd$border,
                              group = "Network WB")

    overlays <- c(overlays, "network WB")
  }

  if(!is.null(pd$off_network_wtbd)){

    m  <- leaflet::addPolygons(m, data = lt(pd$off_network_wtbd),
                              weight = st$off_network_wtbd$lwd,
                              fillColor = st$off_network_wtbd$col,
                              fillOpacity = .8,
                              color = st$off_network_wtbd$border,
                              group = "Off-Network WB")

    overlays <- c(overlays, "off-network WB")

  }

  if(!is.null(pd$flowline)){

    m <- leaflet::addPolylines(m, data = lt(pd$flowline),
                         weight = st$flowline$lwd + 2,
                         opacity = 1,
                         color = st$flowline$border,
                         label = ~COMID,
                         group = "Flowlines")

    overlays <- c(overlays, "flowlines")
  }

  if(!is.null(pd$outlets)) {

    base <- do.call(rbind.data.frame, st$outlets)
    base$type <- rownames(base)
    pd$outlets$type <- ifelse(pd$outlets$type %in% base$type, pd$outlets$type,"default")

    outs <- left_join(pd$outlets, base, by = "type")

    pal <- leaflet::colorFactor(c(base$col), domain = base$type)

    m <- leaflet::addCircleMarkers(m, data = lt(outs),
                                   fillOpacity = 1,
                                   fillColor    = ~col,
                                   stroke = FALSE,
                                   radius = ~cex*5,
                                   group = "outlets"
                             ) %>%
      leaflet::addLegend(position = 'topright',
                colors = pal(unique(outs$type)), labels = unique(outs$type),
                title = "Outlet Type", opacity = .7
                )

    overlays <- c(overlays, "outlets")
  }

  m <-leaflet::addLayersControl(m,
      baseGroups = c("Grayscale", "Imagery", "Terrain"),
      overlayGroups = overlays,
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )

  print(m)

  if(return_map){
    return(invisible(m))
  } else {
    return(invisible(pd))
  }

}
