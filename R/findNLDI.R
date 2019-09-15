#' @title Find Linked Hydro Data via the NLDI
#'
#' @description This function provides a formal query to the Network Linked Data Index (NLDI).
#' There are three tasks that can be accomplished (1) feature discovery (2) NHD Network Tracing (3) Network entity discovery.
#' One parameter ('comid', 'nwis', 'np_des', or 'wqp') must be supplied to indicate where to start on NHD
#' The 'nav' parameter describes what direction the network should be traced from the starting point (UT = Upper Tributary, UM = Upper Mainsteam, DD = Down Stream Diversion, DM = Down Stream Mainstem)
#' Finally, the 'find' parameter, describes what features should be retunred. The 'km' parameter can limit the distance the network is traced.
#'
#' @param comid an NHD COMID
#' @param nwis  a USGS NWIS station ID
#' @param huc12 a HUC12 ID
#' @param npdes_rad Facilities that Discharge to Water NHDPlus Indexed Dataset (ID)
#' @param wqp a water quality portal location ID
#' @param nav where to navigate from the starting point
#' @param find what resources to find along the navigation path ("comid", "huc12pp", "npdes_rad", "nwis", "wqp", "basin")
#' @param distance_km how far to look along the navigation path (in kilometers)
#' @return a list of sf features
#' @importFrom httr RETRY
#' @importFrom sf read_sf
#' @export
#' @examples
#' \donttest{
#' # Starting at NWIS 05428500, navigate along the upper mainsteam, and upper tributary
#' #to find the basin and NWIS sites:
#'
#'  nldi = findNLDI(nwis = "05428500", nav = c("UT", "UM", "DM"), find = c("nwis", "basin"))
#'
#'  plot(nldi$basin$geometry, lwd = 2, col = NA, border = 'black')
#'  plot(nldi$UT$geometry, col = 'blue', add = T)
#'  plot(nldi$DM$geometry, col = 'purple', add = T)
#'  plot(nldi$UM$geometry, col = 'red', lwd = 3, add = T)
#'  plot(nldi$nwissite$geometry, pch = 16, col = 'darkgreen', add = T)
#' }

findNLDI = function(comid = NULL, nwis = NULL, huc12 = NULL, npdes_rad = NULL, wqp = NULL,
                    nav = NULL, find = NULL, distance_km = NULL) {

  get_nldi = function(url){
    c <- rawToChar(httr::RETRY("GET", url, times = 10, pause_cap = 240)$content)
    if (nchar(c) == 0) { c = NULL } else { sf::read_sf(c) }
  }

  tmp = find %in% c("comid", "huc12", "npdes_rad", "nwis", "wqp", 'basin')
  bad.request = find[!tmp]

  if(!all(tmp)){
    stop(paste(paste(bad.request, sep = ", "),  "not an available resource."))
  }

  if('basin' %in% find){ b = T; find = find[-which(find == "basin")] } else { b = F }
  if('nwis' %in%  find){ find[which(find == "nwis")]  = "nwissite"}
  if('huc12' %in% find){ find[which(find == "huc12")] = "huc12pp" }

  start <- navigate2 <- navigate <- features <- basin <- list()

  if (sum(!is.null(comid), !is.null(nwis), !is.null(huc12), !is.null(wqp), !is.null(npdes_rad)) != 1) {
    stop("Define a single starting point. Use `find` to identify other resources.")
  }

  # Define starting point
  if (!is.null(comid))     { start[[1]] = paste0("https://cida.usgs.gov/nldi/comid/",         comid) }
  if (!is.null(nwis))      { start[[1]] = paste0("https://cida.usgs.gov/nldi/nwissite/USGS-", nwis) }
  if (!is.null(huc12))     { start[[1]] = paste0("https://cida.usgs.gov/nldi/huc12pp/",       huc12) }
  if (!is.null(npdes_rad)) { start[[1]] = paste0("https://cida.usgs.gov/nldi/npdes_rad",      npdes_rad) }
  if (!is.null(wqp))       { start[[1]] = paste0("https://cida.usgs.gov/nldi/wqp",            wqp) }


  for (i in seq_along(nav)) {
    if (nav[i] %in% c("UM", "UT", "DD", "DM")) {
      navigate[[nav[i]]] = paste0(start[[1]], "/navigate/", nav[i])
    }
  }

  nav2 = nav

  if(sum(c("UT", "UM") %in% nav2) == 2 ) {nav2[which(nav2 == "UT")] = "UM"}
  if(sum(c("DD", "DT") %in% nav2) == 2 ) {nav2[which(nav2 == "DT")] = "DD"}

  nav2 = unique(nav2)

  for (i in seq_along(nav2)) {
    if (nav2[i] %in% c("UM", "UT", "DD", "DM")) {
      navigate2[[nav2[i]]] = paste0(start[[1]], "/navigate/", nav2[i])
    }
  }

  if(b){ basin[[1]] = paste0(start[[1]], "/basin") }

  if(length(find) > 0) { features = lapply(navigate2, paste0, paste0("/", find)) }

  names(features) = rep(find, each = length(nav2))

  if(!is.null(distance_km)){ features = lapply(features, paste0, paste0("?distance=", distance_km))}

  ll = as.list(unlist(c(start, basin, navigate, features)))

  shp = lapply(ll, get_nldi)

  names(shp) = c("site", if(b){'basin'}, names(navigate), names(features))

  name = unique(names(shp))

  all = list()

  for(i in 1:length(name)){
    index = shp[which(names(shp) == name[i])]
    all[[name[i]]] = do.call(rbind, index)
  }

  return(shp)
}


