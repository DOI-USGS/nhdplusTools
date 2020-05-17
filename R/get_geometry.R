#' @title  Get flowline node
#' @description Given one or more flowlines, returns
#'   a particular node from the flowline.
#' @param x sf data.frame with one or more flowlines
#' @param position character either "start" or "end"
#' @export
#' @importFrom sf st_crs st_coordinates st_as_sf
#' @importFrom dplyr select group_by filter row_number n ungroup
#' @examples
#'
#' fline <- sf::read_sf(system.file("extdata/sample_natseamless.gpkg",
#'                              package = "nhdplusTools"),
#'                  "NHDFlowline_Network")
#' start <- get_node(fline, "start")
#' end <- get_node(fline, "end")
#'
#' plot(sf::st_zm(fline$geom),
#'      lwd = fline$StreamOrde, col = "blue")
#' plot(sf::st_geometry(start), add = TRUE)
#'
#' plot(sf::st_zm(fline$geom),
#'      lwd = fline$StreamOrde, col = "blue")
#' plot(sf::st_geometry(end), add = TRUE)
#'
get_node <- function(x, position = "end") {
  in_crs <- st_crs(x)

  x <- x %>%
    st_coordinates() %>%
    as.data.frame()

  if("L2" %in% names(x)) {
    x <- group_by(x, .data$L2)
  } else {
    x <- group_by(x, .data$L1)
  }

  if(position == "end") {
    x <- filter(x, row_number() == n())
  } else if(position == "start") {
    x <- filter(x, row_number() == 1)
  }

  x <- dplyr::select(ungroup(x), .data$X, .data$Y)

  st_as_sf(x, coords = c("X", "Y"), crs = in_crs)
}
