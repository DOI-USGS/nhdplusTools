% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_oaproc.R
\name{get_split_catchment}
\alias{get_split_catchment}
\title{Get split catchment}
\usage{
get_split_catchment(point, upstream = TRUE)
}
\arguments{
\item{point}{scf POINT including crs as created by:
\code{sf::st_sfc(sf::st_point(.. ,..), crs)}}

\item{upstream}{logical If TRUE, the entire drainage basin upstream
of the point provided is returned in addition to the local catchment.}
}
\value{
sf data.frame containing the local catchment, the split portion
and optionally the total drainage basin.
}
\description{
Uses catchment splitting web service to retrieve
the portion of a catchment upstream of the point provided.
}
\examples{
\donttest{
point <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)

trace <- get_raindrop_trace(point)

if(inherits(trace, "sf")) {

(snap_point <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]]),
                          crs = 4326))

(catchment <- get_split_catchment(snap_point))

bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)

nhdplusTools::plot_nhdplus(bbox = bbox, cache_data = FALSE)

plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE, col = "white")

(catchment <- get_split_catchment(snap_point, upstream = FALSE))

bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)

nhdplusTools::plot_nhdplus(bbox = bbox, cache_data = FALSE)

plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE, col = "white")

pour_point <- sf::st_sfc(sf::st_point(x = c(-89.25619, 42.98646)), crs = 4326)

(catchment <- get_split_catchment(pour_point, upstream = FALSE))

bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)

nhdplusTools::plot_nhdplus(bbox = bbox, cache_data = FALSE)

plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
plot(sf::st_transform(sf::st_sfc(pour_point, crs = 4326), 3857), add = TRUE, col = "white")
}
}

}
