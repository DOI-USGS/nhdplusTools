% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vaa.R
\name{get_characteristics_metadata}
\alias{get_characteristics_metadata}
\title{Get catchment characteristics metadata table}
\usage{
get_characteristics_metadata(search, cache = TRUE)
}
\arguments{
\item{search}{character string of length 1 to free search the metadata table.
If no search term is provided the entire table is returned.}

\item{cache}{logical should cached metadata be used?}
}
\description{
Download and cache table of catchment characteristics.

Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes
for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream
Watersheds for the Conterminous United States (ver. 3.0, January 2021): U.S.
Geological Survey data release, \doi{10.5066/F7765D7V}.
}
\examples{
\donttest{
get_characteristics_metadata()
}
}
