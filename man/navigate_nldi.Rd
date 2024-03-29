% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nldi.R
\name{navigate_nldi}
\alias{navigate_nldi}
\title{Navigate NLDI}
\usage{
navigate_nldi(
  nldi_feature,
  mode = "upstreamMain",
  data_source = "flowlines",
  distance_km = 10
)
}
\arguments{
\item{nldi_feature}{list with names `featureSource` and `featureID` where
`featureSource` is derived from the "source" column of  the response of
\link[dataRetrieval]{get_nldi_sources} and the `featureID` is a known identifier
from the specified `featureSource`.}

\item{mode}{character chosen from ("UM", "UT", DM", "DD"). See examples.}

\item{data_source}{character chosen from "source" column of the response
of \link[dataRetrieval]{get_nldi_sources} or empty string for flowline geometry.}

\item{distance_km}{numeric distance in km to stop navigating.}
}
\value{
sf data.frame with result
}
\description{
Navigate the Network Linked Data Index network.
}
\examples{
\donttest{
library(sf)
library(dplyr)

nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05428500")

navigate_nldi(nldi_feature = nldi_nwis,
              mode = "upstreamTributaries")$UT \%>\%
  st_geometry() \%>\%
  plot()

navigate_nldi(nldi_feature = nldi_nwis,
              mode = "UM")$UM \%>\%
  st_geometry() \%>\%
  plot(col = "blue", add = TRUE)



nwissite <- navigate_nldi(nldi_feature = nldi_nwis,
                          mode = "UT",
                          data_source = "nwissite")$UT_nwissite

st_geometry(nwissite) \%>\%
  plot(col = "green", add = TRUE)

nwissite
}

}
