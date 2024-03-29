% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/A_nhdplusTools.R
\name{nhdplusTools_cache_settings}
\alias{nhdplusTools_cache_settings}
\title{nhdplusTools cache settings}
\usage{
nhdplusTools_cache_settings(mode = NULL, timeout = NULL)
}
\arguments{
\item{mode}{character 'memory' or 'filesystem'}

\item{timeout}{numeric number of seconds until caches invalidate}
}
\value{
list containing settings at time of calling. If inputs are
NULL, current settings. If settings are altered, previous setting values.
}
\description{
Provides an interface to adjust nhdplusTools `memoise` cache.

Mode and timeout can also be set using environment variables.
`NHDPLUSTOOLS_MEMOISE_CACHE` and `NHDPLUSTOOLS_MEMOISE_TIMEOUT` are
used unless overriden with this function.
}
