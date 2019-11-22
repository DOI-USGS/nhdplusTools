nhdplusTools 0.3.11.9000
==========
0.3.12 is a work in progress. These features are in the works for the next release and are available via `remotes::install_github("USGS-R/nhdplusTools")`

* Added `plot_nhdplus()` with both outlet-based and bbox based data subsetting as well as rudimentary style modification capabilities.
* Implemented stream order and Pfafstetter code generators (`get_streamorder()` and `get_pfaf()`) for dendritic networks.
* `subset_nhdplus()` can now return data without writing a file to disk.
* NLDI feature specification is more flexible, doesn't require names anymore.

nhdplusTools 0.3.11
==========
* Updated link to NHDPlusHR data
* Empty tibble rather than NULL response from empty NLDI result
* Slides from AWRA National 2019, link in README

nhdplusTools 0.3.10
==========
* Added name alignment function
* Added ability to sort response of network navigation

nhdplusTools 0.3.9
==========
* Added downloader functions for more hydrography datasets
* Change navigations to exclude requested catchment

nhdplusTools 0.3.8
==========
* Modified vignettes and examples to avoid web-service calls breaking CRAN build.
* Added error handling to some web service requests.
* Added CITATION and NEWS.

nhdplusTools 0.3.7
==========
* First CRAN release!
