nhdplusTools 0.4.3
==========

- New function added: `get_streamlevel()` calculates stream level, a bottom up level path order used by nhdplus to categorize complete river levels.

nhdplusTools 0.4.2
==========
Version 0.4.2 further cleans up temporary and cache data functionality for CRAN policy. 

New functionality was added to indexing functionality. 

* When multiple flowlines are returned, `disambiguate_flowline_indexes()` supports for disambiguating them based on numeric or text attributes that should match. This functionality is not heavily tested, but forms a basis for expansion of this functionality going forward.
* A function to find the point location of a flowline index, `get_hydro_location()` was added. It will return point geometry given a geometry identifier and measure along that identifier.

nhdplusTools 0.4.1
==========
Version 0.4.1 is a minor release with bug fixes and updates for CRAN policy.

A noteable addition is handling for strictly dendritic topology encoded using a "tocomid" attribute rather than "fromnode" and "tonode" attributes. This functionality was added in subsetting functions and is backward compatible with previous versions.

nhdplusTools 0.4.0
==========
Version 0.4.0 adds new functionality for data access via web services and updates the usability and flexibility of data discovery and subsetting functions. This release includes significant rework of functions related to network navigation and web-service data subsetting. 

* Previsouly, `subset_nhdplus()` included internal functionality for downloading data via web service. This code for subsetting NHDPlus via web service was completely rewritten and a number of new web-service data access functions were added. The `get_nhdplus()` function has been added and is for web-service data access only. `subset_nhdplus()`, which will output a subset to a local `.gpkg` file, now uses `get_nhdplus()` for download functionality.
* `navigate_nldi()` and most other [NLDI](https://waterdata.usgs.gov/blog/nldi-intro/) functions are now based on an NLDI client in the [`dataRetrieval`](https://code.usgs.gov/water/dataRetrieval) package. Functionality has been maintained backward compatible with `nhdplusTools` v0.3 as much as possible with some minor modifications to accomadate changes in dataRetrieval. The response format from `navigate_nldi()` has been changed slightly in v0.4, it now includes both the origin feature and navigation type(s) as a list. Two un-needed nldi discovery functions were removed.
* Functions to download and cache all NHDPlus VAAs (in a 195mb `.fst` file) for use in network navigation and other applications have been added. These can be used with any nhdplusTools functions that do not require geometry.
* `get_levelpaths()` now has an `override_factor` parameter that will cause stream leveling to follow the weight rather than the name input if the weight is `override_factor` times larger than the name indication.

*Functions Added*
* `get_nhdplus()`
* `get_nhdarea()`
* `get_waterbodies()`
* `get_gagesII()`
* `get_huc12()`
* `get_huc8()`
* `download_vaa()`
* `get_vaa()`
* `get_vaa_names()`
* `get_vaa_path()`

*Functions Removed*
* `discover_nldi_navigation()`
* `discover_nldi_sources()`

*Functions Deprecated*
* `stage_national_data()`

*Functions with Consolodated Internals*
* `discover_nhdplus_id()`
* `get_nldi_basin()`
* `get_nldi_feature()`
* `subset_nhdplus()`
* `plot_nhdplus()`

Finally, the handling of data and caching in the package has been overhauled. These changes are non-functional, but aim to decrease data downloads by caching rather than using temp directories.

nhdplusTools 0.3.16
==========
* `subset_nhdplus()` now validates geometry and ensures all outputs are in NAD83
* `subset_nhdplus()` queries the NHDPlus database rather than loading then filtering
* `get_levelpaths()` rewritten for performance. Package now uses `data.table`.

nhdplusTools 0.3.15
==========
* Added `discover_nldi_characteristics()` and `get_nldi_characteristics()`
* Changed `navigate_nldi()` to use the new NLDI navigation end point. Distance is now required.
* Fixed a bug in `get_flowline_index()` to handle multipart lines.
* Added flowline_only input to `plot_nhdplus()` to improve scalability
* Added streamorder filtering to `subset_nhdplus()` for download filtering.
* Default behavior of `plot_nhdplus()` updated to improve scalability of large downloads.

nhdplusTools 0.3.14
==========
* Added `get_node()` to get top and bottom of a flowline.
* Switched geospatial data dependency to use labs.waterdata.usgs.gov
* Added `subset_rpu()` to enable subsetting the NHD by Raster Processing Unit
* Added `get_waterbody_index()` to find associations between point locations and waterbodys.
* Added ability to get multiple matches from `get_flowline_index()` with `max_matches` parameter.
* Added ability to download flowlines in `get_flowline_index()` rather than requiring data to be loaded locally.

nhdplusTools 0.3.13
==========
* tested with dplyr 1.0 and sf 0.9. 

nhdplusTools 0.3.12
==========
* Added `plot_nhdplus()` with both outlet-based and bbox based data subsetting as well as rudimentary style modification capabilities.
* Implemented stream order and Pfafstetter code generators (`get_streamorder()` and `get_pfaf()`) for dendritic networks.
* `subset_nhdplus()` can now return data without writing a file to disk.
* NLDI feature specification is more flexible, doesn't require names anymore.
* `get_nhdplushr()` has been updated to modify terminal path and other identifiers making nhdplusHR subsets "stand alone". A function `make_standalone()` is now exported.
* Added `get_terminal()` and `get_pathlength()` functions to generate nhdplus network attributes.

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
