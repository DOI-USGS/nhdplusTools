nhdplusTools 1.1.0
==========

This release has a significant consolidation of code to work with web services.
It incorporates an ArcGIS REST service for the first time. It has very few 
backward compatibility issues with previous versions and introduces new functions
and behavior.

A key change in behavior is the introduction of caching with the `memoise` package.
See #366, pull request #364, and documentation in `nhdplusTools_cache_settings()`
for more.

- fixed nhd and nhdplushr urls #368
- fix issue with nhdplusTools_data_dir() #365
- fixed bug with large nhdplus downloads with empty tiles. #361
- Added 3DHP_all service client. #363
- Removed deprecated function `get_huc12` and `get_huc8`
- Updated documentation of `get_huc()` and other web service functions.
- added `nhdplusTools_cache_settings()` to control use of a `memoise` cache. #366
- fix minor bug related to binding nhdplushr data together. #380


nhdplusTools 1.0.0
==========

`nhdplusTools` has been split into two packages. A new package [`hydroloom`](https://github.com/DOI-USGS/hydroloom) is now available. It contains all nhdplusTools functionality not related to specific dataset schemas. See [this issue](https://github.com/DOI-USGS/nhdplusTools/issues/307) for details.

## `hydroloom` 

v1.0.0 should be backward compatible with v0.7 but deprecation warnings have been added and one previously deprecated function has been removed. 

This release evolves `nhdplusTools` to use [`hydroloom`](https://doi-usgs.github.io/hydroloom/articles/hydroloom.html) as a core dependency. `nhdplusTools` now depends on `hydroloom` for all network navigation and indexing functionality that is not specific to the nhdplus data model or specific to a US-context.

Specific functions imported from [`hydroloom`](https://doi-usgs.github.io/hydroloom/articles/hydroloom.html) include:

- hy
- accumulate_downstream
- add_levelpaths
- add_pathlength
- add_pfafstetter
- add_streamlevel
- add_streamorder
- add_toids
- disambiguate_indexes
- fix_flowdir
- get_hydro_location
- get_node
- get_partial_length
- index_points_to_lines
- index_points_to_waterbodies
- make_fromids
- make_index_ids
- navigate_connected_paths
- navigate_hydro_network
- rename_geometry
- rescale_measures
- sort_network
- st_compatibalize

[See the `hydroloom` reference list for details about these functions.](https://doi-usgs.github.io/hydroloom/reference/index.html)

With these functions migrated to `hydroloom`, a number of `nhdplusTools` functions are now deprecated in favor of the `hyroloom` implementation.

- `make_node_topology()`
- `get_pfaf()`
- `get_path_members()`
- `get_path_lengths()`
- `get_levelpaths()`
- `get_terminal()`
- `make_node_topology()`

In v1.0.0, the `download_nhd()` function was added.

In v1.0.0, the `stage_national_data` (previously deprecated) function was removed.

nhdplusTools 0.7.1
==========
This release fixes a minor example issue for CRAN checks

nhdplusTools 0.7.0
==========
This release adds three new functions.

- `get_characteristics_metadata()`
- `get_catchment_characteristics()`
- `rescale_catchment_characteristics()`

These functions are designed to provide access to:

Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes
for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream
Watersheds for the Conterminous United States (ver. 3.0, January 2021): U.S.
Geological Survey data release, \doi{10.5066/F7765D7V}.

See https://github.com/DOI-USGS/nhdplusTools/issues/304 and 
https://github.com/DOI-USGS/nhdplusTools/issues/303 for details.

nhdplusTools 0.6.2
==========
Update a test for a change in sf

nhdplusTools 0.6.1
==========
No functional changes. Package migrated to new repository: https://github.com/DOI-USGS/nhdplusTools

nhdplusTools 0.6.0
==========

- minor change to behavior of `navigate_nldi` the "origin" will always be named 
as such rather than the named 1-element list being dropped to a data.frame. 
`https://github.com/DOI-USGS/dataRetrieval/issues/623`
- Address scalability issue with web service downloads. #308
- Add ability to add node topology back to a edge list topology. #302

nhdplusTools 0.5.7
==========

- Update reference for updated flowline attributes.
- Fix failing example for cran checks.

nhdplusTools 0.5.6
==========

- Update URL for NHDPlusV2 download
- make geometry validity checks more robust

nhdplusTools 0.5.4 & v0.5.5
==========

Note v0.5.4 was burned in a CRAN release mistake

fixes a cran test failure in an example and an issue with precision of `get_flowline_index()`

nhdplusTools 0.5.3
==========

v0.5.3 is primarily bug fixes and enhancements.

One notable addition is the addition of the `rpu_boundaries` and `vpu_boundaries` datasets which provide simplified polygons around the RPU and VPU subset domains of the NHDPlusV2 dataset.


nhdplusTools 0.5.2
==========

[Mike Johnson](https://mikejohnson51.github.io/) is now recognized as an author of `nhdplusTools`.

New citation:

```
> citation("nhdplusTools")

To cite nhdplusTools in publications, please use:

  Blodgett, D., Johnson, J.M., 2022, nhdplusTools: Tools for Accessing and
  Working with the NHDPlus, https://doi.org/10.5066/P97AS8JD
```

## New Functions
- `map_nhdplus()` works much the same as `plot_nhdplus()` but creates a leaflet map rather than a static plot.
- `get_nldi_index()` is used to call the NLDI web service to retrieve a hydrologic location.

## Improvements
- `get_flowline_index()` and `get_waterbody_index()` units and projection handling was altered. Defaults for the `search_radius` input were changed. Coordinate reference system handling in geoprocessing now relies on the CRS of the provided points rather than the flowlines.
- `get_nldi_basin()` now supports precise basin delineation via a logical "split" parameter as well as the option to retrieve un-simplified basin geometry.
- All network-dependent functions in `nhdplusTools` now return a warning and NULL if the web service is unavailable or fails for other reasons. This is according to CRAN policy.


nhdplusTools 0.5.1
==========

This is a very minor release required by CRAN checks.

Some scalability issues in `plot_nhdplus()` were identified and fixed.

nhdplusTools 0.5.0
==========

## Documentation updates
- A new "advanced network attributes" vignette was added.
- All vignettes were revisited to ensure they are up to date.
- The introduction vignette is now very brief with links to others.

## Web Service Wrapper Functions
- New functions `get_raindrop_trace()`, `get_split_catchment()` use a web service to access elevation data and retrieve a raindrop trace path to the nearest flowline in nhdplus V2 and a split catchment or basin to a precise point location respectively.
- New functions `get_xs_point()`, `get_xs_points()`, and `get_elev_along_path()` access an elevation data service from the 3D Elevation Program and return cross section / path elevation data. 

## Utility Functions
- New functions `st_compatibalize()` and `rename_geometry()` provide utility functionality to make two layers compatible, including projection and rename geometry columns respectively.

- New function `get_sorted()` generates a sorted and optionally partitioned version of a dendritic tree.

## Subsetting and data utilities
- New function `subset_vpu()` wraps `subset_rpu()` and will subset a vector processing unit.
- New function `fix_flowdir()` will re-order geometry nodes such that their order corresponds to the convention used in nhdplus data.
- `get_vaa()` will now return an updated nhdplus network attribute set derived from multiple improved sources.

## Network Navigation and Attributes
- New function `navigate_network()` provides a wrapper around a number of network navigation capabilities and will support local or web service data.
- New function `get_tocomid()` encapsulates a suite of functionality for converting edge node network representations to edge list format.
- New function `get_path_lengths()` finds network distances between pairs of points.
- New function `get_partial_length()` finds flowpath length up and downstream of a hydrologic location.

## Plotting
- `plot_nhdplus()` now includes on network and off network waterbodies. 
- New function `get_wb_outlet()` finds the outlet flowline of an on-network waterbody. This function works for both NHDPlusV2 and hires.

## Package Internals
- nhdplusTools testing has been updated to use `testthat` edition 3.
- All terminal "toID" values are 0 rather than NA to avoid confusion with a mix of both.
- `igraph` package is no longer a dependency. All graph algorithms have been implemented in base R for performance and simplicity.
- Many package functions have been converted to use lowercase attributes. Attribute naming is settling into two modes -- ID, toID, and lowercase otherwise -- and comid, tocomid, and lowercase. The ID syntax is meant to be a temporary ID while comid is meant to be some external identifier that should be treated as such.
- The master branch was renamed main.


nhdplusTools 0.4.3
==========

- New function added: `get_streamlevel()` calculates stream level, a bottom up level path order used by nhdplus to categorize complete river levels.

- nhdplusTools internals have started to be refactored to use all lower case attribute names. No changes should be seen from outside the package, but please [report issues](https://github.com/DOI-USGS/nhdplusTools/issues) with attribute naming.
- Package testing now runs in parallel and uses revision 3 of the `testthat` package.

nhdplusTools 0.4.2
==========
Version 0.4.2 further cleans up temporary and cache data functionality for CRAN policy. 

New functionality was added to indexing functionality. 

* When multiple flowlines are returned, `disambiguate_flowline_indexes()` supports for disambiguating them based on numeric or text attributes that should match. This functionality is not heavily tested, but forms a basis for expansion of this functionality going forward.
* A function to find the point location of a flowline index, `get_hydro_location()` was added. It will return point geometry given a geometry identifier and measure along that identifier.

nhdplusTools 0.4.1
==========
Version 0.4.1 is a minor release with bug fixes and updates for CRAN policy.

A notable addition is handling for strictly dendritic topology encoded using a "tocomid" attribute rather than "fromnode" and "tonode" attributes. This functionality was added in subsetting functions and is backward compatible with previous versions.

nhdplusTools 0.4.0
==========
Version 0.4.0 adds new functionality for data access via web services and updates the usability and flexibility of data discovery and subsetting functions. This release includes significant rework of functions related to network navigation and web-service data subsetting. 

* Previously, `subset_nhdplus()` included internal functionality for downloading data via web service. This code for subsetting NHDPlus via web service was completely rewritten and a number of new web-service data access functions were added. The `get_nhdplus()` function has been added and is for web-service data access only. `subset_nhdplus()`, which will output a subset to a local `.gpkg` file, now uses `get_nhdplus()` for download functionality.
* `navigate_nldi()` and most other [NLDI](https://waterdata.usgs.gov/blog/nldi-intro/) functions are now based on an NLDI client in the [`dataRetrieval`](https://code.usgs.gov/water/dataRetrieval) package. Functionality has been maintained backward compatible with `nhdplusTools` v0.3 as much as possible with some minor modifications to accommodate changes in dataRetrieval. The response format from `navigate_nldi()` has been changed slightly in v0.4, it now includes both the origin feature and navigation type(s) as a list. Two un-needed nldi discovery functions were removed.
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

*Functions with Consolidated Internals*
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
* Added `get_waterbody_index()` to find associations between point locations and waterbodies.
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
