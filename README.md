# nhdplusTools

[![Build Status](https://travis-ci.org/dblodgett-usgs/nhdplusTools.svg?branch=master)](https://travis-ci.org/dblodgett-usgs/nhdplusTools) [![Coverage Status](https://coveralls.io/repos/github/dblodgett-usgs/nhdplusTools/badge.svg?branch=master)](https://coveralls.io/github/dblodgett-usgs/nhdplusTools?branch=master)

## Tools for Manipulating the NHDPlus Network

This package is a growing collection of tools for manipulation of hydrographic
data that adheres to the NHDPlus data model. It is being developed as part of
an ongoing collaboration between the National Weather Service and the U.S.
Geological Survey between January and October 2018. There is no established
funding or plan to continue development of this package beyond October 2018
but the hope is that this can become a community toolbox for NHDPlus in R.

### Installation:

```
install.packages("devtools")
devtools::install_github("dblodgett-usgs/nhdplusTools")
```

### Data:

The most convenient way to get the NHDPlus is via the [geopackage hosted here.](https://www.epa.gov/waterdata/nhdplus-national-data) [(direct link to download)](https://s3.amazonaws.com/nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_CONUS_Seamless_Geopackage_05.7z) You will need [7z](https://www.7-zip.org/) or the [`archive` package](https://github.com/jimhester/archive) to extract it.

## Package Vision

The `nhdplusTools` package is intended to provide a reusable set of tools to
subset, relate data to, and refactor NHDPlus data. It implements a data model
consistent with both the [NHDPlus](https://www.epa.gov/waterdata/nhdplus-national-hydrography-dataset-plus)
and [HY\_Features](http://opengeospatial.github.io/HY_Features/). The package
aims to provide a simple set of tools with minimal dependencies that can be used
to build sophisticated workflows using NHDPlus data.

**This vision is intended as a guide to contributors -- conveying what kinds of
contributions are of interest to the package's long term vision. It is a
reflection of the current thinking and is open to discussion and modification.**

### Functional Vision
The following describe a vision for the functionality that should be included
in the package in the long run.

##### Subsetting
The NHDPlus is a very large dataset both spatially and in terms of the number
of attributes it contains. Subsetting utilities will provide network location
discovery, network navigation, and data export utilities to generate spatial
and attribute subsets of the NHDPlus dataset.

##### Indexing
One of the most important roles of the NHDPlus is as a connecting network for
ancillary data and models. The first step in any workflow that uses the
network like this is indexing relevant data to the network. A number of methods
for indexing exist, they can be broken into two main categories: linear
referencing and catchment indexing. Both operate on features represented by
points, lines, and polygons. `nhdplusTools` should eventually support both
linear and catchment indexing.

##### Refactoring
The `nhdplusTools` package was started based on a set of tools to refactor the
NHDPlusV2 network. The concept of refactoring as intended here includes:

1) aggregating catchments into groups based on existing network topology,  
2) modifying catchment topology to eliminate small catchments,  
3) splitting large or long catchments to create a more uniform catchment size
distribution.

This type of functionality is especially relevant to modeling applications that
need specific modeling unit characteristics but wish to preserve the network as
much as possible for interoperability with other applications that use the
NHDPlus network.

### Data Model
Given that `nhdplusTools` is focused on working with NHDPlus data, the NHDPlus
data model will largely govern the data model the package is designed to work
with. That said, much of the package functionality also uses concepts from
the HY\_Features standard.  

*Note:* The HY\_Features standard is based on the notion that a "catchment" is a
wholistic feature that can be "realized" (some might say modeled) in a number of
ways. In other words, a catchment can only be fully characterized through a
collection of different conceptual representations. In NHDPlus, the "catchment"
feature is the polygon feature that describes the drainage divide around the
hydrologic unit that contributes surface flow to a given flowline. While this
may seem like a significant difference, in reality, the NHDPlus COMID identifier
lends itself very well to the HY\_Features catchment concept. The COMID is
used as a wholistic identifier for the catchment polygon, the flowline that
connects the catchment inlet and outlet, and value added attributes that
describe characteristics of the catchment's interior. In this way, the COMID
identifier is actually an identifier for a wholistic collection of data that
together fully describe an NHDPlus catchment.

Below is a description of the expected scope of data to be used by the
`nhdplusTools` package. While other data and attributes may come into scope,
it should only be done as a naive pass-through, as in data subsetting, or
with considerable deliberation.

##### Flowlines and Waterbodies
Flowline geometry is a mix of 1-d streams and 1-d "artificial paths". In order
to complete the set of features meant to represent water, we need to include
waterbody and potentially NHDArea polygons (double line stream overlays).

##### Catchment Polygons
Catchment polygons are the result of a complete elevation derived hydrography
process with hydro-enforcement applied with both Watershed Boundary Dataset
Hydrologic Units and NHD reaches.

##### Network Attributes
The NHDPlus includes numerous attributes that are built using the network and
allow a wide array of capabilities that would require excessive iteration or
sophisticated and complex graph-oriented data structures and algorithms.

### Architecture
The NHDPlus is a very large dataset. The architecture of this package as it
relates to handling data and what dependencies are used will be very important.

##### Web vs Local Data
Web services will generally be avoided. However, applications that would require
loading significant amounts of data to perform something that can be
accomplished with a web service very quickly will be considered. Systems like
the [Network Linked Data Index](https://owi.usgs.gov/blog/nldi-intro/) may be
used for data discovery.

##### NHDPlus Version
Initial package development will focus on the [National Seamless NHDPlus](https://www.epa.gov/waterdata/nhdplus-national-data)
database. [NHDPlus High Resolution](https://nhd.usgs.gov/NHDPlus_HR.html) will
be a target for support in the medium to long run.

##### Package Dependencies
If at all possible, dependencies should be available via CRAN, have solid
expected maintenance, allow national-scale analyses, and not require difficult
to install system libraries. `dplyr`, and `sf` are the primary dependencies that
should be used if at all possible.


### Related similar packages:
https://github.com/mbtyers/riverdist  
https://github.com/jsta/nhdR  
https://github.com/lawinslow/hydrolinks  
https://github.com/mikejohnson51/HydroData  
... others -- please suggest additions?

### Contributing:

First, thanks for considering a contribution! I hope to make this package a community created resource
for us all to gain from and won't be able to do that without your help!

1) Contributions should be thoroughly tested with [testthat](https://testthat.r-lib.org/).  
2) Code style should attempt to follow the [tidyverse style guide.](http://style.tidyverse.org/)  
3) Please attempt to describe what you want to do prior to contributing by submitting an issue.  
4) Please follow the typical github [fork - pull-request workflow.](https://gist.github.com/Chaser324/ce0505fbed06b947d962)  
5) Make sure you use roxygen and run Check before contributing. More on this front as the package matures. 

Other notes:
- lintr runs in the tests so... write good code.
- consider running `goodpractice::gp()` on the package before contributing.
- consider running `devtools::spell_check()` if you wrote documentation.
- this package may end up using pkgdown running `pkgdown::build_site()` will refresh it.

## Disclaimer

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey  (USGS), an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](https://creativecommons.org/publicdomain/zero/1.0/)
