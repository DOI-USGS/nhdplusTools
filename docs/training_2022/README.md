# 2022 nhdplusTools Training

## Session 1: Introduction to nhdplusTools and Mainstems

Agenda: 
- Ice breaker: what phenology did you note this weekend?
- Introductions: What's your domain background? What if any programming do you do?
- Concepts and introduction to background material.
- Coding and interactive demonstration

During the interactive demonstration, participants are encouraged to take notes in their copy of the code. If an RStudio environment with needed dependencies is available, following along and running the code is also encouraged.

Take Aways: 

1. Core Hydrologic Feature Concepts
1. Three nested topologies
1. How these materialize in real data

- nhdplusTools data model and identifier schemes.
  - HY_Catchment, HY_Flowpath, Drainage Basin, Mainstem.
    - [mainstems paper](https://www.sciencedirect.com/science/article/pii/S1364815220309841?via%3Dihub#fig1)
    - [HY_Features](https://docs.opengeospatial.org/is/14-111r6/14-111r6.html#figure5)
  - catchment identifiers and path identifiers.
    - [Advanced Network Attributes vignette](https://usgs-r.github.io/nhdplusTools/articles/advanced_network.html)
  - COMID, nhdplusv2_comid, nhdplushr_nhdplusid, and "ID". And also REACHCODE.
    - qualified identifiers vs unqualified identifiers
    - three distinct types of topology
- nhdplusTools data sources and access patterns
  - web service discovery, local network, remote data
  - NHDPlusHR limitations in contrast to NHDPlusV2
- demo and examples.
  - Load and visualize data.
  - Connecting data as a network!
  
## Session 2: Applications related to hydrologic location and multiple representations of hydrologic features.

Agenda: 
- Hydrologic location background and summary of resources.
- Summary and background resources for retrieving data.
- Interactive code demonstration.

Take Aways:

1. How to use nhdplusTools to perform network navigations
1. How to retrieve data for the network you found
1. Some approaches for crosswalking hydrographic datasets with eachother

- nhdplusTools indexing and hydrologic locations
  - [indexing and referencing vignette](https://usgs-r.github.io/nhdplusTools/articles/indexing.html)
- crosswalking datasets and dissambiguation
  - Reachcode + the network
  - Other strategies - names, drainage area, etc.
- Retrieving additional data for a given domain.
  - [US Data vignette](https://usgs-r.github.io/nhdplusTools/articles/US_data.html)
  - [NLDI Catchment Characteristics](https://waterdata.usgs.gov/blog/nldi_update/)
- mapping and visualization
  - Grouping
  - Categorization

- Demo and examples.
  - Review of session 1 demo.
  - How do we get access to NHDPlusHR data?
  - How do we get access to NHDPlusMR data?
  - How do we relate to NHDPlusHR data?
  - How do we relate to NHDPlusV2 data?
  - How do we get additional data that might be of interest?

Our wish list:

1. Crosswalking our dated version of the NHD (attached is an rds of our NHD clipped to the Delaware Basin portion of PA) to the newer Plus, V2, and HR versions.
    + Our data management structure relies on the COMID in this dated NHD version. We know this is problematic and have been working towards utilizing reachcode and leveraging a feature service, not a static version. But the struggle is real during the transition.
1. Obtaining all  of the following upstream of a segment, point, or within a bounding box:
    + Segments, VAAs, watershed polygon, upstream cumulative land cover
    + This is a crucial part of our workflow that we’re currently stumbling over – quickly joining flowline and catchment attributes to segments due to our dated NHD version.
1. Efficient mapping for displaying various info above


Additional questions we have, if time allows:

1. When to download? Some of the functions require path to be identified and others don’t. I was a bit confused where these data were coming from.
    + get_ function vs subset_ function. Order of operations? I’d often like to subset before get. Only a small fraction of people using the tool will ever need the national seamless all at once…
    + My goal is to pull in clipped (or subset) data quickly, save in a temp file until map and stats are generated and then remove
1. The use of “ID” nomenclature. We’re unsure of differences between COMID, Permanent Identifier, Common Identifier, nhdplusID across versions.
