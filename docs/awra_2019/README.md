# R for Water Resources
### Dave Blodgett and Laura DeCicco - 2019 AWRA National Meeting, 11/4/2019

This directory contains R workflow artifacts and associated slides for a presentation given at the AWRA Annual Meeting in November, 2019 in Salt Lake City, Utah.

[AWRA Slides](https://doi-usgs.github.io/nhdplusTools/awra_2019/#/)

To reproduce this work flow, you should:  

1) Clone this repository locally.  
1) Open the R project in this folder.  
1) Ensure you have all required packages installed:   
   `drake`, `dataRetrieval`, `nhdplusTools`, `EGRET`, `intersectr`.  
   Install with:
   
   ```
   install.packages(c("drake", "dataRetrieval", "nhdplusTools", "EGRET", "remotes"))
   remotes::install_github("doi-usgs/intersectr")
   ```
   
1) Open the `AWRA_National_2019.Rpres` file in Rstudio with this Rproject open and you should have a "preview" button at the top of the page.
1) Open the `get_data.R` script in this folder and run the drake workflow.

The code in `get_data.R` creates a number of drake targets that are subsequently loaded using the function `drake::loadd` in the `AWRA_National_2019.Rpres` file.
