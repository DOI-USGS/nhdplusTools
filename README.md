# nhdplusTools

[![Build Status](https://travis-ci.org/dblodgett-usgs/nhdplusTools.svg?branch=master)](https://travis-ci.org/dblodgett-usgs/nhdplusTools) [![Coverage Status](https://coveralls.io/repos/github/dblodgett-usgs/nhdplusTools/badge.svg?branch=master)](https://coveralls.io/github/dblodgett-usgs/nhdplusTools?branch=master)

## Tools for Manipulating the NHDPlus Network

This package is a growing collection of tools for manipulation of hydrographic data that adheres to the NHDPlus data model. It is being developed as part of an ongoing collaboration between the National Weather Service and the U.S. Geological Survey between January and October 2018. There is no established funding or plan to continue devlopment of this package beyond October 2018 but the hope is that this can become a community toolbox for NHDPlus in R.

### Installation:

```
install.packages("devtools")
devtools::install_github("dblodgett-usgs/nhdplusTools")
```

### Data:

The most convenient way to get the NHDPlus is via the [geopackage hosted here.](Nhttps://www.epa.gov/waterdata/nhdplus-national-data) [(direct link to download)](https://s3.amazonaws.com/nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_CONUS_Seamless_Geopackage_05.7z) You will need [7z](https://www.7-zip.org/) or the [`archive` package](https://github.com/jimhester/archive) to extract it.

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
