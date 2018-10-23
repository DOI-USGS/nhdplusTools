library(testthat)
library("nhdplusTools")
library("sf")
library("dplyr")

test_check("nhdplusTools", filter = "nhdplus")
test_check("nhdplusTools", filter = "collapse")
test_check("nhdplusTools", filter = "get")
test_check("nhdplusTools", filter = "index")
test_check("nhdplusTools", filter = "reconcile")
test_check("nhdplusTools", filter = "refactor")
test_check("nhdplusTools", filter = "subset")
test_check("nhdplusTools", filter = "split")

