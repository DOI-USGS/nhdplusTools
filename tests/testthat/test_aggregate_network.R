context("aggregate network")

test_that("example runs", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  fline <- dplyr::right_join(dplyr::select(walker_flowline, COMID),
                             suppressWarnings(prepare_nhdplus(walker_flowline, 0, 0, 0, FALSE)))

  fline <- dplyr::select(fline, ID = COMID, toID = toCOMID,
                         LevelPathID = LevelPathI, Hydroseq)

  outlets <- data.frame(ID = c(5329357, 5329317, 5329365, 5329303, 5329435, 5329817),
                        type = c("outlet", "outlet", "outlet", "terminal", "outlet", "outlet"),
                        stringsAsFactors = FALSE)

  aggregated <- aggregate_network(fline, outlets)

  expect_equal(names(aggregated), c("cat_sets", "fline_sets"))

  expect_equal(nrow(aggregated$fline_sets), 12)

  outlets$ID[1] <- 10

  expect_error(aggregate_network(fline, outlets), "Outlet IDs must all be in flowpaths.")

  outlets$ID[1] <- 5329357
  outlets$type[1] <- "terminal"

  expect_error(aggregate_network(fline, outlets), "Terminal paths must have an NA or 0 toID")
})
