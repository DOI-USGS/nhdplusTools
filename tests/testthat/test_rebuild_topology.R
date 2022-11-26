source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

test_that("get_tocomid", {

  tocomid <- get_tocomid(sample_flines)

  expect_equal(nrow(tocomid), nrow(sample_flines))

  expect_true(!any(is.na(tocomid$tocomid)))

  tocomid <- get_tocomid(sample_flines, missing = NA, return_dendritic = FALSE)

  expect_equal(length(tocomid$tocomid), 714)

  expect_equal(sum(tocomid$tocomid == 0), 1)

  expect_error(get_tocomid(dplyr::select(sample_flines, -Divergence)))

  tocomid <- get_tocomid(sample_flines, add = FALSE)

  expect_equal(names(tocomid), c("comid", "tocomid"))
})

test_that("make_node_topology", {
  source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))

  d <- get_tocomid(
    dplyr::select(new_hope_flowline, COMID, FromNode, ToNode, Divergence, FTYPE,
                  AreaSqKM, LENGTHKM, GNIS_ID),
  )

  expect_error(make_node_topology(d), "fromnode or tonode already in data")

  x <- dplyr::select(d, -fromnode, -tonode)

  y <- x
  y$tocomid[1] <- NA
  expect_error(make_node_topology(y), "NA toids found -- must be 0")

  y$tocomid[1] <- 12345
  expect_error(make_node_topology(y), "Not all non zero toids are in ids")

  y <- make_node_topology(x)

  expect_s3_class(y, "sf")

  expect_equal(names(y), c("comid", "tocomid", "fromnode", "tonode",
                           "divergence", "ftype", "areasqkm", "lengthkm",
                           "gnis_id", "geom"))

  y <- make_node_topology(x, add = FALSE)

  expect_s3_class(y, 'data.frame', exact = TRUE)

  expect_equal(names(y), c("comid", "fromnode", "tonode"))

  # we expect the same number of tonodes
  expect_equal(length(unique(new_hope_flowline$ToNode)), length(unique(y$tonode)))

  # we expect more tonodes because we lost divergences.
  expect_equal(length(unique(new_hope_flowline$FromNode)) + sum(new_hope_flowline$Divergence == 2),
               length(unique(y$fromnode)))

  # just the divergences which have unique fromids in x but don't in new hope.
  add_div <- get_tocomid(sf::st_drop_geometry(dplyr::select(new_hope_flowline,
                                                            COMID, FromNode, ToNode)),
                         return_dendritic = FALSE, remove_coastal = FALSE)
  add_div <- add_div[add_div$tocomid %in%
                       new_hope_flowline$COMID[new_hope_flowline$Divergence == 2],]

  y <- make_node_topology(x)

  # we need to get the node the divergences upstream neighbor goes to
  # first get the new outlet nodes for our old ids
  div <- dplyr::left_join(dplyr::select(add_div, comid, tocomid),
                          dplyr::select(y,
                                        comid, tonode), by = "comid")

  # now join upstream renaming the tonode to fromnode
  y <- dplyr::left_join(y, dplyr::select(div, tocomid, new_fromnode = tonode),
                        by = c("comid" = "tocomid"))

  y <- dplyr::mutate(y, fromnode = ifelse(!is.na(new_fromnode),
                                          new_fromnode, fromnode))

  y <- dplyr::select(y, -new_fromnode)

  y <- dplyr::distinct(y)

  expect_equal(y$tonode[y$comid == 8893700], y$fromnode[y$comid == 8893714])
  expect_equal(y$tonode[y$comid == 8893700], y$fromnode[y$comid == 8893706])

  # we would now expect the same number of fromnodes in each network
  expect_equal(length(unique(new_hope_flowline$FromNode)),
               length(unique(y$fromnode)))

  z <- make_node_topology(x, add_div = add_div)

  expect_equal(z$fromnode, y$fromnode)
  expect_equal(z$tonode, y$tonode)

  # the below was used for testing
  # check <- sf::st_drop_geometry(dplyr::left_join(new_hope_flowline, x, by = c("COMID" = "id")))
  # check <- select(check, COMID, FromNode, ToNode, fromnode, tonode)
  #
  # nodes <- data.frame(node = unique(c(check$FromNode, check$ToNode)))
  #
  # nodes <- left_join(nodes,
  #                    dplyr::select(check, fromcomid = COMID, node = ToNode))
  #
  # nodes <- left_join(nodes,
  #                    dplyr::select(check, tocomid = COMID, node = FromNode))
  #
  #
  # n2 <- data.frame(node = unique(c(check$fromnode, check$tonode)))
  #
  # n2 <- left_join(n2,
  #                    dplyr::select(check, fromcomid = COMID, node = tonode))
  #
  # n2 <- left_join(n2,
  #                    dplyr::select(check, tocomid = COMID, node = fromnode))
  #
  # matches <- lapply(1:nrow(n2), function(x) {
  #   any(sapply(1:nrow(nodes), function(y) {
  #     n2$fromcomid[x] == nodes$fromcomid[y] &
  #       n2$tocomid[x] == nodes$tocomid[y]
  #   }))
  # })
  #
  # matches <- as.logical(matches)
  #
  # any(!matches, na.rm = TRUE)

})
