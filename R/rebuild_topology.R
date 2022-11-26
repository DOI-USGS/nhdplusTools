#' @importFrom dplyr all_of
get_hyg <- function(x, add, id = "comid") {
  if(add && inherits(x, "sf")) {
    select(x, all_of(id))
  } else {
    NULL
  }
}

#' get node topology from edge topology
#' @description creates a node topology table from an edge topology
#' @inheritParams get_sorted
#' @inheritParams get_tocomid
#' @param add_div data.frame containing id and toid diverted paths to add.
#' Should have id and toid fields in the first and second columns. Names
#' are not used.
#' @return data.frame containing id, fromnode, and tonode attributes or all
#' attributes provided with id, fromnode and tonodde in the first three columns.
#' @export
#' @importFrom dplyr distinct left_join select
#' @examples
#' source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#'
#' x <- dplyr::select(get_tocomid(
#'   dplyr::select(new_hope_flowline, COMID, FromNode, ToNode, Divergence, FTYPE,
#'                 AreaSqKM, LENGTHKM, GNIS_ID)
#' ), -tonode, -fromnode)
#'
#' head(y <- make_node_topology(x))
#'
#' # just the divergences which have unique fromids in x but don't in new hope.
#' div <- get_tocomid(dplyr::select(new_hope_flowline, COMID, FromNode, ToNode),
#'                    return_dendritic = FALSE,
#'                    remove_coastal = FALSE)
#' div <- div[div$tocomid %in%
#'              new_hope_flowline$COMID[new_hope_flowline$Divergence == 2],]
#'
#' y <- make_node_topology(x, div)
#'

make_node_topology <- function(x, add_div = NULL, add = TRUE) {

  orig_name <- names(x)[1:2]

  hy_g <- get_hyg(x, add, orig_name[1])

  x <- drop_geometry(x)

  if(length(unique(x[, 1])) != nrow(x)) stop("duplicate identifiers found")

  names(x)[1:2] <- c("id", "toid")

  if(any(is.na(x$toid))) stop("NA toids found -- must be 0")
  if(!all(x$toid[x$toid != 0] %in% x$id)) stop("Not all non zero toids are in ids")
  if(any(c("fromnode", "tonode") %in% names(x))) stop("fromnode or tonode already in data")

  order <- data.frame(id = x$id)

  x <- get_sorted(x)

  head_count <- nrow(x)
  head_nodes <- seq_len(head_count)

  x$fromnode <- head_nodes

  x <- left_join(x, select(x, "id", tonode = "fromnode"),
                 by = c("toid" = "id"))

  outlets <- x$toid == 0

  x$tonode[outlets] <- seq(max(x$tonode, na.rm = TRUE) + 1,
                           max(x$tonode, na.rm = TRUE) + sum(outlets))

  if(!is.null(add_div)) {
    # we need to get the node the divergences upstream neighbor goes to
    # first get the new outlet nodes for our old ids
    add_div <- drop_geometry(add_div[, 1:2])
    names(add_div)[1:2] <- c("id", "toid")
    add_div <- left_join(select(add_div, "id", "toid"),
                         select(x, "id", "tonode"), by = "id")

    # now join upstream renaming the tonode to fromnode
    x <- left_join(x, select(add_div, "toid", new_fromnode = "tonode"),
                   by = c("id" = "toid"))

    x <- mutate(x, fromnode = ifelse(!is.na(.data$new_fromnode),
                                     .data$new_fromnode, .data$fromnode))

    x <- select(x, -"new_fromnode")

    x <- distinct(x)
  }

  if(add) {

    if(!is.null(hy_g)) {
      x <- sf::st_sf(left_join(x, hy_g, by = c("id" = "comid")))
    }

    x <- x[ , c("id", "toid", "fromnode", "tonode",
                names(x)[!names(x) %in% c("id", "toid", "fromnode", "tonode")])]

    names(x)[1:2] <- orig_name[1:2]

    x

  } else {

    x <- as.data.frame(select(x, "id", "fromnode", "tonode"))

    names(x)[1] <- orig_name[1]

    x

  }
}
