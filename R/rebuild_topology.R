#' Get tocomid
#' @description Given flowlines with fromnode and tonode attributes,
#' will return a toid attribute that is the result of joining
#' tonode and fromnode attributes. In the case that a terminalpa
#' attribute is included, the join is executed by terminalpa group.
#' This is done grouped by terminalpathID because duplicate node
#' ids have been encountered accross basins in some datasets. If
#' `remove_coastal` is `TRUE` (the default) either ftype or fcode are
#' required.
#' @param x data.frame with comid, tonode, fromnode, and (optionally)
#' divergence and terminalpa attributes.
#' @param return_dendritic logical if TRUE, a divergence attribute is required
#' (2 indicates diverted path, 1 is main) and diverted paths will be treated
#' as headwaters. If this is FALSE, the return value is a data.frame including
#' the comid and tocomid attributes.
#' @param missing integer value to use for terminal nodes.
#' @param remove_coastal logical remove coastal features prior to generating
#' tocomid values? ftype or fcode are required if `TRUE`. fcode == 56600 or
#' fcode == "Coastline" will be fremoved.
#' @param add logical if TRUE, a additional data will be added, otherwise
#' a data.frame with only an identifier and new attributes wil be returned.
#' @return data.frame containing comid and tocomid attributes or all
#' attributes provided with comid and tocomid in the first and second columns.
#' @export
#' @examples
#' source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#'
#' tocomid <- get_tocomid(sample_flines)
#'
#' tocomid <- get_tocomid(sample_flines, return_dendritic = FALSE)
#'
get_tocomid <- function(x, return_dendritic = TRUE, missing = 0,
                        remove_coastal = TRUE, add = TRUE) {

  x <- check_names(x, "get_tocomid", tolower = TRUE)

  hy_g <- get_hyg(x, add)

  x <- drop_geometry(x)

  order <- data.frame(comid = x$comid)

  if(remove_coastal)
    x <- filter_coastal(x)

  joiner_fun <- function(x) {
    left_join(x, select(x,
                        tocomid = .data$comid,
                        .data$fromnode),
              by = c("tonode" = "fromnode"))
  }

  if(return_dendritic) {
    if(!"divergence" %in% names(x)) {
      stop("To remove non dendritic paths, a divergence attribute is required.")
    }

    x[["fromnode"]][which(x$divergence == 2)] <- NA

  }

  if("terminalpa" %in% names(x)) {

    x <- group_split(group_by(x, .data$terminalpa))
    x <- bind_rows(lapply(x, joiner_fun))

  } else {

    x <- joiner_fun(x)

  }

  x <- left_join(order, x, by = c("comid"))

  if(!is.na(missing)) {
    x[["tocomid"]] <- tidyr::replace_na(x[["tocomid"]], 0)
  }

  if(add) {

    if(!is.null(hy_g)) {
      x <- sf::st_sf(left_join(x, hy_g, by = "comid"))
    }

    x[ , c("comid", "tocomid",
           names(x)[!names(x) %in% c("comid", "tocomid")])]

  } else {

    as.data.frame(select(x, .data$comid, .data$tocomid))

  }
}

get_hyg <- function(x, add) {
  if(add && inherits(x, "sf")) {
    select(x, .data$comid)
  } else {
    NULL
  }
}

#' get node topology from edge topology
#' @description creates a node topology table from an edge topology
#' @inheritParams get_sorted
#' @inheritParams get_tocomid
#' @return data.frame containing id, fromnode, and tonode attributes or all
#' attributes provided with id, fromnode and tonodde in the first three columns.
#' @export
#' @importFrom dplyr distinct left_join select
#' @examples
#' source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#'
#' x <- get_tocomid(
#'   dplyr::select(new_hope_flowline, COMID, FromNode, ToNode, Divergence, FTYPE,
#'                 AreaSqKM, LENGTHKM, GNIS_ID)
#' )
#'
#' head(make_node_topology(x))

make_node_topology <- function(x, add = TRUE) {

  orig_name <- names(x[1:2])

  hy_g <- get_hyg(x, add)

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

  x <- left_join(x, select(x, id, tonode = fromnode),
                 by = c("toid" = "id"))

  outlets <- x$toid == 0

  x$tonode[outlets] <- seq(max(x$tonode, na.rm = TRUE) + 1,
                           max(x$tonode, na.rm = TRUE) + sum(outlets))

  if(add) {

    if(!is.null(hy_g)) {
      x <- sf::st_sf(left_join(x, hy_g, by = c("id" = "comid")))
    }

    x <- x[ , c("id", "toid", "fromnode", "tonode",
                names(x)[!names(x) %in% c("id", "toid", "fromnode", "tonode")])]

    names(x)[1:2] <- orig_name[1:2]

    x

  } else {

    x <- as.data.frame(select(x, .data$id, .data$fromnode, .data$tonode))

    names(x)[1] <- orig_name[1]

    x

  }
}
