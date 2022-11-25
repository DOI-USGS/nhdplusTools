#' make index ids
#' @description makes index ids for the provided hy object. These can be used
#' for graph traversal algorithms such that the row number and id are equal.
#' @param x hy data.frame
#' @importFrom dplyr rename left_join
#' @noRd
#' @examples
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x)
#'
#' index_ids <- make_index_ids(x)
make_index_ids <- function(x) {

  if(any(duplicated(x$id))) {
    out <- data.frame(id = unique(x$id),
                      indid = seq(1, length(unique(x$id))))

    out <- left_join(left_join(select(x, "id", "toid"),
                               out, by = "id"),
                     rename(out, toindid = "indid"),
                     by = c("toid" = "id"))

    out$toindid <- tidyr::replace_na(out$toindid, 0)

    out <- select(out, -"id", -"toid")

  } else {
    out <- data.frame(indid = seq(1, nrow(x)))

    out$toindid <- match(x$toid, x$id, nomatch = 0)
  }

  out

}

make_fromids <- function(index_ids, return_list = FALSE) {

  froms <- left_join(select(index_ids, "indid"),
                     select(index_ids, indid = "toindid", fromindid = "indid"),
                     by = "indid")

  froms <- data.frame(indid = unique(froms$indid),
                      fromindid = I(split(froms$fromindid, froms$indid)))

  # slightly faster but requires data.table
  # index_ids <- as.data.table(index_ids)
  #
  # froms <- merge(
  #   index_ids[,list(indid)],
  #   data.table::setnames(index_ids, c("toindid", "indid"), c("indid", "fromindid")),
  #   by = "indid", all.x = TRUE
  # )
  #
  # froms <- froms[,list(froms = list(c(fromindid))), by = indid]


  froms_l <- lengths(froms$fromindid)
  max_from <- max(froms_l)

  # Convert list to matrix with NA fill
  froms_m <- as.matrix(sapply(froms$fromindid, '[', seq(max_from)))

  # NAs should be length 0
  froms_l[is.na(froms_m[1, ])] <- 0

  if(return_list) return(list(froms = froms_m, lengths = froms_l,
                              froms_list = froms))

  return(list(froms = froms_m, lengths = froms_l))

}

#' add toids
#' @description Given an hy object with fromnode and tonode attributes,
#' will return a toid attribute that is the result of joining
#' tonode and fromnode attributes.
#' @param x hy object without a toid attribute
#' @param return_dendritic logical remove non dendritic paths if TRUE. Requires
#' a "divergence" flag where 1 is main and 2 is secondary.
#' @return hy object with toid attribute
#' @importFrom dplyr left_join
#' @noRd
#' @examples
#' x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
#'
#' x <- add_toids(x)
#'
add_toids <- function(x, return_dendritic = TRUE) {

  joiner_fun <- function(x) {
    left_join(x, select(drop_geometry(x),
                        toid = "id",
                        "fromnode"),
              by = c("tonode" = "fromnode"))
  }

  order <- data.frame(id = x$id)

  if(return_dendritic) {
    if(!"divergence" %in% names(x)) {
      stop("To remove non dendritic paths, a divergence attribute is required.")
    }

    x$fromnode[which(x$divergence == 2)] <- NA

  }

  x <- joiner_fun(x)

  x <- left_join(order, x, by = c("id"))

  x$toid <- tidyr::replace_na(x$toid, 0)


  x[ , c("id", "toid",
         names(x)[!names(x) %in% c("id", "toid")])]

}
