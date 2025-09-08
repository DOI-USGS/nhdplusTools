get_features_paging <- function(base_call, limit = 1000, status = TRUE) {

  if(!grepl("\\?", base_call)) {
    base_call <- paste0(base_call, "?")
  } else {
    base_call <- paste0(base_call, "&")
  }

  offset <- 0

  keep_going <- TRUE

  if(status) message("Starting download of first set of features.")

  out <- rep(list(list()), 1e6)
  i <- 1

  while(keep_going) {
    req <- paste0(base_call, "limit=", limit, "&offset=", offset)

    out[[i]] <- try(read_sf(req))

    if(inherits(out[[i]], "sf") & nrow(out[[i]]) == limit) {
      offset <- offset + limit
    }

    if(nrow(out[[i]]) < limit) keep_going <- FALSE

    if(!inherits(out[[i]], "sf")) {
      warning("Something went wrong requesting data.")
      keep_going <- FALSE
    }

    if(status & keep_going) message("Starting next download from ", offset, ".")

    i <- i + 1
  }

  out <- out[1:(i - 1)]

  sf::st_sf(dplyr::bind_rows(unify_types(out)))
}

unify_types <- function(out) {
  all_class <- bind_rows(lapply(out, function(x) {
    vapply(x, function(x) class(x)[1], character(1))
  }))

  set_type <- function(out, n, type) {
    lapply(out, function(x) {
      x[[n]] <- as(x[[n]], type)
      x
    })
  }

  for(n in names(all_class)) {
    if(length(unique(all_class[[n]])) > 1) {
      if("numeric" %in% all_class[[n]]) { # prefer numeric
        out <- set_type(out, n, "numeric")
      } else if("integer" %in% all_class[[n]]) { # then integer
        out <- set_type(out, n, "integer")
      } else if("cheracter" %in% all_class[[n]]) {
        out <- set_type(out, n, "character")
      }
    }
  }

  rows <- sapply(out, nrow)

  if(any(rows > 0))
    out <- out[rows > 0]

  out
}
