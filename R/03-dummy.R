#' Create dummy matrix
#'
#' @param names list with name vectors
#' @param current current matrix list
createDummyMatrix <- function(names, current = NULL) {
  ll <- length(names)
  stopifnot(ll > 1)

  default <- emptyMatrix(names[[ll - 1]], names[[ll]])

  createDummyList(names[1:(ll - 2)], default, current = current)
}

createDummyList <- function(names, default = NA, current = NULL) {
  if (length(names) == 0) {
    return(default)
  }

  res <- list()
  # use a loop because we need to manipulate res
  for (n in seq_along(names)) {
    indices <- as.matrix(expand.grid(names[1:n]))

    value <- if (n == length(names)) {
      default
    } else {
      list()
    }

    for (i in seq_len(nrow(indices))) {
      setList(res, indices[i, ], value)
    }
  }

  ## handle switch of index type
  oldTypes <- getIndexTypes(current)
  newTypes <- getIndexTypes(res)

  if (!is.null(current) && length(oldTypes) == length(newTypes)) {
    changed <- oldTypes != newTypes

    indicesNew <- as.matrix(expand.grid(names))
    indicesOld <- indicesNew
    indicesOld[, changed] <- NA

    for (i in seq_len(nrow(indices))) {
      value <- getList(current, indicesOld[i, ])
      if (is.null(value) || length(value) == 0) value <- default
      setList(res, indices[i, ], value)
    }
  }

  res
}

getIndexTypes <- function(data) {
  if (is.list(data)) {
    inner <- if (length(data) > 0) getIndexTypes(data[[1]]) else NULL
    c(!is.null(names(data)), inner)
  }
  else {
    NULL
  }
}
