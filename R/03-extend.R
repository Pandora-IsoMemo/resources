extend <- function(what, with, overwrite = FALSE, strip = FALSE) {
  stopifnot(is.list(with))

  if (!is.list(what)) what <- list()
  if (strip) what <- stripList(what, with)

  replace <- if (overwrite) namesOrIndices(with)
  else missingOrList(what, namesOrIndices(with))

  for (r in replace) {
    current <- if (r %notin% namesOrIndices(what)) NULL
    else what[[r]]

    if (is.list(with[[r]])) what[[r]] <- extend(current, with[[r]], overwrite, strip)
    else what[[r]] <- with[[r]]
  }
  what
}

stripList <- function(what, with) {
  missing <- namesOrIndices(what) %notin% namesOrIndices(with)
  what[missing] <- NULL
  if (is.null(names(with))) names(what) <- NULL
  what
}

missingInList <- function(x, list) {
  x %notin% namesOrIndices(list) || is.null(list[[x]]) || is.list(list[[x]]) ||
    length(list[[x]]) == 0
}

missingOrList <- function(list, n = namesOrIndices(list)) {
  m <- unlist(lapply(n, missingInList, list = list))
  n[m]
}

namesOrIndices <- function(l) {
  stopifnot(is.list(l))

  if (is.null(names(l))) seq_along(l)
  else names(l)
}
