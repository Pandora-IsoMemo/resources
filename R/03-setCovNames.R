setCovNames <- function(obj, names) {
  if (is.list(obj)) {
    obj <- lapply(obj, setCovNames, names = names)
  }

  if (is.matrix(obj)) {
    new <- diag(length(names))
    mode(new) <- mode(obj)

    colnames(new) <- names
    rownames(new) <- names

    mincol <- pmin(ncol(new), ncol(obj))
    minrow <- pmin(nrow(new), nrow(obj))

    new[seq_len(minrow), seq_len(mincol)] <- obj[seq_len(minrow), seq_len(mincol)]

    obj <- new
  }

  return(obj)
}
