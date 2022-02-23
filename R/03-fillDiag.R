fillDiag <- function(m, value = 1) {
  if (!is.matrix(m)) {
    return(m)
  }

  emptyRows <- which(apply(m, 1, function(x) all(is.na(x))))
  emptyCols <- which(apply(m, 2, function(x) all(is.na(x))))
  bothEmpty <- intersect(emptyRows, emptyCols)
  m[bothEmpty, ] <- 0
  m[, bothEmpty] <- 0
  m[cbind(bothEmpty, bothEmpty)] <- value
  m
}
