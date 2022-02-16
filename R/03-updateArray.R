updateArray <- function(a, x, y, z) {
  aNew <- array(NA, dim = c(length(x), length(y), length(z)),
                dimnames = list(x, y, z))

  if (is.null(a)) return(aNew)

  stopifnot(length(dim(a)) == 3)

  dims <- pmin(dim(a), dim(aNew))

  for (i in seq_len(dims[1])){
    for (j in seq_len(dims[2])){
      for (k in seq_len(dims[3])){
        aNew[i, j, k] <- a[i, j, k]
      }
    }
  }

  aNew
}

updateCovarianceArray <- function(a, x, y) {
  aNew <- array(diag(length(x)),
                dim = c(length(x), length(x), length(y)),
                dimnames = list(x, x, y))

  if (is.null(a)) return(aNew)

  dims <- pmin(dim(a), dim(aNew))

  for (i in seq_len(dims[1])){
    for (j in seq_len(dims[2])){
      for (k in seq_len(dims[3])){
        aNew[i, j, k] <- a[i, j, k]
      }
    }
  }

  aNew
}
