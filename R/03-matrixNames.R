setMatrixNames <- function(m, rownames = NULL, colnames = NULL) {
  if (is.null(m)) m <- matrix(NA, 0, 0)

  if (!is.matrix(m)) m <- matrix(m, length(m), length(m) > 0)

  if (!is.null(rownames)) {
    while (nrow(m) < length(rownames)) {
      m <- rbind(m, rep(NA, ncol(m)))
    }
    rownames(m) <- c(rownames, rep("", nrow(m) - length(rownames)))
  }

  if (!is.null(colnames)) {
    while (ncol(m) < length(colnames)) {
      m <- cbind(m, rep(NA, nrow(m)))
    }
    colnames(m) <- c(colnames, rep("", ncol(m) - length(colnames)))
  }

  m
}

addMissingValues <- function(x, add) {
  c(x, add[!(add %in% x)])
}

dropEmptyValues <- function(x) {
  x[is.na(x) | trimws(x) == ""]
}

asMatrix <- function(m) {
  if (is.null(m)) m <- matrix(NA, 0, 0)

  if (!is.matrix(m)) m <- matrix(m, length(m), length(m) > 0)

  if (is.null(rownames(m))) rownames(m) <- rep("", nrow(m))
  if (is.null(colnames(m))) colnames(m) <- rep("", ncol(m))

  m
}
