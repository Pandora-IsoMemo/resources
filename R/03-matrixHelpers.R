bindMatrices <- function(x, y, z) {
    m <- list(x, y, z)

    m <- Filter(Negate(is.null), m)

    m <- lapply(m, as.matrix)

    ncols <- max(unlist(lapply(m, ncol)))

    m <- lapply(m, function(o) {
        while (ncol(o) < ncols) {
            o <- cbind(o, rep(NA, nrow(o)))
        }

        colnames(o) <- colnames(y)

        o
    })

    do.call(rbind, m)
}