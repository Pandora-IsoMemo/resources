readString <- function(content, mode) {
    opts <- list(
        'comma-separated' = ',',
        'tab-separated' = "\t",
        'semicolon' = ";"
    )

    if (mode == 'auto') {
        l <- lapply(opts, function(cc) {
            length(unlist(regmatches(content, gregexpr(cc, content))))
        })
        sep <- opts[[which.max(l)]]
    } else {
        sep <- opts[[mode]]
    }

    as.matrix(
        read.table(
            textConnection(content), 
            row.names = NULL,
            header = TRUE, 
            check.names = FALSE, 
            sep = sep
        )
    )
}
