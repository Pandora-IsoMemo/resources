getList <- function(l, i) {
  e <- parent.frame()
  obj <- deparse(substitute(l))
  indices <- createIndex(i)
  cmd <- paste0(obj, indices)
  eval(parse(text = cmd), envir = e)
}

setList <- function(l, i, value) {
  e <- parent.frame()
  obj <- deparse(substitute(l))
  indices <- createIndex(i)
  cmd <- paste0(obj, indices, " <- ", deparse(substitute(value)))
  eval(parse(text = cmd), envir = e)
}

createIndex <- function(i) {
  if (length(i) == 0) {
    return("")
  }

  args <- rep(NA, length(i))
  args[is.na(i)] <- "[[1]]"
  args[!is.na(i)] <- paste0('[["', i[!is.na(i)], '"]]')

  paste(args, collapse = "")
}

indexLength <- function(x) {
  if (is.list(x) && length(x) > 0) {
    indexLength(x[[1]]) + 1
  } else if (is.list(x)) {
    1
  } else {
    0
  }
}
