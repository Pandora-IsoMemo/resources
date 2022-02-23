#' Create events from changed matrix names
#'
#' @param old old matrix
#' @param new new matrix
#' @param row name of variable for row names to be used in events list
#' @param col name of variable for column names to be used in events list
#' @param update create update events
#'
#' @return list of events of the form list(event, variable, old, new)
createNameEvents <- function(old, new, row, col, update = TRUE) {
  res <- c(
    createRemoveEvent(old, new, row, rownames),
    createRemoveEvent(old, new, col, colnames)
  )

  res <- c(
    res,
    createInsertEvent(old, new, row, rownames),
    createInsertEvent(old, new, col, colnames)
  )

  if (update) {
    res <- c(
      res,
      createUpdateEvent(old, new, row, rownames),
      createUpdateEvent(old, new, col, colnames)
    )
  }
  names(res) <- NULL

  res
}

createInsertEvent <- function(old, new, variable, nameFunction = rownames) {
  oldNames <- nameFunction(old)
  newNames <- nameFunction(new)

  oldLength <- length(oldNames)
  newLength <- length(newNames)

  minLength <- min(oldLength, newLength)

  updated <- oldNames[seq_len(minLength)] != newNames[seq_len(minLength)]

  add <- updated &
    oldNames[seq_len(minLength)] %in% oldNames[seq_len(minLength)][!updated]

  addNames <- newNames[which(add)]

  if (newLength > oldLength) {
    addNames <- c(addNames, newNames[seq(oldLength, newLength)])
  }

  addNames <- addNames[!(addNames %in% oldNames)]
  addNames <- unique(addNames)

  lapply(addNames, function(name) {
    list(event = "insert", variable = variable, old = NULL, new = name)
  })
}

createUpdateEvent <- function(old, new, variable, nameFunction = rownames) {
  oldNames <- nameFunction(old)
  newNames <- nameFunction(new)

  minLength <- min(length(oldNames), length(newNames))

  oldNames <- oldNames[seq_len(minLength)]
  newNames <- newNames[seq_len(minLength)]

  updated <- oldNames != newNames
  updated <- updated &
    !(newNames %in% oldNames[!updated]) &
    !(oldNames %in% oldNames[!updated]) # fire insert here

  d <- data.frame(
    old = oldNames[updated],
    new = newNames[updated],
    stringsAsFactors = FALSE
  )

  d <- unique(d)

  mapply(function(old, new) {
    list(event = "update", variable = variable, old = old, new = new)
  }, old = d$old, new = d$new, SIMPLIFY = FALSE)
}

createRemoveEvent <- function(old, new, variable, nameFunction = rownames) {
  oldLength <- length(nameFunction(old))
  newLength <- length(nameFunction(new))

  if (oldLength <= newLength) {
    return(list())
  }

  removeNames <- nameFunction(old)[seq(newLength, oldLength)]
  removeNames <- unique(removeNames[!(removeNames %in% nameFunction(new))])

  lapply(removeNames, function(name) {
    list(event = "remove", variable = variable, old = name, new = NULL)
  })
}

#' Update matrix based on events list from createNameEvents
#'
#' @param m matrix
#' @param events event list from createNameEvents
#' @param row variable used for row names
#' @param col variable used for column names
#'
#' @return altered matrix
processNameEvents <- function(m, events, row, col) {
  m <- asMatrix(m)
  events <- sortNameEvents(events)

  for (event in events) {
    m <- processSingleEvent(m, event, row, col)
  }
  m
}

sortNameEvents <- function(events) {
  types <- unlist(lapply(events, `[[`, "event"))
  types <- factor(types, levels = c("remove", "update", "insert"))
  events[order(types)]
}

processNameEventsList <- function(l, events, row, col, lindex, rownames, colnames) {
  m <- matrix(NA, length(rownames), length(colnames))
  rownames(m) <- rownames
  colnames(m) <- colnames

  l <- processNameEventsListNames(l, events, lindex, dummy = m)

  for (i in names(l)) {
    l[[i]] <- processNameEvents(l[[i]], events, row, col)
  }

  l
}

processNameEventsListNames <- function(l, events, lindex, dummy) {
  for (event in events) {
    if (event$variable != lindex) next

    if (event$event == "insert" && !(event$new %in% names(l))) {
      l[[event$new]] <- dummy
    }

    if (event$event == "update" && event$old %in% names(l)) {
      names(l)[names(l) == events$old] <- event$new
    }

    if (event$event == "remove" && event$old %in% names(l)) {
      l[[event$old]] <- NULL
    }
  }

  return(l)
}

processSingleEvent <- function(m, event, row, col) {
  if (!(event$variable %in% c(row, col))) {
    return(m)
  }

  where <- character(0)

  if (event$variable == row) where <- "row"
  if (event$variable == col) where <- c(where, "col")

  switch(event$event,
    insert = processInsertEvent(m, old = event$old, new = event$new, where),
    update = processUpdateEvent(m, old = event$old, new = event$new, where),
    remove = processRemoveEvent(m, old = event$old, new = event$new, where)
  )
}

processInsertEvent <- function(m, old = NULL, new, where = "row") {
  if ("row" %in% where & !(new %in% rownames(m))) {
    m <- rbind(m, rep_len(NA, ncol(m)))
    rownames(m)[nrow(m)] <- new
  }

  if ("col" %in% where & !(new %in% colnames(m))) {
    m <- cbind(m, rep_len(NA, nrow(m)))
    colnames(m)[ncol(m)] <- new
  }
  m
}

processRemoveEvent <- function(m, old, new = NULL, where = "row") {
  if ("row" %in% where & old %in% rownames(m)) {
    matches <- rownames(m) == old

    m <- m[!matches, , drop = FALSE]
  }
  if ("col" %in% where & old %in% colnames(m)) {
    matches <- colnames(m) == old

    m <- m[, !matches, drop = FALSE]
  }
  m
}

emptyRows <- function(m) {
  apply(m, 1, emptyVector)
}

emptyCols <- function(m) {
  apply(m, 2, emptyVector)
}

emptyVector <- function(v) {
  all(is.na(v) | v == "")
}

processUpdateEvent <- function(m, old, new, where = "row") {
  if ("row" %in% where & old %in% rownames(m)) {
    rownames(m)[rownames(m) == old] <- new
  }
  if ("col" %in% where & old %in% colnames(m)) {
    colnames(m)[colnames(m) == old] <- new
  }
  m
}
