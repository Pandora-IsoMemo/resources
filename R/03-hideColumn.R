hideColumn <- function(id, column, rownames = TRUE) {
  toggleColumn(id, column, FALSE, rownames = rownames)
}

showColumn <- function(id, column, rownames = TRUE) {
  toggleColumn(id, column, TRUE, rownames = rownames)
}

showAllColumns <- function(id) {
  lapply(
    showAllColumnsJS(id),
    shinyjs::runjs
  )
}

toggleColumn <- function(id, column, visible, rownames) {
  if (is.na(column)) {
    return()
  }
  lapply(
    toggleColumnJS(id, column, visible, rownames),
    shinyjs::runjs
  )
}

toggleColumnJS <- function(id, column, visible, rownames) {
  valueCol <- column * 2
  visibility <- if (visible) "table-cell" else "none"

  list(
    paste0(
      "$('#", id, " table tr td:nth-child(", valueCol + rownames, ")')",
      ".css('display', '", visibility, "');"
    ),
    paste0(
      "$('#", id, " table tr td:nth-child(", valueCol - 1 + rownames, ")')",
      ".css('display', '", visibility, "');"
    ),
    paste0(
      "$('#", id, " table tr:nth-child(1) th:nth-child(", column + rownames, ")')",
      ".css('display', '", visibility, "');"
    ),
    paste0(
      "$('#", id, " table tr:nth-child(2) th:nth-child(", valueCol + rownames, ")')",
      ".css('display', '", visibility, "');"
    ),
    paste0(
      "$('#", id, " table tr:nth-child(2) th:nth-child(", valueCol - 1 + rownames, ")')",
      ".css('display', '", visibility, "');"
    )
  )
}

showAllColumnsJS <- function(id) {
  list(
    paste0(
      "$('#", id, " table tr td')",
      ".css('display', 'table-cell');"
    ),
    paste0(
      "$('#", id, " table tr th')",
      ".css('display', 'table-cell');"
    )
  )
}
