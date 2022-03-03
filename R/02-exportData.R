exportDataUI <- function(id, title) {
  ns <- NS(id)
  actionButton(ns("export"), title)
}

exportData <- function(input, output, session, data) {
  observeEvent(input$export, {
    showModal(
      modalDialog(
        "Export Data",
        easyClose = TRUE,
        footer = modalButton("OK"),
        selectInput(
          session$ns("exportType"),
          "File type",
          choices = c("csv", "xlsx", "json"),
          selected = "xlsx"
        ),
        conditionalPanel(
          condition = "input['exportType'] == 'csv'",
          ns = session$ns,
          div(
            style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(session$ns("colseparator"), "column separator:", value = ",")
          ),
          div(
            style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput(session$ns("decseparator"), "decimal separator:", value = ".")
          )
        ),
        checkboxInput(session$ns("bins"), "Export as bins"),
        conditionalPanel(
          condition = "input['bins'] == true",
          ns = session$ns,
          sliderInput(
            session$ns("binSize"),
            "Bin size",
            min = 0.01,
            max = 10,
            step = 0.01,
            value = 0.1
          )
        ),
        downloadButton(session$ns("exportExecute"), "Export")
      )
    )
  })
  
  output$exportExecute <- downloadHandler(
    filename = function() {
      exportFilename(input$exportType)
    },
    content = function(file) {
      switch(input$exportType,
             csv = exportCSV(file, data()(), input$colseparator, input$decseparator),
             xlsx = exportXLSX(file, data()()),
             json = exportJSON(file, data()())
      )
    }
  )
}

#' Filename of Export
#'
#' @param fileending character csv or xlsx
#' @export
exportFilename <- function(fileending) {
  paste("isotopeData", fileending, sep = ".")
}

#' Export to csv
#'
#' @param file filename
#' @param dat data.frame
#' @param colseparator column seperator
#' @param decseparator decimal seperator
#' @export
exportCSV <- function(file, dat, colseparator, decseparator) {
  write.table(
    x = dat,
    file = file,
    sep = colseparator,
    dec = decseparator,
    row.names = FALSE
  )
}

#' Export to xlsx
#'
#' @param file filename
#' @param dat data.frame
#' @export
exportXLSX <- function(file, dat) {
  write.xlsx(dat, file)
}

#' Export to json
#'
#' @param file filename
#' @param dat data.frame
exportJSON <- function(file, dat) {
  json <- toJSON(dat)
  write(json, file)
}