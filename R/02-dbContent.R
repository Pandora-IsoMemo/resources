dbContentButton <- function(id, table) {
  ns <- NS(id)

  actionButton(ns("popup"), tableLabels(table))
}

dbContent <- function(input, output, session, table) {
  observeEvent(input$popup, {
    showModal(dbContentDialog(id = table, ns = session$ns))
  })

  data <- reactive({
    getDbContent(table)
  })

  output$table <- renderDT(data())

  callModule(exportData, "export", data = reactive(function() {
    getDbContent(table)
  }))
}

getDbContent <- function(table) {
  openxlsx::read.xlsx(system.file("lives.xlsx", package = "ReSources"))
}

dbContentDialog <- function(id, ns) {
  modalDialog(
    title = tableLabels(id),
    easyClose = TRUE,
    size = "l",
    footer = modalButton("OK"),
    div(
      style = "overflow-x: auto; padding: 10px 0;",
      DTOutput(ns("table"))
    ),
    exportDataUI(ns("export"), "Export Data")
  )
}

tableLabels <- function(id) {
  switch(id,
    feeding = "Feeding experiments",
    suess = "Suess effect",
    diet = "Diet-to-consumer parameters",
    digest = "Digestibility",
    cat("Table ", id, "not found")
  )
}
