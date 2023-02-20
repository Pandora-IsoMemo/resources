plotExport <- function(input, output, session, plotFun, type = "plot", plotly = FALSE) {
  observeEvent(input$export, {
    plotOutputElement <- if (plotly) {
      plotlyOutput(session$ns("plotly"))
    } else {
      plotOutput(session$ns("plot"), height = "300px")
    }

    exportTypeChoices <- if (plotly) {
      c("png", "jpeg", "svg", "pdf")
    } else {
      c("png", "pdf", "svg", "tiff")
    }

    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      plotOutputElement,
      selectInput(
        session$ns("exportType"), "Filetype",
        choices = exportTypeChoices
      ),
      if (!plotly) numericInput(session$ns("width"), "Width (px)", value = 1280) else NULL,
      if (!plotly) numericInput(session$ns("height"), "Height (px)", value = 800) else NULL,
      downloadButton(session$ns("exportExecute"), "Export"),
      easyClose = TRUE
    ))
  })

  output$plot <- renderPlot({
    plotFun()()
  })

  output$plotly <- renderPlotly({
    plotFun()()
  })

  output$exportExecute <- downloadHandler(
    filename = function() {
      paste0(gsub("-", "", Sys.Date()), "_", type, ".", input$exportType)
    },
    content = function(file) {
      if (plotly) {
        tmpfile <- paste0("plot.", input$exportType)
        save_image(plotFun()(), file = tmpfile) %>%
          tryCatchWithWarningsAndErrors(errorTitle = "Export failed", alertStyle = "shinyalert")
        file.copy(tmpfile, file)
      } else {
        switch(input$exportType,
          png = png(file, width = input$width, height = input$height),
          pdf = pdf(file, width = input$width / 72, height = input$height / 72),
          tiff = tiff(file, width = input$width, height = input$height),
          svg = svg(file, width = input$width / 72, height = input$height / 72)
        )
        print(plotFun()())
        dev.off()
      }
    }
  )
}

plotExportButton <- function(id, plotly = FALSE) {
  ns <- NS(id)
  actionButton(ns("export"), "Export Plot")
}
