plotExport <- function(input, output, session, plotFun, type = "plot", plotly = FALSE) {
  observeEvent(input$export, {
    test <- try(plotly:::orca_available())
    if (inherits(test, "try-error")) {
      alert("The orca command-line utility is required for this functionality.\n\nPlease follow the installation instructions here -- https://github.com/plotly/orca#installation")
      return()
    }
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
        plotly::orca(plotFun()(), file = tmpfile)
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
