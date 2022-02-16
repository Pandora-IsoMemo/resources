modelDiagnosticsPlotUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    mainPanel(
      width = 8,
      plotOutput(outputId = ns("DiagnosticsPlot")),
      plotExportButton(ns("exportDiagnosticsPlot")),
      exportDataUI(ns("exportDataDiag"), "Export Data"),
      exportDataUI(ns("exportDataChainsAll"), "Export all chains")
    ),
    sidebarPanel(
      width = 3,
      selectInput(inputId = ns("estTypeDiag"),
                  label = "Select group of estimates",
                  choices = c("Sources", "User Estimates")),
      selectInput(inputId = ns("groupTypeDiag"),
                  label = "Display by:",
                  choices = c("Estimate" = "Estimate", "Target" = "Target")),
      selectInput(inputId = ns("filterTypeDiag"),
                  label = "Select category:",
                  choices = NULL),
      # conditionalPanel(
      #   condition = "input.estTypeDiag == 'Source contributions by proxy' || input.estTypeDiag == 'userEstimates'",
      #   ns = ns,
      #   selectInput(inputId = ns("filterType2Diag"),
      #               label = "Select parameter group:",
      #               choices = NULL),
      # ),
      selectInput(inputId = ns("individualsDiag"),
                  label = "Select filter categories:",
                  choices = c("")),
      pickerInput(ns("groupVarsDiag"), "Select target",
                  choices = list(),
                  selected = NULL, multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE, 
                    size = 12
                  )),
      selectInput(inputId = ns("plotTypeDiag"),
                  label = "Select plot type:",
                  choices = c("Trace", "AutoCor")),
      checkboxInput(inputId = ns("showLegendDiag"), 
                    label = "Show Legend",
                    value = FALSE),
      conditionalPanel(
        condition = "input.plotTypeDiag == 'Histogram'",
        ns = ns,
        sliderInput(inputId = ns("histBinsDiag"), 
                    label = "Nmber of histogram bins", min = 5, max = 200, value = 50)
      ),
      textAreaInput(inputId = ns("headerLabelDiag"), 
                    label = "Header", value = ""),
      textAreaInput(inputId = ns("xlabelDiag"),
                    label = "Title x-axis", value = ""),
      textAreaInput(inputId = ns("ylabelDiag"),
                    label = "Title y-axis", value = ""),
      numericInput(inputId = ns("sizeTextXDiag"), label = "Font size x-axis title", value = 24),
      numericInput(inputId = ns("sizeTextYDiag"), label = "Font size y-axis title", value = 24),
      numericInput(inputId = ns("sizeAxisXDiag"), label = "Font size x-axis", value = 18),
      numericInput(inputId = ns("sizeAxisYDiag"), label = "Font size y-axis", value = 18),
      selectInput(inputId = ns("contributionLimitDiag"),
                  label = "Limit contribution axis", choices = c("None", "0-1", "0-100%"),
                  selected = "0-1"), 
      selectInput(inputId = ns("colorPaletteDiag"), label = "Color Palette",
                  choices = c("default", RColorBrewer::brewer.pal.info %>% row.names()))
    )
  )

}


modelDiagnosticsPlot <- function(input, output, session, model, values) {
  plotParams <- reactive({
    list(
      fruitsObj = model()$fruitsObj$data,
      modelResults = values$modelResultSummary,
      estType = input$estTypeDiag,
      groupType = input$groupTypeDiag,
      filterType = input$filterTypeDiag,
      groupVars = input$groupVarsDiag,
      # filterType2 = input$filterType2Diag,
      individual = input$individualsDiag,
      plotType = input$plotTypeDiag,
      Teaser = FALSE,
      showLegend = input$showLegendDiag,
      histBins = input$histBinsDiag,
      headerLabel = input$headerLabelDiag,
      ylabel = input$ylabelDiag,
      xlabel = input$xlabelDiag,
      xTextSize = input$sizeTextXDiag,
      yTextSize = input$sizeTextYDiag,
      xAxisSize = input$sizeAxisXDiag,
      yAxisSize = input$sizeAxisYDiag,
      contributionLimit = input$contributionLimitDiag,
      colorPalette = input$colorPaletteDiag
    )
  }) %>% debounce(100)
  
  ## Plot Function
  plotFunTargetDiagnostics <- reactive({
    validate(validInput(model()))
    function(){
      do.call(
        plotTargets,
        plotParams()
      )
    }
  })

  ## Render Plot
  output$DiagnosticsPlot <- renderCachedPlot({
    validate(validInput(model()))
    plotFunTargetDiagnostics()()
  },
  cacheKeyExpr = {plotParams()})

  ## Export Plot
  callModule(plotExport, "exportDiagnosticsPlot", plotFun = plotFunTargetDiagnostics, type = "diagnostics")

  ## Export Data Function
  dataFunTargetDiag <- reactive({
    validate(validInput(model()))
    function(){
      params <- c(
        plotParams(),
        returnType = "data"
      )
      do.call(
        plotTargets,
        params
      )
    }
  })

  ## Export Data
  callModule(exportData, "exportDataDiag", dataFunTargetDiag)

  ## Export all Chains
  expChains <- reactive({
    function() {
      getResultStatistics(
        model()$modelResults$parameters,
        model()$modelResults$userEstimateSamples,
        model()$fruitsObj,
        DT = FALSE,
        agg = FALSE
      )
    }
  })

  callModule(exportData, "exportDataChainsAll", expChains)

  ## Update Input
  observe({
    groupTypChoicesDiag <-  c("Estimate")

    if(!is.null(model()) && model()$fruitsObj$modelOptions$modelType != "1"){
      groupTypChoicesDiag <-  c(groupTypChoicesDiag, "Target")
    }

    if (!is.null(model())){
      estTypChoices <- unique(values$modelResultSummary[, "Group"])
    } else {
      estTypChoices <- NULL
    }
    updateSelectInput(session, "estTypeDiag", choices = estTypChoices)

    updateSelectInput(session, "groupTypeDiag", choices = groupTypChoicesDiag)
    
    observeEvent(input$estTypeDiag, {
      if(input$estTypeDiag %in% c("Source contributions",
                                  "Component contributions",
                                  "Source contributions by proxy")){
        updateSelectInput(session, "contributionLimitDiag", selected = "0-1")
      } else {
        updateSelectInput(session, "contributionLimitDiag", selected = "None")
      }
    })
    
  })
  
  observe({
    filterTypeChoicesDiag <-  c("Estimate", "Target")
    if(!is.null(model()) && model()$fruitsObj$modelOptions$modelType != "1"){
      if(model()$fruitsObj$modelOptions$hierarchical){
        filterTypeChoicesDiag <-  c(filterTypeChoicesDiag,
                                    colnames(model()$fruitsObj$data$covariates))
      }
    }
    filterTypeChoicesDiag <- filterTypeChoicesDiag[filterTypeChoicesDiag != input$groupTypeDiag]

    updateSelectInput(session = session, inputId =  "filterTypeDiag",
                      choices = filterTypeChoicesDiag)

  })

  observe({
    if (input$filterTypeDiag == "Target") {
      updateSelectInput(session,
                        "individualsDiag",
                        label = "Target",
                        choices = unique(c(rownames(model()$fruitsObj$data$obsvn), "all")))
      updatePickerInput(session,
                        "groupVarsDiag",
                        label = "Select target")
      
    }

    if (input$filterTypeDiag == "Estimate") {
      parameterNames <- values$modelResultSummary[values$modelResultSummary[, "Group"] ==
                                                    input$estTypeDiag, "Estimate"]

      updateSelectInput(session,
                        "individualsDiag",
                        label = "Estimate",
                        choices = unique(parameterNames))
      updatePickerInput(session,
                        "groupVarsDiag",
                        label = "Select estimates")
      
    }

    if (!(input$filterTypeDiag %in% c("Target", "Estimate", ""))) {
      covNames <- c(as.character(unique(values$modelResultSummary[, input$filterType])), "all")
      updateSelectInput(session,
                        "individualsDiag",
                        label = input$filterTypeDiag,
                        choices = unique(covNames))
      updatePickerInput(session,
                        "groupVarsDiag",
                        label = "Select estimates")
      
    }
    if(input$filterTypeDiag == input$groupTypeDiag){
      updateSelectInput(session,
                        "individualsDiag",
                        choices = "all")
    }
  })
  groupChoices <- reactive({
    as.character(unique(
      values$modelResultSummary[values$modelResultSummary[, "Group"] == input$estTypeDiag, input$groupTypeDiag]
    ))
  })
  
  
  observe({
    updatePickerInput(session = session, inputId = "groupVarsDiag",
                      choices = groupChoices(), selected = groupChoices())
    
  })
  
}
