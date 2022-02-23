outputPlotUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    mainPanel(
      width = 8,
      plotOutput(outputId = ns("SourcePlot")),
      plotExportButton(ns("exportSourcePlot")),
      exportDataUI(ns("exportData"), "Export Data"),
      HTML("<br><br>"),
      conditionalPanel(
        condition = "input.plotType == 'BoxPlot'",
        ns = ns,
        actionButton(ns("add_btn"), "Add data point"),
        actionButton(ns("rm_btn"), "Remove data point"),
        # colourInput(ns("col_btn"), "Select colour of data point"),
        # sliderInput(ns("size_btn"), "Size data point", min = 0.1, max = 10, value = 1),
        uiOutput(ns("pointInput"))
      )
    ),
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = ns("estType"),
        label = "Select group of estimates",
        choices = c("Sources", "User Estimates")
      ),
      selectInput(
        inputId = ns("groupType"),
        label = "Display by:",
        choices = c("Estimate" = "Estimate", "Target" = "Target")
      ),
      selectInput(
        inputId = ns("filterType"),
        label = "Select category:",
        choices = NULL
      ),
      selectInput(
        inputId = ns("individuals"),
        label = "Select filter categories:",
        choices = c("")
      ),
      pickerInput(ns("groupVars"), "Select target",
        choices = list(),
        selected = NULL, multiple = TRUE, options = list(
          `actions-box` = TRUE,
          size = 12
        )
      ),
      selectInput(
        inputId = ns("plotType"),
        label = "Select plot type:",
        choices = c(
          "BoxPlot", "KernelDensity",
          "Histogram"
        )
      ),
      checkboxInput(
        inputId = ns("showLegend"),
        label = "Show legend",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.plotType == 'Histogram'",
        ns = ns,
        sliderInput(
          inputId = ns("histBins"),
          label = "Nmber of histogram bins", min = 5, max = 200, value = 50
        )
      ),
      checkboxInput(inputId = ns("Teaser"), label = "Show header", value = FALSE),
      conditionalPanel(
        condition = "input.Teaser == true",
        ns = ns,
        textAreaInput(inputId = ns("headerLabel"), label = "Header", value = "")
      ),
      textAreaInput(inputId = ns("xlabel"), label = "Title x-axis", value = ""),
      textAreaInput(inputId = ns("ylabel"), label = "Title y-axis", value = ""),
      numericInput(inputId = ns("sizeTextX"), label = "Font size x-axis title", value = 24),
      numericInput(inputId = ns("sizeTextY"), label = "Font size y-axis title", value = 24),
      numericInput(inputId = ns("sizeAxisX"), label = "Font size x-axis", value = 18),
      numericInput(inputId = ns("sizeAxisY"), label = "Font size y-axis", value = 18),
      selectInput(inputId = ns("colorPalette"), label = "Color Palette", choices = c("default", "white", RColorBrewer::brewer.pal.info %>% row.names())),
      selectInput(
        inputId = ns("contributionLimit"), label = "Limit contribution axis",
        choices = c("None", "0-1", "0-100%"), selected = "0-1"
      ),
      selectInput(inputId = ns("fontFamily"), label = "Font", selected = NULL, choices = availableFonts()),
      helpText("This will only have an effect on the pdf output"),
      sliderInput(inputId = ns("boxQuantile"), label = "Box upper quantile", value = 0.68, min = 0.5, max = 0.99, step = 0.01),
      sliderInput(inputId = ns("whiskerMultiplier"), label = "Whiskers coverage interval", value = 0.95, min = 0.5, max = 1, step = 0.001)
    )
  )
}

outputPlot <- function(input, output, session, model, values) {
  callModule(plotExport, "exportSourcePlot", plotFun = plotFunTarget, type = "output")
  options(deparse.max.lines = 1)
  pointDat <- reactiveVal({
    data.frame(
      index = numeric(0),
      y = numeric(0),
      group = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    )
  })

  observeEvent(model(), ignoreNULL = FALSE, {
    pointDat(data.frame(
      index = numeric(0),
      y = numeric(0),
      group = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    ))
  })
  plotParams <- reactive({
    binSize <- NULL
    if (!is.null(input$`exportData-bins`) && input$`exportData-bins` == TRUE) {
      binSize <- input$`exportData-binSize`
    }
    list(
      fruitsObj = model()$fruitsObj$data,
      modelResults = values$modelResultSummary,
      estType = input$estType,
      groupType = input$groupType,
      filterType = input$filterType,
      # filterType2 = input$filterType2,
      groupVars = input$groupVars,
      individual = input$individuals,
      plotType = input$plotType,
      showLegend = input$showLegend,
      histBins = input$histBins,
      binSize = binSize,
      Teaser = input$Teaser,
      headerLabel = input$headerLabel,
      ylabel = input$ylabel,
      xlabel = input$xlabel,
      xTextSize = input$sizeTextX,
      yTextSize = input$sizeTextY,
      xAxisSize = input$sizeAxisX,
      yAxisSize = input$sizeAxisY,
      colorPalette = input$colorPalette,
      contributionLimit = input$contributionLimit,
      pointDat = na.omit(pointDat()),
      fontFamily = input$fontFamily,
      boxQuantile = input$boxQuantile,
      whiskerMultiplier = input$whiskerMultiplier
    )
  }) %>% debounce(100)

  plotFunTarget <- reactive({
    validate(validInput(model()))
    function() {
      params <- c(
        plotParams()
      )
      do.call(
        plotTargets,
        params
      )
    }
  })

  dataFunTarget <- reactive({
    validate(validInput(model()))
    function() {
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

  output$SourcePlot <- renderCachedPlot(
    {
      validate(validInput(model()))
      plotFunTarget()()
    },
    cacheKeyExpr = {
      plotParams()
    }
  )


  callModule(exportData, "exportData", dataFunTarget)

  observe({
    groupTypChoices <- c("Estimate")

    if (!is.null(model()) && model()$fruitsObj$modelOptions$modelType != "1") {
      groupTypChoices <- c(groupTypChoices, "Target")
      if (model()$fruitsObj$modelOptions$hierarchical) {
        groupTypChoices <- c(groupTypChoices, colnames(model()$fruitsObj$data$covariates))
        if (NCOL(model()$fruitsObj$data$covariates) > 1) {
          groupTypChoices <- c(groupTypChoices, "covariateInteraction")
        }
      }
    }

    if (!is.null(model())) {
      estTypChoices <- unique(values$modelResultSummary[, "Group"])
    } else {
      estTypChoices <- NULL
    }
    updateSelectInput(session, "estType", choices = estTypChoices)

    updateSelectInput(session, "groupType", choices = groupTypChoices)

    observeEvent(input$estType, {
      if (grepl(
        paste(c(
          "Source contributions",
          "Component contributions",
          "Source contributions by proxy"
        ), collapse = "|"),
        input$estType
      )) {
        updateSelectInput(session, "contributionLimit", selected = "0-1")
      } else {
        updateSelectInput(session, "contributionLimit", selected = "None")
      }
    })
  })

  observe({
    filterTypeChoices <- c("Estimate", "Target")
    if (!is.null(model()) && model()$fruitsObj$modelOptions$modelType != "1") {
      if (model()$fruitsObj$modelOptions$hierarchical) {
        filterTypeChoices <- c(
          filterTypeChoices,
          colnames(model()$fruitsObj$data$covariates)
        )
      }
    }
    filterTypeChoices <- filterTypeChoices[filterTypeChoices != input$groupType]

    updateSelectInput(
      session = session, inputId = "filterType",
      choices = filterTypeChoices
    )
  })

  observe({
    if (input$filterType == "Target") {
      updateSelectInput(session,
        "individuals",
        label = "Target",
        choices = unique(c(rownames(model()$fruitsObj$data$obsvn), "all"))
      )
      updatePickerInput(session,
        "groupVars",
        label = "Select target"
      )
    }
    if (input$filterType == "Estimate") {
      parameterNames <- values$modelResultSummary[values$modelResultSummary[, "Group"] == input$estType, "Estimate"]
      updateSelectInput(session,
        "individuals",
        label = "Estimate",
        choices = unique(parameterNames)
      )
      updatePickerInput(session,
        "groupVars",
        label = "Select estimates"
      )
    }
    if (!(input$filterType %in% c("Target", "Estimate", ""))) {
      covNames <- c(as.character(unique(values$modelResultSummary[, input$filterType])), "all")

      updateSelectInput(session,
        "individuals",
        label = input$filterType,
        choices = unique(covNames)
      )
      updatePickerInput(session,
        "groupVars",
        label = "Select estimates"
      )
    }

    if (input$filterType == input$groupType) {
      updateSelectInput(session,
        "individuals",
        choices = "all"
      )
    }
  })

  groupChoices <- reactive({
    as.character(unique(
      values$modelResultSummary[values$modelResultSummary[, "Group"] == input$estType, input$groupType]
    ))
  })


  observe({
    updatePickerInput(
      session = session, inputId = "groupVars",
      choices = groupChoices(), selected = groupChoices()
    )
  })

  addRow <- function(df) {
    rbind(df, data.frame(
      index = nrow(df) + 1, y = NA, group = NA,
      pointColor = "black", pointSize = 1,
      pointAlpha = 0.5, stringsAsFactors = FALSE
    ))
  }

  rmRow <- function(df) {
    if (nrow(df) > 0) {
      df[-nrow(df), , drop = FALSE]
    } else {
      df
    }
  }

  observeEvent(input$add_btn, {
    pointDat(addRow(pointDat()))
  })

  observeEvent(input$rm_btn, {
    pointDat(rmRow(pointDat()))
  })

  observe({
    df <- pointDat()
    indices <- df$index

    lapply(indices, function(index) {
      yval <- input[[paste("y", index, sep = "_")]]
      groupval <- input[[paste("group", index, sep = "_")]]
      pointColor <- input[[paste("pointColor", index, sep = "_")]]
      pointSize <- input[[paste("pointSize", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlpha", index, sep = "_")]]
      df[index, "pointColor"] <<- if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<- if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<- if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "group"] <<- if (is.null(groupval)) NA else groupval
    })

    pointDat(df)
  })

  inputGroup <- reactive({
    createPointInputGroup(pointDat(), groupChoices = groupChoices(), ns = session$ns)
  })

  output$pointInput <- renderUI(inputGroup())
  output$n <- reactive(nrow(pointDat()))
  outputOptions(output, "n", suspendWhenHidden = FALSE)
}

createPointInputGroup <- function(df, groupChoices, ns) {
  lapply(seq_len(nrow(df)), function(i) {
    createPointInput(
      index = df$index[i],
      y = df$y[i],
      group = df$group[i],
      groupChoices = groupChoices,
      pointColor = df$pointColor[i],
      pointSize = df$pointSize[i],
      pointAlpha = df$pointAlpha[i],
      ns = ns
    )
  })
}

createPointInput <- function(index, y, group, groupChoices,
                             pointColor, pointSize, pointAlpha, ns) {
  tags$div(
    numericInput(
      ns(paste("y", index, sep = "_")),
      paste("y", index, sep = "_"),
      value = y,
      min = 0,
      max = 1,
      step = 0.01
    ),
    selectInput(
      ns(paste("group", index, sep = "_")),
      paste("group", index, sep = "_"),
      selected = group,
      choices = groupChoices
    ),
    colourInput(ns(paste("pointColor", index, sep = "_")),
      label = "Choose point color", value = pointColor
    ),
    sliderInput(ns(paste("pointSize", index, sep = "_")),
      label = "Point size", value = pointSize, min = 0.1, max = 5, step = 0.1
    ),
    sliderInput(ns(paste("pointAlpha", index, sep = "_")),
      label = "Point alpha", value = pointAlpha, min = 0.1, max = 1, step = 0.1
    )
  )
}

availableFonts <- function() {
  intersect(names(postscriptFonts()), names(pdfFonts()))
}
