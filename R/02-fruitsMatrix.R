fruitsMatrixFilter <- function(scope, id, label = NA) {
  ns <- NS(scope)
  div(
    style = "display:inline-block",
    selectInput(ns(id), label = label, choices = NULL)
  )
}

fruitsMatrixInput <- function(scope, row, col, cov = FALSE, fixedCols = FALSE, double = TRUE, class = "numeric", toggleCov = FALSE) {
  ns <- NS(scope)

  colsFixed <- !is.logical(fixedCols)

  cols <- if (cov) {
    list(names = TRUE, editableNames = FALSE, extend = FALSE)
  } else if (colsFixed && double) {
    list(
      names = TRUE,
      multiheader = TRUE,
      extend = FALSE,
      delta = 2,
      editableNames = FALSE
    )
  } else if (colsFixed && !double) {
    list(
      names = TRUE,
      extend = FALSE,
      editableNames = FALSE,
      delta = 1
    )
  } else if (!colsFixed && double) {
    list(
      names = TRUE,
      multiheader = TRUE,
      extend = TRUE,
      delta = 2,
      editableNames = TRUE,
      delete = TRUE
    )
  } else if (!colsFixed && !double) {
    list(
      names = TRUE,
      extend = TRUE,
      editableNames = TRUE,
      delta = 1,
      delete = TRUE
    )
  }

  value <- if (cov) {
    emptyMatrix(sampleName(row, TRUE), sampleName(row, TRUE))
  } else if (double && colsFixed) {
    emptyMatrix2(sampleName(row, TRUE), fixedCols)
  } else if (double && !colsFixed) {
    emptyMatrix2(sampleName(row, TRUE), sampleName(col, TRUE))
  } else if (!double && colsFixed) {
    emptyMatrix(sampleName(row, TRUE), fixedCols)
  } else {
    emptyMatrix(sampleName(row, TRUE), sampleName(col, TRUE))
  }

  div(
    id = if (cov) ns("containerCov") else ns("container"),
    div(
      class = "copy-paste-buttons",
      if (toggleCov) {
        radioButtons(ns("showCov"), NULL, choices = c(
          "User-defined covariance matrix" = TRUE,
          "calculate covariance matrix from repeated measurements" = FALSE
        ))
      },
      copyButton(if (cov) ns("copyCov") else ns("copy"), tableId = if (cov) ns("covariance") else ns("table")),
      tags$div(
        class = "inline-select",
        selectInput(if (cov) ns("pasteModeCov") else ns("pasteMode"), NULL, choices = c("auto", "comma-separated", "tab-separated", "semicolon"))
      ),
      # pasteButtonsUI ----
      pasteButton(
        inputId = if (cov) ns("pasteCov") else ns("paste"),
        outputId = if (cov) ns("pastedCov") else ns("pasted"),
        containerId = if (cov) ns("containerCov") else ns("container")
      ),
      importDataUI(
        if (cov) ns("importCov") else ns("import"),
        "Import Data"
      ),
      exportDataUI(
        if (cov) ns("exportCov") else ns("export"),
        "Export Data"
      ),
      span(
        id = if (cov) ns("batchImportContainerCov") else ns("batchImportContainer"),
        style = "display:none;",
        importDataUI(
          if (cov) ns("batchImportCov") else ns("batchImport"), "Batch Import"
        ),
        actionButton(if (cov) ns("copyTargetCov") else ns("copyTarget"), "Copy data to other targets")
      )
    ),
    matrixInput(
      inputId = if (cov) ns("covariance") else ns("table"),
      inputClass = paste("fruits-matrix", if (double) "matrix-input-rownames" else NULL, if (colsFixed) "fixed-colnames" else NULL),
      class = class,
      value = value,
      cols = cols,
      pagination = FALSE,
      lazy = TRUE,
      rows = list(
        names = TRUE,
        editableNames = !cov,
        extend = !cov,
        delta = 1,
        delete = !cov
      )
    ),
    if (!cov) uiOutput(ns("pagination"))
  )
}

fruitsMatrixDistribution <- function(scope, choices = c("constant", "normal", "multivariate-normal", "log-normal"), selected = "normal") {
  ns <- NS(scope)

  div(
    style = "display:inline-block",
    selectInput(
      ns("distribution"),
      "Distribution",
      choices = choices,
      selected = selected
    )
  )
}

fruitsMatrix <- function(input, output, session, values, events, meanId, sdId = NULL, distributionId = NULL, covarianceId = NULL,
                         class = "numeric", row, col, namesCov = NULL,
                         filter = list(), filterCov = list(), fixedCols = FALSE) {
  ns <- session$ns

  colsFixed <- !is.logical(fixedCols)

  rowVar <- reactive({
    if (is.reactive(row)) {
      row()
    } else {
      row
    }
  })

  colVar <- reactive({
    if (is.reactive(col)) {
      col()
    } else {
      col
    }
  })

  namesCovVar <- reactive({
    if (is.reactive(namesCov)) {
      namesCov()
    } else {
      namesCov
    }
  })

  # Update Filter
  filterValues <- reactive({
    logDebug("Updating filterValues (%s)", meanId)
    unlist(lapply(filter, function(f) {
      if (!is.null(f$hide) && f$hide()) {
        NA
      } else if (isEmpty(input[[f$id]]) | !(input[[f$id]] %in% f$choices())) {
        f$choices()[1]
      } else {
        input[[f$id]]
      }
    }))
  })

  filterValuesDistribution <- reactive({
    logDebug("Updating filterValuesDistribution (%s)", meanId)
    unlist(lapply(filter, function(f) {
      if (!is.null(f$distribution) && !f$distribution) {
        NULL
      } else if (!is.null(f$hide) && f$hide()) {
        NA
      } else if (isEmpty(input[[f$id]]) | !(input[[f$id]] %in% f$choices())) {
        f$choices()[1]
      } else {
        input[[f$id]]
      }
    }))
  })

  filterChoices <- reactive({
    logDebug("Updating filterChoices (%s)", meanId)
    lapply(filter, function(f) {
      f$choices()
    })
  })

  filterChoicesDistribution <- reactive({
    logDebug("Updating filterChoicesDistribution (%s)", meanId)
    choices <- lapply(filter, function(f) {
      if (!is.null(f$distribution) && !f$distribution) {
        NULL
      } else {
        f$choices()
      }
    })
    Filter(Negate(is.null), choices)
  })

  observe({
    logDebug("Updating filter input fields (%s)", meanId)
    lapply(filter, function(f) {
      updateSelectizeInput(session, f$id, choices = f$choices())
      if (!is.null(f$hide) && f$hide()) {
        hide(f$id)
      } else {
        show(f$id)
      }
    })
  })

  filterValuesCov <- reactive({
    logDebug("Updating filterValuesCov (%s)", meanId)
    unlist(lapply(filterCov, function(f) {
      if (!is.null(f$hide) && f$hide()) {
        NA
      } else if (isEmpty(input[[f$id]]) | !(input[[f$id]] %in% f$choices())) {
        f$choices()[1]
      } else {
        input[[f$id]]
      }
    }))
  })

  filterChoicesCov <- reactive({
    logDebug("Updating filterChoicesCov (%s)", meanId)
    lapply(filterCov, function(f) {
      f$choices()
    })
  })

  observe({
    logDebug("Updating filterCov input fields (%s)", meanId)
    lapply(filterCov, function(f) {
      updateSelectInput(session, f$id, choices = f$choices())
    })
  })

  # toggle for covariance
  observeEvent(input$showCov, {
    if (meanId == "source") {
      values$sourceDistCovRep[[input$term]] <- input$showCov == "TRUE"
    }
    if (meanId == "concentration") {
      values$concentrationDistCovRep <- input$showCov == "TRUE"
    }
  })

  observe({
    if (meanId == "source") {
      updateRadioButtons(session, "showCov", selected = values$sourceDistCovRep[[input$term]])
    }
  })

  observe({
    if (meanId == "concentration") {
      updateRadioButtons(session, "showCov", selected = values$concentrationDistCovRep)
    }
  })

  # Show / Hide batch upload button
  observe({
    logDebug("Updating batch button for %s", meanId)
    showBatchButton <- lapply(filter, function(f) {
      (is.null(f$hide) || !f$hide()) && isTRUE(f$batch)
    })

    if (any(unlist(showBatchButton))) {
      show("batchImportContainer")
    } else {
      hide("batchImportContainer")
    }
  })

  # Extend complex objects
  observe(priority = 100, {
    logDebug("Extend complex objects (%s)", meanId)
    req(length(filterChoices()) > 0)

    dummy <- createDummyMatrix(
      names = c(
        filterChoices(),
        list(
          values[[rowVar()]],
          values[[colVar()]]
        )
      ),
      current = values[[meanId]]
    )
    values[[meanId]] <- extend(
      what = values[[meanId]],
      with = dummy,
      strip = TRUE
    )

    if (!is.null(sdId)) {
      dummy <- createDummyMatrix(
        names = c(
          filterChoices(),
          list(
            values[[rowVar()]],
            values[[colVar()]]
          )
        ),
        current = values[[sdId]]
      )

      values[[sdId]] <- extend(
        what = values[[sdId]],
        with = dummy,
        strip = TRUE
      )
    }

    if (!is.null(distributionId) && length(filterChoicesDistribution()) > 0) {
      distributionDummy <- createDummyList(
        names = filterChoicesDistribution(), "normal",
        current = values[[distributionId]]
      )

      values[[distributionId]] <- extend(
        what = values[[distributionId]],
        with = distributionDummy,
        strip = TRUE
      )
    }

    if (!is.null(covarianceId)) {
      covarianceDummy <- createDummyMatrix(
        names = c(
          filterChoicesCov(),
          list(
            namesCovVar(),
            namesCovVar()
          )
        ),
        current = values[[covarianceId]]
      )
      values[[covarianceId]] <- extend(
        what = values[[covarianceId]],
        with = covarianceDummy,
        strip = TRUE
      )
    }
  })

  observe({
    if (!is.null(covarianceId)) {
      values[[covarianceId]] <- setCovNames(values[[covarianceId]], namesCovVar())
    }
  })

  # Process name events for mean + sd
  observeEvent(events$name, priority = 400, {
    # if (!events$adaptive) {
    #   events$processed <- events$processed + 1
    #   return()
    # }
    #
    # logDebug("Process name events for mean and sd (%s)", meanId)
    # if (length(events$name) == 0) {
    #   return()
    # }
    # 
    # indices <- as.matrix(expand.grid(filterChoices()))
    # # set hidden to NA
    # lapply(seq_along(filter), function(i) {
    #   if (!is.null(filter[[i]]$hide) && filter[[i]]$hide()) indices[, i] <<- NA
    # })
    # 
    # # for tables without filter
    # if (ncol(indices) == 0) indices <- matrix(NA, 1, 0)
    # 
    # for (i in seq_len(nrow(indices))) {
    #   processed <- processNameEvents(
    #     getList(values[[meanId]], indices[i, ]),
    #     events$name,
    #     rowVar(),
    #     colVar()
    #   )
    # 
    #   setList(
    #     values[[meanId]],
    #     indices[i, ],
    #     processed
    #   )
    # 
    #   if (!is.null(sdId)) {
    #     processed <- processNameEvents(
    #       getList(values[[sdId]], indices[i, ]),
    #       events$name,
    #       rowVar(),
    #       colVar()
    #     )
    # 
    #     setList(
    #       values[[sdId]],
    #       indices[i, ],
    #       processed
    #     )
    #   }
    # }
    
    events$processed <- events$processed + 1
  })

  # # Get input from shiny matrix
  inputData <- eventReactive(input$table, {
    logDebug("Get input from shiny matrix for mean and sd (%s)", meanId)

    m <- input$table
    storage.mode(m) <- class
    m <- minimalMatrix(m)

    if (!is.null(sdId)) {
      split <- splitDoubleMatrix(m, rownames = TRUE)
      defaultMatrixNames(split, sampleName(rowVar()), sampleName(colVar()))
    } else {
      m <- asMatrix(m)

      m <- dropEmptyRows(m)
      m <- dropEmptyCols(m)

      defaultMatrixNames(m, sampleName(rowVar()), sampleName(colVar()))
    }
  })

  covarianceInputData <- eventReactive(input$covariance, {
    logDebug("Get input from shiny matrix for covariance (%s)", meanId)
    m <- input$covariance
    storage.mode(m) <- "numeric"

    m <- asMatrix(m)

    m <- dropEmptyRows(m)
    m <- dropEmptyCols(m)

    fillDiag(m)
  })

  # Get data from IsoMemo (only for targetValues)
  observeEvent(events$isoMemo, {
    req(meanId == "obsvn")

    data <- events$isoMemo

    split <- list(
      matrix(data$Mean, ncol = 1),
      matrix(data$Sd, ncol = 1)
    )

    split <- defaultMatrixNames(split, "Individual", "proxy")

    setList(values[[meanId]], filterValues, split[[1]])
    setList(values[[sdId]], filterValues, split[[2]])
  })

  # Get data from values ----
  meanData <- reactive({
    logDebug("Get data from values for mean (%s)", meanId)
    stopifnot(indexLength(values[[meanId]]) == length(filterValues()))

    as.matrix(getList(values[[meanId]], filterValues()))
  })

  sdData <- reactive({
    req(sdId)
    logDebug("Get data from values for sd (%s)", meanId)
    as.matrix(getList(values[[sdId]], filterValues()))
  })

  covarianceData <- reactive({
    req(covarianceId)
    logDebug("Get data from values for covariance (%s)", meanId)
    res <- getList(values[[covarianceId]], filterValuesCov())
    if (is.null(res)) matrix(NA, 0, 0) else as.matrix(res)
  })

  # Remove Name (col / row) ----
  observeEvent(input$tabledelete, priority = 100, {
    logDebug("Remove row or column from (%s)", meanId)
    
    fullMean <- removeLine(matrix = meanData(), 
                           type = input$tabledelete$type, 
                           name = input$tabledelete$name)
    setList(values[[meanId]], filterValues(), fullMean)
    
    if (!is.null(sdId)) {
      fullSd <- removeLine(matrix = sdData(), 
                           type = input$tabledelete$type, 
                           name = input$tabledelete$name)
      setList(values[[sdId]], filterValues(), fullSd)
    }
    
    if (meanId == "targetValuesCovariates") {
      categoricalVars <- intersect(values[["categoricalVars"]], 
                                   extractPotentialCat(values[["targetValuesCovariates"]]))
      setList(values[["categoricalVars"]], NULL, categoricalVars)
      
      numericVars <- intersect(values[["numericVars"]], 
                               extractPotentialNumerics(values[["targetValuesCovariates"]]))
      setList(values[["numericVars"]], NULL, numericVars)
    }
    
    # if (events$adaptive) {
    #   variable <- if (input$tabledelete$type == "row") {
    #     rowVar()
    #   } else {
    #     colVar()
    #   }
    #   
    #   event <- list(
    #     list(
    #       event = "remove",
    #       variable = variable,
    #       old = input$tabledelete$name,
    #       new = NULL
    #     )
    #   )
    #   
    #   events$name <- c(events$name, event)
    # }
  })
  
  # Process input data -> values ----
  observeEvent(inputData(), {
    logDebug("Process input data -> values for mean + sd (%s)", meanId)

    if (!is.null(sdId)) {
      inputMean <- inputData()[[1]]
      inputSd <- inputData()[[2]]
      
      fullMean <- meanData()
      fullSd <- sdData()

      i <- pmin((currentPage() - 1) * itemsPerPage + 1, nrow(meanData()))
      j <- pmin(i + itemsPerPage - 1, nrow(meanData()))

      fullMean <- bindMatrices(
        fullMean[seq_len(i - 1), , drop = FALSE],
        inputMean,
        if (j < nrow(fullMean)) fullMean[seq(from = j + 1, to = nrow(fullMean)), , drop = FALSE] else NULL
      )

      fullSd <- bindMatrices(
        fullSd[seq_len(i - 1), , drop = FALSE],
        inputSd,
        if (j < nrow(fullSd)) fullSd[seq(from = j + 1, to = nrow(fullSd)), , drop = FALSE] else NULL
      )

      events$name <- c(
        events$name,
        createNameEvents(meanData(), fullMean, rowVar(), colVar())
      )

      setList(values[[meanId]], filterValues(), fullMean)
      setList(values[[sdId]], filterValues(), fullSd)
    } else {
      inputMean <- inputData()
      fullMean <- meanData()

      i <- pmin((currentPage() - 1) * itemsPerPage + 1, nrow(meanData()))
      j <- pmin(i + itemsPerPage - 1, nrow(meanData()))

      fullMean <- bindMatrices(
        fullMean[seq_len(i - 1), , drop = FALSE],
        inputMean,
        if (j < nrow(fullMean)) fullMean[seq(from = j + 1, to = nrow(fullMean)), , drop = FALSE] else NULL
      )

      events$name <- c(
        events$name,
        createNameEvents(meanData(), fullMean, rowVar(), colVar())
      )

      setList(values[[meanId]], filterValues(), fullMean)
    }
  })

  observeEvent(covarianceInputData(), {
    logDebug("Process input data -> values covariance (%s)", meanId)

    setList(values[[covarianceId]], filterValuesCov(), covarianceInputData())
  })

  ## pagination
  currentPage <- reactiveVal(1)
  itemsPerPage <- 10

  observe({
    req(input$page)
    currentPage(input$page)
  })

  nPages <- reactive({
    ceiling(nrow(meanData()) / itemsPerPage)
  })

  output$pagination <- renderUI({
    if (nPages() == 1) {
      return(NULL)
    }

    pageSelection <- seq(
      max(1, currentPage() - 3),
      min(nPages(), currentPage() + 3)
    )

    btns <- lapply(pageSelection, function(i) {
      tags$button(
        paste(i),
        onClick = paste0("Shiny.setInputValue('", session$ns("page"), "', ", i, ")"),
        class = paste("btn btn-default paginate", if (currentPage() == i) "active")
      )
    })

    c(
      list(
        tags$button(
          "First",
          onClick = paste0("Shiny.setInputValue('", session$ns("page"), "', 1)"),
          class = paste("btn btn-default paginate")
        )
      ),
      if (pageSelection[1] != 1) "...",
      btns,
      if (tail(pageSelection, n = 1) != nPages()) "...",
      list(
        tags$button(
          "Last",
          onClick = paste0("Shiny.setInputValue('", session$ns("page"), "', ", nPages(), ")"),
          class = "btn btn-default paginate"
        )
      )
    )
  })

  meanDataPage <- reactive({
    i <- pmin((currentPage() - 1) * itemsPerPage + 1, nrow(meanData()))
    j <- pmin(i + itemsPerPage - 1, nrow(meanData()))
    meanData()[i:j, , drop = FALSE]
  })

  sdDataPage <- reactive({
    req(sdId)
    i <- pmin((currentPage() - 1) * itemsPerPage + 1, nrow(meanData()))
    j <- pmin(i + itemsPerPage - 1, nrow(sdData()))
    meanData()[i:j, , drop = FALSE]
    sdData()[i:j, , drop = FALSE]
  })

  # Process data from values -> UI
  observe({
    logDebug("Process date from values -> UI for sd and mean (%s)", meanId)

    if (is.null(sdId)) {
      updateMatrixInput(session, "table", meanDataPage())
    } else {
      updateMatrixInput(session, "table", combineDoubleMatrix(meanDataPage(), sdDataPage()))
    }
  })

  observe({
    req(covarianceId)
    logDebug("Process date from values -> UI for covariance (%s)", meanId)
    updateMatrixInput(session, "covariance", covarianceData())
  })

  observeEvent(input$copy, {
    if (is.null(sdId)) {
      data <- meanData()
    } else {
      data <- combineDoubleMatrix(meanData(), sdData())
    }
    data <- rbind(colnames(data), data)
    data <- cbind(rownames(data), data)
    lines <- apply(data, 1, paste, collapse = "\t")
    tsv <- paste(lines, collapse = "\n")

    shinyjs::runjs(paste0("
      navigator.clipboard.writeText(`", tsv, "`).then(function() {
        console.log('Copied to clipboard')
      }, function() {
        alert('Could not copy to clipboard')
      });
    "))
  })

  observeEvent(input$copyCov, {
    data <- covarianceData()

    data <- rbind(colnames(data), data)
    data <- cbind(rownames(data), data)
    lines <- apply(data, 1, paste, collapse = "\t")
    tsv <- paste(lines, collapse = "\n")

    shinyjs::runjs(paste0("
      navigator.clipboard.writeText(`", tsv, "`).then(function() {
        console.log('Copied to clipboard')
      }, function() {
        alert('Could not copy to clipboard')
      });
    "))
  })
  # input$pasted ----
  observeEvent(input$pasted, {
    m <- readStringWrapper(content = input$pasted$content, mode = input$pasteMode, class = class)
    if(is.null(m)) return()
    
    if (is.null(sdId)) {
      m <- fixMatrixCols(m, colnames(meanData()), fixedCols, rowVar(), colVar())

      m <- defaultMatrixNames(m, sampleName(rowVar()), sampleName(colVar()))
      events$name <- c(
        events$name,
        createNameEvents(meanData(), m, rowVar(), colVar())
      )

      setList(values[[meanId]], filterValues(), m)
    } else {
      split <- splitDoubleMatrix(m, rownames = !is.null(rownames(m)))

      split[[1]] <- defaultMatrixNames(split[[1]], sampleName(rowVar()), sampleName(colVar()))
      events$name <- c(
        events$name,
        createNameEvents(meanData(), split[[1]], rowVar(), colVar())
      )

      setList(values[[meanId]], filterValues(), split[[1]])
      setList(values[[sdId]], filterValues(), split[[2]])
    }
  })

# input$pastedCov ----
  observeEvent(input$pastedCov, {
    m <- readStringWrapper(content = input$pastedCov$content, mode = input$pasteModeCov, class = class)
    if(is.null(m)) return()
    
    m <- dropEmptyRows(m)
    m <- dropEmptyCols(m)

    oldNames <- colnames(covarianceData())
    length(oldNames) <- ncol(m)
    colnames(m) <- oldNames

    setList(values[[covarianceId]], filterValuesCov(), m)
  })

  ## -- Import

  # Get imported data
  dataImported <- callModule(
    importData, "import",
    rowNames = reactive(values[[rowVar()]]),
    colNames = reactive({
      if (is.null(sdId)) {
        values[[colVar()]]
      } else {
        rep(values[[colVar()]], each = 2)
      }
    }),
    customChecks = list(
      function() {
        function(df) {
          if (nrow(df) > 10000) {
            return("You cannot upload more than 10000 rows")
          } else {
            TRUE
          }
        }
      }
    )
  )

  # Process imported data
  observeEvent(dataImported(), {
    logDebug("Process imported data (%s)", meanId)
    m <- dataImported()
    storage.mode(m) <- class

    if (is.null(sdId)) {
      m <- asMatrix(m)

      m <- dropEmptyRows(m)
      m <- dropEmptyCols(m)

      m <- defaultMatrixNames(m, sampleName(rowVar()), sampleName(colVar()))

      setList(values[[meanId]], filterValues(), m)
    } else {
      colnames(m) <- gsub(".-.mean", "", colnames(m))
      split <- splitDoubleMatrix(m, rownames = !is.null(rownames(m)))
      split <- defaultMatrixNames(split, sampleName(rowVar()), sampleName(colVar()))

      setList(values[[meanId]], filterValues(), split[[1]])
      setList(values[[sdId]], filterValues(), split[[2]])
    }
  })

  # Get imported data
  dataImportedCov <- callModule(
    importData, "importCov",
    rowNames = reactive(namesCovVar()),
    colNames = reactive(namesCovVar()),
    customChecks = list(
      function() {
        function(df) {
          if (nrow(df) > 10000) {
            return("You cannot upload more than 10000 rows")
          } else {
            TRUE
          }
        }
      }
    )
  )

  # Process imported data
  observeEvent(dataImportedCov(), {
    logDebug("Process imported data (%s)", meanId)
    m <- dataImportedCov()
    storage.mode(m) <- class

    m <- asMatrix(m)

    m <- dropEmptyRows(m)
    m <- dropEmptyCols(m)

    setList(values[[covarianceId]], filterValuesCov(), m)
  })

  checkColNames <- reactive({
    function(data) {
      batchFilter <- unlist(lapply(filter, function(x) isTRUE(x$batch)))
      choices <- filter[batchFilter][[1]]$choices()
      batchNames <- if (attr(data, "includeRownames")) rownames(data) else unique(data[, 1])

      if (all(batchNames %in% choices)) {
        TRUE
      } else {
        paste(
          "Invalid values in first columns found: ",
          paste(batchNames[!batchNames %in% choices], collapse = ",")
        )
      }
    }
  })

  checkEmptyValues <- reactive({
    function(data) {
      vals <- data[, -1, drop = FALSE]
      mode(vals) <- "numeric"

      if (ncol(vals) < 2) {
        return(TRUE)
      }

      if (attr(data, "includeSd")) {
        vals <- vals[, seq(2, ncol(vals), by = 2, )]
      }

      if (any(is.na(vals) | vals == "")) {
        return("Found empty / non-numeric values.")
      }

      TRUE
    }
  })

  # Get batch imported data
  dataImportedBatch <- callModule(
    importData, "batchImport",
    rowNames = reactive(values[[rowVar()]]),
    colNames = reactive(character(0)),
    batch = TRUE,
    customChecks = list(
      checkColNames,
      checkEmptyValues
    )
  )

  observeEvent(dataImportedBatch(), {
    logDebug("Process imported data (%s)", meanId)
    fullm <- dataImportedBatch()
    includeSd <- attr(fullm, "includeSd")
    includeRownames <- attr(fullm, "includeRownames")

    if (includeRownames) {
      fullm <- cbind(rownames(fullm), fullm)
    }

    ms <- lapply(split(fullm, fullm[, 1]), matrix, ncol = ncol(fullm), dimnames = list(NULL, colnames(fullm)))
    batchFilter <- unlist(lapply(filter, function(x) isTRUE(x$batch)))

    lapply(ms, function(m) {
      batchValue <- unique(m[, 1])
      index <- filterValues()
      index[batchFilter] <- batchValue

      m <- m[, -1, drop = FALSE]

      if (includeRownames) {
        rownames(m) <- m[, 1]
        m <- m[, -1, drop = FALSE]
      }

      storage.mode(m) <- class

      if (is.null(sdId) || !includeSd) {
        m <- asMatrix(m)

        m <- dropEmptyRows(m)
        m <- dropEmptyCols(m)

        m <- defaultMatrixNames(m, sampleName(rowVar()), sampleName(colVar()))

        setList(values[[meanId]], index, m)

        if (!is.null(sdId)) {
          mEmpty <- m
          mEmpty[] <- NA
          setList(values[[sdId]], index, mEmpty)
        }
      } else {
        split <- splitDoubleMatrix(m, rownames = !is.null(rownames(m)))
        split <- defaultMatrixNames(split, sampleName(rowVar()), sampleName(colVar()))

        setList(values[[meanId]], index, split[[1]])
        setList(values[[sdId]], index, split[[2]])
      }
    })
  })

  checkColNamesCov <- reactive({
    function(data) {
      batchFilter <- unlist(lapply(filterCov, function(x) isTRUE(x$batch)))
      choices <- filterCov[batchFilter][[1]]$choices()
      batchNames <- if (attr(data, "includeRownames")) rownames(data) else unique(data[, 1])

      if (all(batchNames %in% choices)) {
        TRUE
      } else {
        paste(
          "Invalid values in first columns found: ",
          paste(batchNames[!batchNames %in% choices], collapse = ",")
        )
      }
    }
  })

  checkRowNamesCov <- reactive({
    function(data) {
      if (attr(data, "includeRownames")) {
        names <- data[, 1]
        if (!setequal(unique(names), namesCovVar())) {
          return(paste(
            "Invalid rownames found:",
            paste(names[!names %in% namesCovVar()], collapse = ",")
          ))
        }
      }

      TRUE
    }
  })

  checkColsCov <- reactive({
    function(data) {
      includeRownames <- isTRUE(attr(data, "includeRownames"))
      expectedLength <- length(namesCovVar()) + 1
      if (ncol(data) != expectedLength) {
        paste(
          "Wrong number of columns. ", expectedLength, " expected"
        )
      } else {
        TRUE
      }
    }
  })

  checkRowsCov <- reactive({
    function(data) {
      batchNames <- if (attr(data, "includeRownames")) rownames(data) else unique(data[, 1])

      ss <- split(data, batchNames)
      expectedLength <- length(namesCovVar())
      lengths <- lapply(unique(batchNames), function(n) {
        nrow(data[batchNames == n, , drop = FALSE])
      })

      if (!all(lengths == expectedLength)) {
        paste(
          "Wrong number of rows. ", expectedLength, " expected for every target."
        )
      } else {
        TRUE
      }
    }
  })

  dataImportedBatchCov <- callModule(
    importData, "batchImportCov",
    rowNames = namesCovVar,
    colNames = namesCovVar,
    batch = TRUE,
    customChecks = list(
      checkColNamesCov,
      checkRowNamesCov,
      checkColsCov,
      checkRowsCov
    )
  )

  observeEvent(dataImportedBatchCov(), {
    logDebug("Process imported data (%s)", meanId)
    fullm <- dataImportedBatchCov()
    includeSd <- attr(fullm, "includeSd")
    includeRownames <- attr(fullm, "includeRownames")

    if (includeRownames) {
      fullm <- cbind(rownames(fullm), fullm)
    }

    ms <- lapply(split(fullm, fullm[, 1]), matrix, ncol = ncol(fullm), dimnames = list(NULL, colnames(fullm)))
    batchFilter <- unlist(lapply(filterCov, function(x) isTRUE(x$batch)))

    lapply(ms, function(m) {
      batchValue <- unique(m[, 1])
      index <- filterValuesCov()
      index[batchFilter] <- batchValue

      m <- m[, -1, drop = FALSE]

      if (includeRownames) {
        rownames(m) <- m[, 1]
        m <- m[, -1, drop = FALSE]
      }

      storage.mode(m) <- class

      m <- asMatrix(m)

      m <- dropEmptyRows(m)
      m <- dropEmptyCols(m)

      setList(values[[covarianceId]], index, m)
    })
  })

  observeEvent(input$copyTarget, {
    batchFilter <- unlist(lapply(filter, function(x) isTRUE(x$batch)))
    indices <- as.matrix(expand.grid(filterChoices()))
    templateIndices <- indices[indices[, batchFilter] == filterValues()[batchFilter], , drop = FALSE]
    for (i in seq_len(nrow(indices))) {
      for (j in seq_len(nrow(templateIndices))) {
        index <- indices[i, ]
        templateIndex <- templateIndices[j, ]
        if (all(index[!batchFilter] == templateIndex[!batchFilter]) && all(index[batchFilter] != templateIndex[batchFilter])) {
          setList(values[[meanId]], index, getList(values[[meanId]], templateIndex))

          if (!is.null(sdId)) {
            setList(values[[sdId]], index, getList(values[[sdId]], templateIndex))
          }
        }
      }
    }
  })


  observeEvent(input$copyTargetCov, {
    batchFilter <- unlist(lapply(filterCov, function(x) isTRUE(x$batch)))
    indices <- as.matrix(expand.grid(filterChoicesCov()))
    templateIndices <- indices[indices[, batchFilter] == filterValuesCov()[batchFilter], , drop = FALSE]
    for (i in seq_len(nrow(indices))) {
      for (j in seq_len(nrow(templateIndices))) {
        index <- indices[i, ]
        templateIndex <- templateIndices[j, ]
        if (all(index[!batchFilter] == templateIndex[!batchFilter]) && all(index[batchFilter] != templateIndex[batchFilter])) {
          setList(values[[covarianceId]], index, getList(values[[covarianceId]], templateIndex))
        }
      }
    }
  })


  ## -- Export
  tableData <- reactive({
    function() {
      if (is.null(sdId)) {
        meanId()
      } else {
        data <- combineDoubleMatrix(meanData(), sdData())
        colnames(data) <- gsub("\\|\\|(mean|sd).*", " - \\1", colnames(data))
        data
      }
    }
  })
  callModule(exportData, "export", tableData)

  tableDataCov <- reactive({
    function() {
      covarianceData()
    }
  })
  callModule(exportData, "exportCov", tableDataCov)

  # Process distribution changes
  observeEvent(c(input$distribution, input$showCov), {
    logDebug("Process distribution change from UI -> values (%s)", meanId)
    req(distributionId)
    setList(
      values[[distributionId]],
      filterValuesDistribution(),
      input$distribution
    )
    filterIds <- lapply(filter, `[[`, "id")
    filterCovIds <- lapply(filterCov, `[[`, "id")

    ff <- setdiff(filterCovIds, filterIds)

    # show and hide showCov field
    if (input$distribution != "multivariate-normal") {
      hide("showCov")
    } else {
      show("showCov")
    }


    fun <- if (input$distribution == "multivariate-normal" && (is.null(input$showCov) || input$showCov == TRUE)) {
      show
    } else {
      hide
    }

    fun("covariance")
    fun("copyCov")
    fun("pasteCov")
    fun("exportCov-export")
    fun("importCov-openPopup")
    fun("pasteModeCov")

    lapply(ff, fun)
  })

  # batch button
  observe({
    visible <- (input$distribution == "multivariate-normal" && (is.null(input$showCov) || input$showCov == TRUE))

    showBatchButton <- lapply(filterCov, function(f) {
      (is.null(f$hide) || !f$hide()) && isTRUE(f$batch)
    })

    if (any(unlist(showBatchButton)) && visible) {
      show("batchImportContainerCov")
    } else {
      hide("batchImportContainerCov")
    }
  })

  observe({
    req(distributionId)
    logDebug("Process distribution change from values -> UI (%s)", meanId)
    updateSelectInput(
      session, "distribution",
      selected = getList(values[[distributionId]], filterValuesDistribution())
    )
  })
}

isEmpty <- function(x) {
  is.null(x) | is.na(x) | trimws(x) == ""
}


#' Remove Line
#' 
#' Removes the named row or column from matrix
#' 
#' @param matrix matrix
#' @param type character type of the line, either "row" or "column"
#' @param name character name of row or column
removeLine <- function(matrix, type, name) {
  if (type == "row") {
    i <- rownames(matrix) == name
    matrix <- matrix[!i, , drop = FALSE]
  } else {
    i <- colnames(matrix) == name
    matrix <- matrix[, !i, drop = FALSE]
  }
  
  return(matrix)
}
