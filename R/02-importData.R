loadData <- function(file, type, sep = ",", dec = ".", rownames = FALSE) {
  encTry <- as.character(guess_encoding(file)[1, 1])
  if (type == "xlsx") {
    xlsSplit <- strsplit(file, split = "\\.")[[1]]
    if (xlsSplit[length(xlsSplit)] == "xls") {
      type <- "xls"
    }
  }
  data <- switch(type,
    csv = suppressWarnings({
      read.csv(file,
        sep = sep, dec = dec, stringsAsFactors = FALSE, row.names = NULL,
        fileEncoding = encTry
      )
    }),
    txt = suppressWarnings({
      read.csv(file,
        sep = sep, dec = dec, stringsAsFactors = FALSE, row.names = NULL,
        fileEncoding = encTry
      )
    }),
    xlsx = read.xlsx(file),
    xls = suppressWarnings({
      readxl::read_excel(file)
    })
  )
  if (is.null(data)) {
    return(NULL)
  }

  if (is.null(dim(data))) {
    stop("Could not determine dimensions of data")
    return(NULL)
  }

  if (any(dim(data) == 0)) {
    stop("Number of rows or columns equal to 0")
    return(NULL)
  }

  if (rownames) {
    rn <- data[, 1]
    data <- as.matrix(data[, -1, drop = FALSE])
    rownames(data) <- rn
  }


  return(data)
}

# ui function for import data
importDataUI <- function(id, label = "Import Data") {
  ns <- NS(id)
  actionButton(ns("openPopup"), label)
}

# server function to import data
importDataServer <- function(id,
                             rowNames = NULL,
                             colNames = NULL,
                             batch = FALSE,
                             customChecks = list()) {
  moduleServer(id,
  function(input, output, session) {
  ns <- session$ns

  values <- reactiveValues(
    warnings = list(),
    fileImportSuccess = NULL,
    dataImport = NULL,
    data = NULL
  )

  observeEvent(input$openPopup, ignoreNULL = TRUE, {
    reset("file")
    values$fileImportWarning <- NULL
    values$fileImportSuccess <- NULL
    values$dataImport <- NULL
    values$data <- NULL

    showModal(importDataDialog(ns = ns, batch = batch))
  })

  observeEvent(list(
    input$file,
    input$type,
    input$colSep,
    input$decSep,
    input$rownames,
    input$includeSd,
    input$colnames
  ), {
    values$dataImport <- NULL

    inFile <- input$file

    if (is.null(inFile)) {
      return(NULL)
    }

    values$warnings <- list()
    values$fileImportSuccess <- NULL

    df <- tryCatch(
      loadData(
        inFile$datapath, input$type, input$colSep, input$decSep,
        isTRUE(input$rownames)
      ),
      error = function(e) {
        values$warnings <- c(values$warnings, "Could not read in file.")
        NULL
      },
      warning = function(w) {
        values$warnings <- c(values$warnings, "Could not read in file.")
        NULL
      }
    )

    if (is.null(df)) {
      return(NULL)
    }

    df <- as.matrix(df)

    attr(df, "includeSd") <- isTRUE(input$includeSd)
    attr(df, "includeRownames") <- isTRUE(input$rownames)

    ## set colnames
    if (!input$colnames && !is.null(colNames)) {
      colnames(df) <- rep("", ncol(df))
      mini <- min(length(colNames()), ncol(df))
      colnames(df)[seq_len(mini)] <- colNames()[seq_len(mini)]
    }

    ## Import technically successful
    values$dataImport <- df

    values$headData <- lapply(head(as.data.frame(df)), function(z) {
      if (is.character(z)) {
        substr(z, 1, 50)
      } else {
        z
      }
    })[1:min(ncol(df), 5)]


    ## Import valid?
    lapply(customChecks, function(fun) {
      res <- fun()(df)
      if (!isTRUE(res)) {
        values$warnings <- c(values$warnings, res)
      }
    })

    if (length(values$warnings) > 0) {
      return(NULL)
    }

    values$fileImportSuccess <- "Data import was successful"
  })

  output$warning <- renderUI(tagList(lapply(values$warnings, tags$p)))
  output$success <- renderText(values$fileImportSuccess)

  output$preview <- renderTable(values$headData,
    bordered = TRUE,
    rownames = TRUE, colnames = TRUE
  )

  observeEvent(input$cancel, {
    removeModal()
  })

  observeEvent(input$accept, {
    removeModal()

    if (length(values$warnings) == 0) {
      values$data <- values$dataImport
    }
  })

  return(reactive(values$data))
  })
}

# import data dialog ui
importDataDialog <- function(ns, batch = FALSE) {
  modalDialog(
    title = "Import Data",
    footer = tagList(
      actionButton(ns("cancel"), "Cancel"),
      actionButton(ns("accept"), "Accept")
    ),
    fileInput(ns("file"), "File"),
    selectInput(
      ns("type"),
      "File type",
      choices = c("xlsx", "csv"),
      selected = "xlsx"
    ),
    conditionalPanel(
      condition = paste0("input.type == 'csv'"),
      div(
        style = "display: inline-block;horizontal-align:top; width: 80px;",
        textInput(ns("colSep"), "column separator:", value = ",")
      ),
      div(
        style = "display: inline-block;horizontal-align:top; width: 80px;",
        textInput(ns("decSep"), "decimal separator:", value = ".")
      ),
      ns = ns
    ),
    checkboxInput(ns("colnames"), "First row contains colnames"),
    checkboxInput(ns("rownames"), paste(if (batch) "Second" else "First", "column contains rownames")),
    if (batch) checkboxInput(ns("includeSd"), "Uncertainties are included", value = TRUE),
    helpText(
      "The first row in your file need to contain variable names."
    ),
    if (batch) {
      helpText(
        "The first column in your file need to contain the observation names from the target table."
      )
    },
    div(class = "text-danger", uiOutput(ns("warning"))),
    div(class = "text-success", textOutput(ns("success"))),
    tableOutput(ns("preview"))
  )
}
