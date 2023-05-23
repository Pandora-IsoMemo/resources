#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname downloadModel
#'
#' @export
downloadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    tags$strong(label),
    textAreaInput(ns("notes"), "Notes"),
    checkboxInput(ns("onlyInputs"), "Store only data and model options"),
    downloadButton(ns("downloadModelFile"), "Save model")
  )
}


#' Server function download model
#'
#' Backend for download model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param values (reactive) list: Shiny input
#' @param model (reactive) output of the model
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#'
#' @export
downloadModel <-
  function(input,
           output,
           session,
           values,
           model,
           uploadedNotes) {
    observe({
      updateTextAreaInput(session, "notes", value = uploadedNotes())
    })
    
    output$downloadModelFile <- downloadHandler(
      filename = function() {
        gsub("[ ]", "_", paste0(Sys.time(), "_fruitsModel", ".zip"))
      },
      content = function(file) {
        logDebug(paste0("Entering ", session$ns(""), "downloadHandler()"))
        
        zipdir <- tempdir()
        modelfile <- file.path(zipdir, "model.rds")
        notesfile <- file.path(zipdir, "README.txt")
        helpfile <- file.path(zipdir, "help.html")
        
        valuesExport <- reactiveValuesToList(values)
        valuesExport <- valuesExport[allVariables()]
        
        if (input$onlyInputs) {
          modelExport <- NULL
        } else {
          modelExport <- model()
        }
        
        saveRDS(list(
          values = valuesExport,
          model = modelExport,
          version = paste("ReSources", packageVersion("ReSources"))
        ),
        file = modelfile)
        writeLines(input$notes, notesfile)
        save_html(getHelp(input$tab), helpfile)
        zipr(file, c(modelfile, notesfile, helpfile))
        
      }
    )
    
  }


#' Upload model module
#'
#' UI function to upload a zip file with notes and a list of models
#'
#' @param id id of module
#'
#' @rdname uploadModel
#'
#' @export
uploadModelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("uploadModel"), label = "Load local model"),
    remoteModelsUI(ns("remoteModels")),
    tags$br(),
    tags$br()
  )
}


#' Server function upload model
#'
#' Backend for upload model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param model (reactive) output of the model
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#' @param reset (reactive) result of reset button
#'
#' @export
uploadModel <-
  function(input,
           output,
           session,
           model,
           uploadedNotes,
           reset) {
    pathToModel <- reactiveVal(NULL)
    uploadedValues <- reactiveVal(list())
    
    observeEvent(input$uploadModel, {
      logDebug(paste0(
        "Entering ",
        session$ns(""),
        "observeEvent(input$uploadModel)"
      ))
      
      pathToModel(input$uploadModel$datapath)
    })
    
    pathToRemote <- remoteModelsServer("remoteModels",
                                       githubRepo = "resources",
                                       rPackageName = "ReSources",
                                       rPackageVersion = "ReSources" %>%
                                         packageVersion() %>%
                                         as.character(),
                                       resetSelected = reactive(reset() >= 1))
    
    observeEvent(pathToRemote(), {
      # reset values
      uploadedValues(list())
      pathToModel(pathToRemote())
    })
    
    observeEvent(pathToModel(), {
      logDebug("Entering (%s) observe() (Load Model)", session$ns(""))
      
      alertType <- "error"
      
      res <- try({
        zip::unzip(pathToModel())
        modelImport <- readRDS("model.rds")
        uploadedNotes(readLines("README.txt"))
      })
      
      if (inherits(res, "try-error")) {
        shinyalert(
          title = "Could not load file.",
          text = paste(
            "The file to be uploaded should be a .zip file",
            "that contains the following files:",
            "help.html, model.rds, README.txt. ",
            "If you download a model it will exactly have this format."
          ),
          type = alertType
        )
        return()
      }
      
      if (!exists("modelImport")) {
        shinyalert(title = "Could not load file.",
                   text = "File format not valid. Model object not found.",
                   type = alertType)
        return()
      }
      
      if (is.null(modelImport$values)) {
        warningInputs <-
          "No input data or model selection parameters found."
        
        alertType <- "warning"
      } else {
        warningInputs <-
          "Input data and model selection parameters loaded. "
        
        emptyTables <- checkForEmptyTables(modelImport$values)
        if (length(emptyTables) > 0) {
          warningInputs <- paste(
            warningInputs,
            "Following tables contain only empty values: <br/>",
            paste0(emptyTables, collapse = ", "),
            " "
          )
          alertType <- "warning"
        } else {
          alertType <- "success"
        }
        
        uploadedValues(modelImport$values)
      }
      
      if (!is.null(modelImport$version)) {
        uploadedVersion <- paste("Saved version:", modelImport$version, ".")
      } else {
        uploadedVersion <- ""
      }
      
      if (is.null(modelImport$model)) {
        warningModel <-
          "No model results found. "
        alertType <- "warning"
        
        model(NULL)
      } else {
        warningModel <- "Model results loaded. "
        # no update of alertType, do not overwrite a warning
        model(modelImport$model)
      }
      
      # clean up
      rm(modelImport)
      if (file.exists("model.rds")) file.remove("model.rds")
      if (file.exists("README.txt")) file.remove("README.txt")
      if (file.exists("help.html")) file.remove("help.html")
      
      dataLoadedAlert(warningInputs, warningModel, uploadedVersion, alertType)
    })
    
    uploadedValues
  }


checkForEmptyTables <- function(values) {
  isTableEmpty <- function(tbl) {
    all(sapply(tbl, function(x)
      all(is.na(x))))
  }
  
  ifEnabled <- function(checkboxValue, matrixName) {
    if (!checkboxValue)
      return(NULL)
    
    matrixName
  }
  
  tablesInUI <- c(
    Target = "obsvn",
    `target-to-source offset` = "weightsOffset",
    Covariates = "targetValuesCovariates",
    `Coordinates & chronology` = "exportCoordinates",
    Components = "weights",
    Sources = "source",
    `source specific offset` = "sourceOffset",
    Concentrations = "concentration"
  )
  checkboxesInUI <- c(
    `Include target offset` = "targetOffset",
    `Enter Covariates` = "targetValuesShowCovariates",
    `Coordinates & chronology` = "targetValuesShowCoordinates",
    `Include source specific offsets` = "includeSourceOffset",
    `Include components` = "modelWeights",
    `Include concentrations` = "modelConcentrations"
  )
  
  namesTablesToCheck <- c(
    "Target",
    ifEnabled(values$targetOffset, "target-to-source offset"),
    ifEnabled(values$targetValuesShowCovariates, "Covariates"),
    ifEnabled(
      values$targetValuesShowCoordinates,
      "Coordinates & chronology"
    ),
    ifEnabled(values$modelWeights, "Components"),
    "Sources",
    ifEnabled(values$includeSourceOffset, "source specific offset"),
    ifEnabled(values$modelConcentrations, "Concentrations")
  )
  
  emptyTables <- c()
  for (i in tablesInUI[namesTablesToCheck]) {
    if (isTableEmpty(values[[i]])) {
      emptyTables <- c(emptyTables, i)
    }
  }
  
  namesEmptyTables <- names(tablesInUI[tablesInUI %in% emptyTables])
  
  return(namesEmptyTables)
}

dataLoadedAlert <-
  function(warningInputs,
           warningModel,
           uploadedVersion,
           alertType) {
    shinyalert(
      title = "Upload finished.",
      text = HTML(paste0(
        #"<div align='left'>",
        "<p>",
        paste(warningInputs,
              warningModel,
              uploadedVersion,
              sep = "</p><br/><p>"),
        "</p>"#,
        #"</div>"
      )),
      type = alertType,
      html = TRUE
    )
  }
