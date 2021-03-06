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
    tags$h4(label),
    textAreaInput(ns("notes"), "Notes"),
    checkboxInput(ns("onlyInputs"), "Store only data and model options"),
    downloadButton(ns("downloadModelFile"), "Download"),
    tags$br()
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
          version = packageVersion("ReSources")
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
#' @param label label of module
#'
#' @rdname uploadModel
#'
#' @export
uploadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    HTML("<br>"),
    tags$h4(label),
    fileInput(ns("uploadModel"), label = "Upload local model"),
    selectInput(
      ns("remoteModel"),
      label = "Select remote model",
      choices = dir(file.path(settings$pathToSavedModels)) %>%
        sub(pattern = '\\.zip$', replacement = ''),
      selected = NULL
    ),
    actionButton(ns("loadRemoteModel"), "Load Remote Model")#,
    #helpText("Remote models are only available on on https://isomemoapp.com")
  )
}


#' Server function upload model
#'
#' Backend for upload model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param values (reactive) list: Shiny input
#' @param model (reactive) output of the model
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#'
#' @export
uploadModel <-
  function(input,
           output,
           session,
           values,
           model,
           uploadedNotes) {
    pathToModel <- reactiveVal(NULL)
    
    observeEvent(input$uploadModel, {
      pathToModel(input$uploadModel$datapath)
    })
    
    observeEvent(input$loadRemoteModel, {
      pathToModel(file.path(
        settings$pathToSavedModels,
        paste0(input$remoteModel, ".zip")
      ))
    })
    
    observeEvent(pathToModel(), {
      logDebug("Entering observe() (Load Model from file)")
      
      res <- try({
        zip::unzip(pathToModel())
        modelImport <- readRDS("model.rds")
        uploadedNotes(readLines("README.txt"))
      })
      if (inherits(res, "try-error")) {
        shinyjs::alert(
          paste0(
            "Could not load file.\n",
            "The file to be uploaded should be a .zip file",
            " that contains the following files:",
            " \n help.html,\n model.rds,\n README.txt\n",
            "If you download a model it will exactly have this format."
          )
        )
        return()
      }
      
      if (!exists("modelImport")) {
        shinyjs::alert("File format not valid. Model object not found.")
        return()
      }
      
      
      if (is.null(modelImport$model)) {
        warningEmptyModel <-
          "Model object is empty, no model output available. Only data and model inputs will be uploaded. "
        model(NULL)
      } else {
        warningEmptyModel <- ""
        model(modelImport$model)
      }
      
      if (is.null(modelImport$values)) {
        warningEmptyInputs <-
          "Input values are empty, no inputs are updated. Only the model object will be uploaded. "
      } else {
        warningEmptyInputs <- ""
        for (name in names(modelImport$values)) {
          values[[name]] <- modelImport$values[[name]]
        }
      }
      
      rm(modelImport)
      
      alert(paste0(
        warningEmptyInputs,
        warningEmptyModel,
        "Upload completed."
      ))
      
      values$status <- values$statusSim <- "COMPLETED"
    })
  }
