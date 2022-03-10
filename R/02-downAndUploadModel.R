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
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#'
#' @export
downloadModel <- function(input, output, session, values, uploadedNotes){
  
  observe({
    updateTextAreaInput(session, "notes", value = uploadedNotes())
  })
  
  output$downloadModelFile <- downloadHandler(
    filename = function() {
      gsub("[ ]", "_", paste0(Sys.time(), "_fruitsModel", ".zip"))
    },
    content = function(file) {
      zipdir <- tempdir()
      modelfile <- file.path(zipdir, "model.Rdata")
      notesfile <- file.path(zipdir, "README.txt")
      helpfile <- file.path(zipdir, "help.html")
      
      model <- reactiveValuesToList(values)
      model <- model[allVariables()]
      model$version <- packageVersion("ReSources")
      save(model, file = modelfile)
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
      choices = dir(file.path("./predefinedModels")),
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
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#'
#' @export
uploadModel <- function(input, output, session, values, uploadedNotes){
  pathToModel <- reactiveVal(NULL)
  
  observeEvent(input$uploadModel, {
    pathToModel(input$uploadModel$datapath)
  })
  
  observeEvent(input$loadRemoteModel, {
    pathToModel(file.path("./predefinedModels", input$remoteModel))
  })
  
  observeEvent(pathToModel(), {
    logDebug("Entering observe() (Load Model from file)")
    res <- try({
      zip::unzip(pathToModel())
      load("model.Rdata")
      uploadedNotes(readLines("README.txt"))
    })
    if (inherits(res, "try-error")) {
      shinyjs::alert("Could not load file.")
      return()
    }
    
    if (!exists("model")) {
      shinyjs::alert("File format not valid. Model object not found.")
      return()
    }
    
    for (name in names(model)) {
      values[[name]] <- model[[name]]
    }
    
    values$status <- values$statusSim <- "INITIALIZE"
    values$reset <- runif(1)
    values$obsvnNames <- unique(rownames(values$obsvn[["default"]]))
    
    alert("Model loaded")
  })
  
}
