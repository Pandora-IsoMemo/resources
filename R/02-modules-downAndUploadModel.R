# #' Download model module
# #'
# #' UI function to download a zip file with notes and a list of models
# #'
# #' @param id id of module
# #' @param label label of module
# #'
# #' @rdname downloadModel
# #'
# #' @export
# downloadModelUI <- function(id, label) {
#   ns <- NS(id)
#   
#   tagList(
#     tags$strong(label),
#     textAreaInput(ns("notes"), "Notes"),
#     checkboxInput(ns("onlyInputs"), "Store only data and model options"),
#     downloadButton(ns("downloadModelFile"), "Save Model")
#   )
# }


# #' Server function download model
# #'
# #' Backend for download model module
# #'
# #' @param input shiny input
# #' @param output shiny output
# #' @param session shiny session
# #' @param values (reactive) list: Shiny input
# #' @param model (reactive) output of the model
# #' @param uploadedNotes (reactive) variable that stores content of README.txt
# #'
# #' @export
# downloadModel <-
#   function(input,
#            output,
#            session,
#            values,
#            model,
#            uploadedNotes) {
#     observe({
#       updateTextAreaInput(session, "notes", value = uploadedNotes())
#     })
#     
#     output$downloadModelFile <- downloadHandler(
#       filename = function() {
#         gsub("[ ]", "_", paste0(Sys.time(), "_fruitsModel", ".zip"))
#       },
#       content = function(file) {
#         logDebug(paste0("Entering ", session$ns(""), "downloadHandler()"))
#         
#         zipdir <- tempdir()
#         modelfile <- file.path(zipdir, "model.rds")
#         notesfile <- file.path(zipdir, "README.txt")
#         helpfile <- file.path(zipdir, "help.html")
#         
#         valuesExport <- reactiveValuesToList(values)
#         valuesExport <- valuesExport[allVariables()]
#         
#         if (input$onlyInputs) {
#           modelExport <- NULL
#         } else {
#           modelExport <- model()
#         }
#         
#         saveRDS(list(
#           values = valuesExport,
#           model = modelExport,
#           version = paste("ReSources", packageVersion("ReSources"))
#         ),
#         file = modelfile)
#         writeLines(input$notes, notesfile)
#         save_html(getHelp(input$tab), helpfile)
#         zipr(file, c(modelfile, notesfile, helpfile))
#         
#       }
#     )
#     
#   }
 
 
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
    `target-to-source offset` = "weightOffset",
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
