#' Target Values UI
#'
#' UI of the data - target values tab
#'
#' @param id id of module
#' @param title (character) title of module
targetValuesUI <- function(id, title = NULL) {
  ns <- NS(id)
  
  tagList(
    tags$h4(title),
    fruitsMatrixFilter(
      scope = ns("targetValues"),
      id = "term",
      label = "Term"
    ),
    fruitsMatrixDistribution(scope = ns("targetValues")),
    fruitsMatrixInput(ns("targetValues"), "obsvnNames", "targetNames"),
    checkboxInput(ns("targetOffset"), "Include target offset",
                  value = TRUE),
    conditionalPanel(
      condition = "input.targetOffset == true",
      fruitsMatrixInput(
        ns("weightOffset"),
        "targetNames",
        "offsetNames",
        fixedCols = "Offset"
      ),
      ns = ns
    ),
    checkboxInput(ns("targetValuesShowCovariates"), "Enter Covariates"),
    conditionalPanel(
      condition = "input.targetValuesShowCovariates == true",
      ns = ns,
      fruitsMatrixInput(
        ns("targetValuesCovariates"),
        "obsvnNames",
        "covariateNames",
        double = FALSE,
        class = "character"
      )
    ),
    fruitsMatrixFilter(
      scope = ns("targetValues"),
      id = "obsvn",
      label = "Observation - Target Covariance Matrix"
    ),
    fruitsMatrixInput(
      scope = ns("targetValues"),
      row = "targetNames",
      col = "targetNames",
      cov = TRUE
    ),
    checkboxInput(
      ns("targetValuesShowCoordinates"),
      "Coordinates & chronology"
    ),
    conditionalPanel(
      condition = "input.targetValuesShowCoordinates == true",
      ns = ns,
      fruitsMatrixInput(
        ns("exportCoordinates"),
        "obsvnNames",
        "coordinateNames",
        double = FALSE,
        fixedCols = c(
          "longitude",
          "latitude",
          "LowerLimit/Mean/Point",
          "UpperLimit/SD"
        )
      )
    )
  )
}


#' Target Values Server
#'
#' Server function of the data - target values
#' @param id id of module
#' @param values values
#' @param events events
#' @param termChoices termChoices
#' @param modelType (reactive) character id, e.g. "1"
targetValuesServer <-
  function(id,
           values,
           events,
           termChoices,
           modelType) {
    moduleServer(id,
                 function(input, output, session) {
                   ## TargetValues - callModule fruitsMatrix ----
                   callModule(
                     fruitsMatrix,
                     "targetValues",
                     values = values,
                     events = events,
                     meanId = "obsvn",
                     sdId = "obsvnError",
                     distributionId = "obsvnDistribution",
                     covarianceId = "targetValuesCovariance",
                     row = "obsvnNames",
                     col = "targetNames",
                     namesCov = reactive(values$targetNames),
                     filter = list(list(id = "term", choices = termChoices)),
                     filterCov = list(
                       list(id = "term", choices = termChoices),
                       list(
                         id = "obsvn",
                         choices = reactive(values$obsvnNames),
                         batch = TRUE
                       )
                     )
                   )
                   
                   ## WeightOffset - callModule fruitsMatrix ----
                   callModule(
                     fruitsMatrix,
                     "weightOffset",
                     values = values,
                     events = events,
                     meanId = "weightOffset",
                     sdId = "weightOffsetUncert",
                     row = "targetNames",
                     col = "offsetNames",
                     fixedCols = "Offset"
                   )
                   
                   observeEvent(values$targetValuesShowCovariates, {
                     logDebug("Entering observeEvent(values$targetValuesShowCovariates)")
                     updateCheckboxInput(session,
                                         "targetValuesShowCovariates",
                                         value = values$targetValuesShowCovariates
                     )
                   })
                   
                   observeEvent(input$targetValuesShowCovariates, {
                     logDebug("Entering observeEvent(input$targetValuesShowCovariates)")
                     if (!identical(
                       input$targetValuesShowCovariates,
                       values$targetValuesShowCovariates
                     )) {
                       values$targetValuesShowCovariates <-
                         input$targetValuesShowCovariates
                     }
                     if (input$targetValuesShowCovariates == TRUE &
                         modelType() == "1") {
                       values$modelType <- "2"
                     }
                   })
                   
                   ## TargetValuesCovariates - callModule fruitsMatrix ----
                   callModule(
                     fruitsMatrix,
                     "targetValuesCovariates",
                     values = values,
                     events = events,
                     meanId = "targetValuesCovariates",
                     row = "obsvnNames",
                     col = "covariateNames",
                     class = "character"
                   )
                   
                   observeEvent(modelType(), {
                     logDebug("Entering observeEvent(modelType())")
                     values$modelType <- modelType()
                     
                     if (modelType() == "1" & (input$targetValuesShowCovariates == TRUE)) {
                       updateCheckboxInput(session, "targetValuesShowCovariates", value = FALSE)
                     }
                   })
                   
                   
                 })
  }


#' Components UI
#'
#' UI of the data - components tab
#'
#' @param id id of module
#' @param title (character) title of module
componentsUI <- function(id, title = NULL) {
  ns <- NS(id)
  
  tagList(
    tags$h4(title),
    fruitsMatrixDistribution(
      scope = ns("weights"),
      choices = c("constant", "normal", "log-normal")
    ),
    fruitsMatrixInput(ns("weights"), "targetNames", "fractionNames")
  )
}


#' Components Server
#'
#' Server function of the data - components
#' @param id id of module
#' @param values values
#' @param events events
componentsServer <-
  function(id,
           values,
           events) {
    moduleServer(id,
                 function(input, output, session) {
                   ## Weights - callModule fruitsMatrix ----
                   callModule(
                     fruitsMatrix,
                     "weights",
                     values = values,
                     events = events,
                     meanId = "weights",
                     sdId = "weightsUncert",
                     row = "targetNames",
                     col = "fractionNames",
                     distributionId = "weightDistribution"
                   )
                 })
  }


#' Sources UI
#'
#' UI of the data - sources tab
#'
#' @param id id of module
#' @param title (character) title of module
sourcesUI <- function(id, title = NULL) {
  ns <- NS(id)
  
  tagList(
    tags$h4(title),
    div(
      fruitsMatrixFilter(
        scope = ns("source"),
        id = "obsvn",
        label = "Observation"
      )
    ),
    fruitsMatrixFilter(
      scope = ns("source"),
      id = "term",
      label = "Term"
    ),
    fruitsMatrixDistribution(scope = ns("source")),
    div(fruitsMatrixFilter(
      scope = ns("source"),
      id = "target",
      label = "Proxy"
    )),
    fruitsMatrixInput(
      scope = ns("source"),
      row = "sourceNames",
      col = "targetNames"
    ),
    checkboxInput(
      ns("includeSourceOffset"),
      "Include source specific offsets",
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.includeSourceOffset == true",
      ns = ns,
      fruitsMatrixFilter(
        scope = ns("sourceOffset"),
        id = "obsvn",
        label = "Observation"
      ),
      fruitsMatrixFilter(
        scope = ns("sourceOffset"),
        id = "target",
        label = "Proxy"
      ),
      fruitsMatrixInput(ns("sourceOffset"), row = "sourceNames", col = "targetNames")
    ),
    fruitsMatrixInput(
      ns("source"),
      row = "sourceNames",
      col = "sourceNames",
      cov = TRUE,
      toggleCov = TRUE
    )
  )
}


#' Sources Server
#'
#' Server function of the data - sources
#' @param id id of module
#' @param values values
#' @param events events
#' @param hideTargetFilter (reactive) logical, hideTargetFilter
#' @param termChoices termChoices
#' @param sourceObsvnFilterChoices sourceObsvnFilterChoices
#' @param sourceObsvnFilterHide sourceObsvnFilterHide
sourcesServer <-
  function(id,
           values,
           events,
           hideTargetFilter,
           termChoices,
           sourceObsvnFilterChoices,
           sourceObsvnFilterHide) {
    moduleServer(id,
                 function(input, output, session) {
                   ## Source - callModule fruitsMatrix ----
                   ns <- session$ns
                   
                   sourceCovNames <- reactive({
                     if (!hideTargetFilter()) {
                       apply(expand.grid(values$fractionNames, values$targetNames),
                             1,
                             paste,
                             collapse = "-")
                     } else {
                       values$targetNames
                     }
                   })
                   
                   callModule(
                     fruitsMatrix,
                     "source",
                     values = values,
                     events = events,
                     meanId = "source",
                     sdId = "sourceUncert",
                     row = "sourceNames",
                     col = reactive(if (hideTargetFilter()) {
                       "targetNames"
                     } else {
                       "fractionNames"
                     }),
                     namesCov = sourceCovNames,
                     distributionId = "sourceDistribution",
                     covarianceId = "sourceCovariance",
                     filter = list(
                       list(id = "term", choices = termChoices),
                       list(
                         id = "obsvn",
                         choices = sourceObsvnFilterChoices,
                         hide = sourceObsvnFilterHide,
                         distribution = FALSE,
                         batch = TRUE
                       ),
                       list(
                         id = "target",
                         choices = reactive(values$targetNames),
                         hide = hideTargetFilter,
                         distribution = FALSE
                       )
                     ),
                     filterCov = list(
                       list(id = "term", choices = termChoices),
                       list(
                         id = "obsvn",
                         choices = sourceObsvnFilterChoices,
                         hide = sourceObsvnFilterHide,
                         batch = TRUE
                       )
                     )
                   )
                   
                   ## Hide Input for 0 weights
                   observe({
                     logDebug("Entering observe() (values$modelWeights)")
                     
                     if (values$modelWeights) {
                       zeroTarget <- row(values$weights)[values$weights == 0]
                       zeroFraction <-
                         col(values$weights)[values$weights == 0]
                       visible <-
                         input[["source-target"]] == values$targetNames[zeroTarget]
                       showAllColumns(ns("source-table"))
                       if (length(visible) > 0 &&
                           !any(is.na(visible)) && any(visible)) {
                         idFrac <-
                           which(colnames(values$weights) %in% values$fractionNames[zeroFraction])
                         if (length(idFrac) > 0) {
                           lapply(idFrac, hideColumn, id = ns("source-table"))
                         }
                       }
                     } else {
                       showAllColumns(ns("source-table"))
                     }
                   })
                   
                   ## SourceOffset - callModule fruitsMatrix ----
                   callModule(
                     fruitsMatrix,
                     "sourceOffset",
                     values = values,
                     events = events,
                     meanId = "sourceOffset",
                     sdId = "sourceOffsetUncert",
                     row = "sourceNames",
                     col = reactive(if (hideTargetFilter()) {
                       "targetNames"
                     } else {
                       "fractionNames"
                     }),
                     filter = list(
                       list(
                         id = "obsvn",
                         choices = sourceObsvnFilterChoices,
                         hide = sourceObsvnFilterHide,
                         batch = TRUE
                       ),
                       list(
                         id = "target",
                         choices = reactive(values$targetNames),
                         hide = hideTargetFilter
                       )
                     )
                   )
                 })
  }


#' Concentrations UI
#'
#' UI of the data - concentrations tab
#'
#' @param id id of module
#' @param title (character) title of module
concentrationsUI <- function(id, title = NULL) {
  ns <- NS(id)
  
  tagList(
    tags$h4(title),
    div(
      fruitsMatrixFilter(
        scope = ns("concentration"),
        id = "obsvn",
        label = "Observation"
      )
    ),
    fruitsMatrixDistribution(scope = ns("concentration")),
    fruitsMatrixInput(ns("concentration"), row = "sourceNames", col = "targetNames"),
    fruitsMatrixInput(
      ns("concentration"),
      row = "targetNames",
      col = "targetNames",
      cov = TRUE,
      toggleCov = TRUE
    )
  )
}


#' Concentrations Server
#'
#' Server function of the data - concentrations
#' @param id id of module
#' @param values values
#' @param events events
#' @param hideTargetFilter hideTargetFilter
#' @param sourceObsvnFilterChoices sourceObsvnFilterChoices
#' @param sourceObsvnFilterHide sourceObsvnFilterHide
concentrationsServer <-
  function(id,
           values,
           events,
           hideTargetFilter,
           sourceObsvnFilterChoices,
           sourceObsvnFilterHide) {
    moduleServer(id,
                 function(input, output, session) {
                   ## Concentration - callModule fruitsMatrix ----
                   callModule(
                     fruitsMatrix,
                     "concentration",
                     values = values,
                     events = events,
                     meanId = "concentration",
                     sdId = "concentrationUncert",
                     row = "sourceNames",
                     col = reactive(if (hideTargetFilter()) {
                       "targetNames"
                     } else {
                       "fractionNames"
                     }),
                     distributionId = "concentrationDistribution",
                     covarianceId = "concentrationCovariance",
                     namesCov = reactive(if (hideTargetFilter()) {
                       values$targetNames
                     } else {
                       values$fractionNames
                     }),
                     filter = list(
                       list(
                         id = "obsvn",
                         choices = sourceObsvnFilterChoices,
                         hide = sourceObsvnFilterHide,
                         distribution = FALSE,
                         batch = TRUE
                       )
                     ),
                     filterCov = list(
                       list(
                         id = "obsvn",
                         choices = sourceObsvnFilterChoices,
                         hide = sourceObsvnFilterHide,
                         batch = TRUE
                       )
                     )
                   )
                 })
  }
