#' Sources UI
#'
#' UI of the data - sources
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
#' @param hideTargetFilter hideTargetFilter
#' @param sourceCovNames sourceCovNames
#' @param termChoices termChoices
#' @param sourceObsvnFilterChoices sourceObsvnFilterChoices
#' @param sourceObsvnFilterHide sourceObsvnFilterHide
sourcesServer <-
  function(id,
           values,
           events,
           hideTargetFilter,
           sourceCovNames,
           termChoices,
           sourceObsvnFilterChoices,
           sourceObsvnFilterHide) {
    moduleServer(id,
                 function(input, output, session) {
                   ## Source - callModule fruitsMatrix ----
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
#' UI of the data - concentrations
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
