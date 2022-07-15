#' Concentrations UI
#'
#' UI of the data - concentrations
#'
#' @param id id of module
concentrationsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
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
                     values = values(),
                     events = events(),
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
                       values()$targetNames
                     } else {
                       values()$fractionNames
                     }),
                     filter = list(
                       list(
                         id = "obsvn",
                         choices = reactive(sourceObsvnFilterChoices()),
                         hide = reactive(sourceObsvnFilterHide()),
                         distribution = FALSE,
                         batch = TRUE
                       )
                     ),
                     filterCov = list(
                       list(
                         id = "obsvn",
                         choices = reactive(sourceObsvnFilterChoices()),
                         hide = reactive(sourceObsvnFilterHide()),
                         batch = TRUE
                       )
                     )
                   )
                 })
  }
