#' Plotly Plot UI
#'
#'
#' @param id module id
#'
#' @return tagList
#' @export
plotlyPlotUI <- function(id, label) {
  ns <- NS(id)
  tagList(
    pickerInput(
      inputId = ns("select"),
      label = sprintf("Select up to three %s", label),
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    plotlyOutput(outputId = ns("plot")),
    plotExportButton(ns("exportplot")),
    helpText(
      "Please export plot using the camera symbol at the top-right of the plot."
    )
  )
}

#' Server function for plotly plot module
#'
#' Backend for plotly plot module.
#'
#' @param id namespace id
#'
#' @export
plotlyPlotServer <- function(id, type) {
  moduleServer(id,
               function(input, output, session) {
                 
                 
                 callModule(
                   plotExport,
                   "exportPlot",
                   plotFun = plotFunCharacteristicsConc,
                   type = type,
                   plotly = TRUE
                 )
                 
               })
}



# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- shiny::fluidPage(
#   tagList(
#     shiny::navbarPage(
#       header = shinyTools::includeShinyToolsCSS(),
#       title = "test app",
#       theme = shinythemes::shinytheme("flatly"),
#       position = "fixed-top",
#       collapsible = TRUE,
#       id = "test"
#     ),
#     plotOutput("plot"),
#     shinyTools::plotRangesUI(id = "testMod", initRanges = list(xAxis = list(min = 0, max = 10, fromData = FALSE),
#                                                    yAxis = list(min = 0, max = 10, fromData = TRUE)))
#   )
# )
# 
# server <- function(input, output, session) {
#   testPlotFun <- function() {
#     data <- data.frame(
#       x = c(1, 2, 3, 4, 5),
#       y = c(2, 4, 1, 7, 3)
#     )
# 
#     ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
#       ggplot2::geom_point()
#   }
# 
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       shinyTools::formatRangesOfGGplot(ranges = thisRanges)
#   })
# 
#   thisRanges <- shinyTools::plotRangesServer("testMod",
#                                  type = "ggplot",
#                                  initRanges = list(xAxis = list(min = 0, max = 10, fromData = FALSE),
#                                                  yAxis = list(min = 0, max = 10, fromData = TRUE)))
# }
# 
# shiny::shinyApp(ui = ui, server = server)
