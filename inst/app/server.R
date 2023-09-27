library(ReSources)
library(yaml)

options(shiny.maxRequestSize = 200*1024^2)

# load config variables
configFile <- system.file("config.yaml", package = "ReSources")
appConfig <- yaml::read_yaml(configFile)

server <- function(input, output, session) {
  savedMaps <- reactiveVal(list())
  fruitsData <- reactiveVal(list(event = NULL, data = NULL))
  isoData <- reactiveVal(NULL)
  isoDataExport <- reactiveVal(list(event = NULL, data = NULL))

  observe({
    hideTab("tab", "model2D")
  })

  observeEvent(isoDataExport()$event, {
    isoData(isoDataExport()$data)
    showTab("tab", "model2D", select = TRUE)
  })

  shiny::callModule(fruitsTab, "fruits", isoDataExport = isoDataExport, config = appConfig)

  if (isoInstalled()) {
    callModule(MpiIsoApp::modelResults2D, "model2D", isoData = isoData,
               savedMaps = savedMaps, fruitsData = fruitsData)
  }

  observeEvent(input$getHelp, {
    showModal(modalDialog(
      title = "Help",
      easyClose = TRUE,
      getHelp(input$tab),
      footer = modalButton("Ok")
    ))
  })
}
