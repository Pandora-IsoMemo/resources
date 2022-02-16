library(ReSources)

tagList(
  div(id = "resources-overlay"),
  shiny::navbarPage(
    title = paste("ReSources", packageVersion("ReSources")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    fruitsUI("fruits", "ReSources"),
    if (isoInstalled()) MpiIsoApp::modelResults2DUI("model2D", "Local Average Model (Iso Memo App)", asFruitsTab = TRUE)
    else NULL
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "ReSources/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "ReSources/removeName.css"),
    tags$script(src = "ReSources/overlay.js"),
    tags$script(src = "ReSources/shinyMatrix.js"),
    tags$script(src = "ReSources/priors.js"),
    tags$script(src = "ReSources/userEstimates.js"),
    tags$script(src = "ReSources/removeName.js"),
    tags$script(src = "ReSources/copyPaste.js"),
    if (isoInstalled()) tagList(
      tags$link(rel = "stylesheet", type = "text/css", href = "IsoMemo/custom.css"),
      tags$script(src = "IsoMemo/custom.js"),
      tags$script(src = "IsoMemo/shinyMatrix.js")
    ) else NULL
  ),
  div(
    id = "header-right",
    div(
      id = "logo-mpi",
      tags$a(href = "https://www.mpg.de/en",
             img(src = "ReSources/MPIlogo.png", alt = "Supported by the Max Planck society"),
             target = "_blank"
             )
    ),
    div(
      id = "logo-isomemo",
      tags$a(href = "https://isomemo.com/",
             img(src = "ReSources/IsoMemoLogo.png", alt = "IsoMemo"),
             target = "_blank"
             )
    ),
    div(
      id = "further-help",
      tags$button(
        onclick = "window.open('https://isomemo.com','_blank');",
        class = "btn btn-default",
        "Further Help"
      )
    ),
    div(
      id = "help",
      actionButton("getHelp", "?")
    )
  ),
  shinyjs::useShinyjs()
)
