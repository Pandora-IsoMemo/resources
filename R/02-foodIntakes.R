foodIntakesButton <- function(id, title) {
  ns <- NS(id)

  actionButton(ns("openPopup"), title)
}

foodIntakes <- function(input, output, session, values) {
  foodIntakeData <- reactiveVal()

  defaultData <- reactive({
    matrix(as.numeric(NA), nrow = length(values$sourceNames), ncol = 2,
           dimnames = list(values$sourceNames, c("mean", "sd"))
    )
  })

  observeEvent(input$openPopup, {
    foodIntakeData(values$userDefinedAlphas)

    default <- if (length(values$userDefinedAlphas) > 0) values$userDefinedAlphas[[1]]
    else defaultData()

    default[is.na(default)] <- "";

    choices <- names(values$userDefinedAlphas)
    selected <- if (length(choices) > 0) choices[1]
    else NULL

    showModal(foodIntakesPopup(ns = session$ns, default = default, choices = choices,
                               selected = selected))
  })

  observeEvent(input$save, {
    values$userDefinedAlphas <- foodIntakeData()
  })

  observeEvent(input$ok, {
    removeModal()
  })

  observeEvent(input$data, {
    req(input$intake)
    dat <- foodIntakeData()
    dat[[input$intake]] <- input$data
    foodIntakeData(dat)
  })

  observeEvent(input$intake, {
    req(input$intake)
    updateMatrixInput(session, "data", foodIntakeData()[[input$intake]])
  })

  foodIntakeNames <- reactive({
    names(foodIntakeData())
  })

  observeEvent(foodIntakeNames(), ignoreNULL = TRUE, {
    selected <- if (input$intake %in% foodIntakeNames()) input$intake
    else if (length(foodIntakeNames() > 0)) foodIntakeNames()[1]
    else NULL

    updateSelectInput(session, "intake", choices = foodIntakeNames(), selected = selected)

    if (is.null(selected)) updateMatrixInput(session, "data", defaultData())
  })

  observeEvent(input$addIntake, {
    req(input$newIntake)
    dat <- foodIntakeData()

    if (input$newIntake %in% names(dat)) {
      shinyjs::alert("Name already exists")
      return()
    }
    if (input$newIntake == "") {
      shinyjs::alert("Name cannot be empty exists")
      return()
    }

    dat[[input$newIntake]] <- defaultData()
    updateTextInput(session, "newIntake", value = "")
    foodIntakeData(dat)
  })

  observeEvent(input$removeIntake, {
    req(input$intake)
    dat <- foodIntakeData()
    dat[[input$intake]] <- NULL
    foodIntakeData(dat)
  })
  
  observe({
    req(values$sourceNames)
    if(is.null(input$intake) || !(input$intake %in% foodIntakeNames())){
      shinyjs::hide(id = "data")
    } else {
      shinyjs::show(id = "data")
    }
  })
}

foodIntakesPopup <- function(ns, default, choices, selected) {
  modalDialog(
    title = "Food Intakes",
    tags$h5("Please specify name of food intake first:"),
    div(
      style = "margin-bottom: 20px;",
      textInput(ns("newIntake"), "New Food Intake", value = "Food_Intake_1"),
      actionButton(ns("addIntake"), "Add Food Intake")
    ),
    tags$h5("Select a food intake and enter food proportion values:"),
    div(
      style = "margin-bottom: 20px;",
      div(
        style = "inline-block;",
        selectInput(ns("intake"), "Food Intake", choices = choices, selected = selected)
      )
    ),
    matrixInput(ns("data"), label = "Data", value = default,
                rows = list(names = TRUE), cols = list(names = TRUE), class = "numeric", copy = TRUE),
    div(
      style = "inline-block;",
      actionButton(ns("removeIntake"), "Remove Food Intake")
    ),
    footer = tagList(
      actionButton(ns("save"), "Save"),
      actionButton(ns("ok"), "Ok")
    )
  )
}
