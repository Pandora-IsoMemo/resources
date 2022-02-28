OxCalOutputUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    fluidRow(
      column(width = 3, selectInput(ns("terrestrialCurve"),
        label = "Terrestrial Curve",
        choices = NULL
      )),
      column(width = 2, conditionalPanel(
        condition = "input.terrestrialCurve == '3'",
        ns = ns,
        radioButtons(ns("mixType"),
          "Type of mixture",
          choices = c(
            "Point" = "Option point",
            "Mean + SD" = "Option Mean SD",
            "Uniform" = "Option uniform"
          )
        )
      )),
      column(
        width = 2,
        conditionalPanel(
          condition = "input.terrestrialCurve == '3' & input.mixType == 'Option point'",
          ns = ns,
          numericInput(ns("mixPoint"), "Mix Point", 0)
        ),
        conditionalPanel(
          condition = "input.terrestrialCurve == '3' & input.mixType == 'Option Mean SD'",
          ns = ns,
          numericInput(ns("mixMean"), "Mix Mean", 0)
        ),
        conditionalPanel(
          condition = "input.terrestrialCurve == '3' & input.mixType == 'Option uniform'",
          ns = ns,
          numericInput(ns("mixMin"), "Mix Min", 0)
        )
      ),
      column(
        width = 2,
        conditionalPanel(
          condition = "input.terrestrialCurve == '3' & input.mixType == 'Option Mean SD'",
          ns = ns,
          numericInput(ns("mixSd"), "Mix SD", 1)
        ),
        conditionalPanel(
          condition = "input.terrestrialCurve == '3' & input.mixType == 'Option uniform'",
          ns = ns,
          numericInput(ns("mixMax"), "Mix Max", 1)
        )
      ),
      column(width = 1, offset = 2, actionButton(ns("help"), "Help"))
    ),
    fluidRow(
      column(width = 3, selectInput(ns("aquaticCurve1"),
        label = "Aquatic Curve 1",
        choices = NULL
      )),
      column(width = 2, selectInput(ns("OxCalA"),
        "Estimate 1",
        choices = c("none")
      )),
      column(width = 2, numericInput(ns("meanDeltaR1"),
        "Mean Delta R 1",
        value = 0
      )),
      column(width = 2, numericInput(ns("sdDeltaR1"),
        "SD Delta R 1",
        value = 1
      )),
      column(
        width = 2,
        radioButtons(ns("bins"), "Type of estimate",
          choices = c(
            "Mean + SD" = "Option Mean SD",
            "PDF" = "Option PDF"
          )
        )
      )
    ),
    fluidRow(
      column(width = 3, selectInput(ns("aquaticCurve2"),
        label = "Aquatic Curve 2",
        choices = NULL
      )),
      column(width = 2, selectInput(ns("OxCalB"),
        "Estimate 2",
        choices = c("none")
      )),
      column(width = 2, numericInput(ns("meanDeltaR2"),
        "Mean Delta R 2",
        value = 0
      )),
      column(width = 2, numericInput(ns("sdDeltaR2"),
        "SD Delta R 2",
        value = 1
      ))
    ),
    actionButton(ns("GenerateOxCal"), "Generate Oxcal code"),
    tags$hr(),
    textAreaInput(ns("OxCalText"), "Oxcal Output",
      width = "100%", height = "400px"
    ) %>%
      shiny::tagAppendAttributes(style = "width: 100%;"),
    actionButton(ns("OxcalExecute"), "Execute in Oxcal"),
    downloadButton(ns("downloadOxCal"), "Download Oxcal code")
  )
}

OxCalOutput <- function(input, output, session, model, exportCoordinates) {
   terrestrialCurvesXlsx <- reactive({
    file <-
      "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/b7732618-7764-460a-b1fa-c614f4cdbe95/download/terrestrial.xlsx"
    read.xlsx(file)
  })

  aquaticCurves1Xlsx <- reactive({
    file <-
      "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/2037632f-f984-4834-8e25-4af5498df163/download/aquatic1.xlsx"
    read.xlsx(file)
  })

  aquaticCurves2Xlsx <- reactive({
    file <-
      "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/120d810e-ff7d-49b7-80b8-e9791e2980b3/download/aquatic2.xlsx"
    read.xlsx(file)
  })

  oxCalBasicCode <- reactive({
    file <- "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/f4b0a2b4-8f65-463d-aff4-2a31490abc78/download/oxcal_basic_code.txt"
    readLines(file, warn = FALSE)
  })

  observe({
    updateSelectInput(session, "terrestrialCurve", choices = getCurveTitlesXlsx(terrestrialCurvesXlsx()))
    updateSelectInput(session, "aquaticCurve1", choices = getCurveTitlesXlsx(aquaticCurves1Xlsx()))
    updateSelectInput(session, "aquaticCurve2", choices = c(
      list("none" = NA),
      getCurveTitlesXlsx(aquaticCurves2Xlsx())
    ))
  })

  observe({
    validate(validInput(model()))
    # parEstimates$Name is always the same for bins = TRUE/FALSE no matter which input$bins we have,
    # but bins == FALSE calculates much faster
    parEstimatesNames <-
      getResultStatistics(model()$modelResults$parameters,
        model()$modelResults$userEstimateSamples,
        model()$fruitsObj,
        agg = TRUE, DT = FALSE, bins = FALSE
      ) %>%
      nameParEstimates() %>%
      pull("Name") %>%
      unique()
    updateSelectInput(session, "OxCalA", choices = c("none", parEstimatesNames))
    updateSelectInput(session, "OxCalB", choices = c("none", parEstimatesNames))
  })

  observeEvent(input$help, {
    helpFile <- "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/aa53dfbf-a521-4aaa-81a6-a01ac89f1667/download/oxcal_help.txt"
    showModal(
      modalDialog(
        title = "OxCal Help",
        readLines(helpFile)
      )
    )
  })

  terrestrialParams <- reactiveVal(NULL)

  observe({
    req(input$terrestrialCurve == "3")

    terrestrialParams(
      switch(input$mixType,
        "Option point" = c(input$Point, 0),
        "Option Mean SD" = c(input$mixMean, input$mixSd),
        "Option uniform" = c(input$mixMin, input$mixMax),
        c(NA, NA)
      )
    )
  })

  observeEvent(input$GenerateOxCal, {
    validate(validInput(model()))

    withProgress(
      {
        if (as.numeric(input$terrestrialCurve) == 3) {
          mixOption <- input$mixType
        } else {
          mixOption <- NULL
        }
        terrestrialCurveCode <- getCodeTerrestrial(
          curve = terrestrialCurvesXlsx()[as.numeric(input$terrestrialCurve), ],
          mixOption = mixOption,
          mixParams = terrestrialParams()
        )

        aquaticCurve1Code <- getCodeAquatic(
          curve = aquaticCurves1Xlsx()[as.numeric(input$aquaticCurve1), ],
          binOption = input$bins,
          deltaRParams = c(input$meanDeltaR1, input$sdDeltaR1)
        )

        aquaticCurve2Code <- getCodeAquatic(
          curve = aquaticCurves2Xlsx()[as.numeric(input$aquaticCurve2), ],
          binOption = input$bins,
          deltaRParams = c(input$meanDeltaR2, input$sdDeltaR2)
        )

        TextOxCal <- createOxCalText(
          model = model(),
          basicCode = oxCalBasicCode(),
          terrestrialCurve = terrestrialCurveCode,
          aquaticCurve1 = aquaticCurve1Code,
          OxCalA = input$OxCalA,
          bins = (input$bins == "Option PDF"),
          aquaticCurve2 = aquaticCurve2Code,
          OxCalB = input$OxCalB,
          coordinates = exportCoordinates
        ) %>%
          paste(collapse = "\n")
      },
      value = 0,
      message = "Generating OxCal output"
    )

    updateTextAreaInput(session, inputId = "OxCalText", value = TextOxCal)
  })


  output$downloadOxCal <- downloadHandler(
    filename = function() {
      paste0("OxCal.txt")
    },
    content = function(file) {
      writeLines(input$OxCalText, file)
    }
  )
}


parseCurveFile <- function(content) {
  content <- paste(content, collapse = "\n")

  sections <- unlist(strsplit(content, "#"))

  lapply(sections[-1], parseSection)
}

parseSection <- function(section) {
  lines <- unlist(strsplit(section, "@@@"))
  lines <- unlist(lapply(lines, trimws))
  lines <- Filter(isNotEmpty, lines)
  lines <-
    unlist(lapply(lines, function(line) {
      strsplit(line, "\n")
    }), recursive = FALSE)
  list(
    title = lines[[2]],
    formula = lines[[3]],
    mixture = lines[-(1:3)]
  )
}

isNotEmpty <- function(x) {
  !is.null(x) && !is.na(x) && trimws(x) != ""
}

getCurveTitles <- function(curves) {
  res <- 1:(length(curves))
  names(res) <- unlist(lapply(curves, `[[`, "title"))
  res
}

getCurveTitlesXlsx <- function(curves) {
  res <- 1:(nrow(curves))
  names(res) <- curves$`Display.name`
  res
}

getCodeTerrestrial <- function(curve, mixOption, mixParams) {
  if (is.null(mixOption)) {
    codeHeader <- curve$`Code.header`
  } else {
    codeHeader <- switch(mixOption,
      "Option point" = curve$`Option.point`,
      "Option Mean SD" = curve$`Option.Mean.SD`,
      "Option uniform" = curve$`Option.uniform`,
      character(0)
    )
  }

  if (!is.null(mixParams)) {
    codeHeader <- codeHeader %>%
      gsub(pattern = "%%Terrestrial_Point%%", replacement = mixParams[[1]]) %>%
      gsub(pattern = "%%Terrestrial_Mean%%", replacement = mixParams[[1]]) %>%
      gsub(pattern = "%%Terrestrial_SD%%", replacement = mixParams[[2]]) %>%
      gsub(pattern = "%%Terrestrial_Min%%", replacement = mixParams[[1]]) %>%
      gsub(pattern = "%%Terrestrial_Max%%", replacement = mixParams[[2]])
  }

  codeHeader
}

getCodeAquatic <- function(curve, binOption, deltaRParams) {
  codeHeader <- curve$`Code.header`
  codeOption <- switch(binOption,
    "Option Mean SD" = curve$`Option.Mean.SD`,
    "Option PDF" = curve$`Option.PDF`,
    character(0)
  )

  if (!is.null(deltaRParams)) {
    codeHeader <- codeHeader %>%
      gsub(pattern = "%%Delta_R_1%%", replacement = deltaRParams[[1]]) %>%
      gsub(pattern = "%%Delta_R_SD_1%%", replacement = deltaRParams[[2]]) %>%
      gsub(pattern = "%%Delta_R_2%%", replacement = deltaRParams[[1]]) %>%
      gsub(pattern = "%%Delta_R_SD_2%%", replacement = deltaRParams[[2]])
  }

  list(header = codeHeader,
       option = codeOption)
}

#' Create Oxcal Text
#'
#' @param model output of the model
#' @param basicCode (character) basic text read from external source
#' @param terrestrialCurve (character) basic text for the terrestrial curve read from external source
#' @param aquaticCurve1 (character) basic text for the aquatic curve 1 read from external source
#' @param aquaticCurve2 (character) basic text for the aquatic curve 2 read from external source
#' @param OxCalA (character) parameter estimate for aquatic curve 1
#' @param meanDeltaR1 (numeric) input of the mean for aquatic curve 1
#' @param sdDeltaR1 (numeric) input of the sd for aquatic curve 1
#' @param OxCalB (character) parameter estimate for aquatic curve 2
#' @param meanDeltaR2 (numeric) input of the mean for aquatic curve 2
#' @param sdDeltaR2 (numeric) input of the sd for aquatic curve 2
#' @param bins (character) either "meansd" for the usage of mean and sd, or "bins" for the usage
#'  of pdf for the selected parameter estimate(s)
#' @param coordinates (data.frame) containing the radiocarbon values (mean+SD) for each target
createOxCalText <- function(model,
                            basicCode,
                            terrestrialCurve, 
                            aquaticCurve1,
                            aquaticCurve2,
                            OxCalA,
                            OxCalB,
                            bins,
                            coordinates) {
  
  if (OxCalA == "none") {
    shinyjs::alert("Please select Estimate 1")
    return("")
  }

  oxcalText <- lapply(basicCode, function(part) {
    if (part == "%%Terrestrial_curve_VAR1%%") {
      return(terrestrialCurve)
    }
    if (part == "%%Aquatic_curve_1_VAR1%%") {
      return(aquaticCurve1$header)
    }
    if (part == "%%Aquatic_curve_2_VAR1%%") {
      return(aquaticCurve2$header)
    }
    if (part == "%%String_from_loop%%") {
      return(getLoop(
        aquaticCurve1$option,
        aquaticCurve2$option,
        model, bins,
        OxCalA, OxCalB,
        coordinates
      ))
    }
    part
  })

  paste(oxcalText, collapse = "\n")
}

#' Get Loop
#'
#' Loop over all targets
#'
#' @inheritParams createOxCalText
getLoop <- function(aquaticCurve1, aquaticCurve2, model, bins, OxCalA, OxCalB, coordinates) {
  if (is.null(aquaticCurve1)) {
    return(NULL)
  }
  
  parEstimates <-
    getResultStatistics(model$modelResults$parameters,
      model$modelResults$userEstimateSamples,
      model$fruitsObj,
      agg = TRUE, DT = FALSE, bins = bins
    ) %>%
    nameParEstimates()
  
  res <- loopOverTargets(
    aquaticCurve1,
    parEstimates %>% filterEstimates(OxCalA),
    bins, coordinates
  )

  if (!is.null(aquaticCurve2)) {
    res <- c(
      res,
      loopOverTargets(
        aquaticCurve2,
        parEstimates %>% filterEstimates(OxCalB),
        bins, coordinates
      )
    )
  }

  res %>%
    paste(collapse = "\n")
}

nameParEstimates <- function(parEstimates) {
  parEstimates$Name <- sapply(1:nrow(parEstimates), function(x) {
    paste(parEstimates$`Group`[x], parEstimates$`Estimate`[x], sep = "_")
  })

  parEstimates
}

filterEstimates <- function(estimates, pattern) {
  estimates[estimates$Name == pattern, ]
}

loopOverTargets <- function(curve, parEstimates, bins, coordinates) {
  if (nrow(parEstimates) == 0) {
    return("")
  }

  res <- lapply(
    1:nrow(parEstimates),
    function(i) {
      getTargetString(
        curve,
        parEstimates[i, ],
        type = bins,
        coordinates = coordinates[rownames(coordinates) == parEstimates[i, ]$Target, ]
      )
    }
  )

  res %>% paste(collapse = "\n")
}

getTargetString <- function(curve, parEstimate, type, coordinates) {
  if (is.null(curve)) {
    return(NULL)
  }
  
  res <- curve %>%
    gsub(pattern = "%%TARGET_ID%%", replacement = paste0("\"", parEstimate$Target, "\"")) %>%
    gsub(pattern = "%%MEAN%%", replacement = parEstimate$mean) %>%
    gsub(pattern = "%%MEAN_B%%", replacement = parEstimate$mean) %>%
    gsub(pattern = "%%SD%%", replacement = parEstimate$sd) %>%
    gsub(pattern = "%%SD_B%%", replacement = parEstimate$sd) %>%
    gsub(pattern = "%%BINS%%", 
         replacement = paste(parEstimate[grep("bin", colnames(parEstimate))], 
                             collapse = ", ")) %>%
    gsub(pattern = "%%BINS_B%%", 
         replacement = paste(parEstimate[grep("bin", colnames(parEstimate))], 
                             collapse = ", ")) %>%
    gsub(pattern = "%%RADIOCARBON_MEAN%%", 
         replacement = cleanNA(coordinates["LowerLimit/Mean/Point"])) %>%
    gsub(pattern = "%%RADIOCARBON_SD%%", 
         replacement = cleanNA(coordinates["UpperLimit/SD"]))
  
  res %>% paste(collapse = " ")
}

cleanNA <- function(x) {
  if (is.na(x)) {
    return("NA")
  } else {
    x
  }
}
