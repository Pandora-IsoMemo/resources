#' @importFrom abind adrop abind
#' @importFrom alphahull ashape
#' @importFrom dplyr mutate_ filter_all any_vars tibble select_if as_tibble bind_rows
#' mutate_all bind_cols select slice pull all_vars "%>%" arrange_ left_join filter_ group_by group_by_ n
#' summarise ungroup
#' @importFrom car ellipse
#' @importFrom coda mcmc raftery.diag gelman.diag geweke.diag heidel.diag
#' @importFrom colourpicker colourInput
#' @importFrom DT dataTableOutput renderDataTable datatable coerceValue renderDT DTOutput
#' @importFrom futile.logger flog.warn
#' @importFrom ggplot2 ggplot ylab xlab aes_ geom_boxplot geom_density
#' geom_histogram geom_line theme element_text scale_fill_brewer labs ylim scale_fill_manual scale_color_manual
#' geom_point scale_color_brewer xlim aes geom_errorbar
#' @importFrom grDevices dev.off pdf png svg tiff chull pdfFonts postscriptFonts colorRampPalette
#' @importFrom htmltools withTags
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom nimble calculateWAIC nimbleOptions nimbleModel configureMCMC buildMCMC compileNimble runMCMC
#' nimbleFunction registerDistributions rmulti
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom plotly plot_ly layout add_trace renderPlotly plotlyOutput
#' @importFrom RColorBrewer brewer.pal
#' @importFrom readr guess_encoding
#' @importFrom readxl read_excel
#' @importFrom shiny runApp NS tabPanel sidebarLayout sidebarPanel
#' tabPanel tags fileInput actionButton mainPanel tabsetPanel span
#' reactiveValues callModule observe reactive renderText navbarMenu observeEvent
#' div textOutput showModal removeModal modalDialog tagList selectInput renderUI
#' conditionalPanel helpText textInput includeHTML textAreaInput uiOutput
#' tagAppendAttributes updateSelectInput fluidRow column isolate numericInput
#' eventReactive updateTextInput updateNumericInput exportTestValues renderTable
#' tableOutput checkboxInput is.reactive singleton reactiveValuesToList withProgress
#' validate setProgress renderPlot plotOutput radioButtons modalButton outputOptions
#' updateRadioButtons updateCheckboxInput showTab hideTab downloadButton downloadHandler
#' req sliderInput reactiveVal renderPrint HTML debounce updateTextAreaInput
#' updateSelectizeInput invalidateLater renderCachedPlot
#' @importFrom shinyjs reset useShinyjs alert show hide
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats setNames runif acf na.omit qnorm cov pnorm rbeta dbeta pchisq dnorm
#' var sd cov2cor p.adjust var.test t.test shapiro.test median quantile manova rnorm pbeta qbeta qchisq dist
#' @importFrom templates tmpl tmplEval
#' @importFrom tibble add_row add_column
#' @importFrom tidyr gather
#' @importFrom utils read.csv data packageVersion head combn compareVersion tail
#' installed.packages write.table capture.output read.table
#' @importFrom zip zipr
NULL
