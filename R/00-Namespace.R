#' @importFrom abind adrop abind
#' @importFrom alphahull ashape
#' @importFrom dplyr mutate_ filter_all any_vars tibble select_if as_tibble bind_rows
#' mutate_all bind_cols select slice pull all_vars "%>%" arrange_ left_join filter_ group_by group_by_ n
#' summarise ungroup
#' @importFrom car ellipse
#' @importFrom coda mcmc raftery.diag gelman.diag geweke.diag heidel.diag
#' @importFrom colourpicker colourInput
#' @importFrom DataTools importDataUI importDataServer
#' @importFrom DT dataTableOutput renderDataTable datatable coerceValue renderDT DTOutput
#' @importFrom futile.logger flog.warn
#' @importFrom ggplot2 ggplot ylab xlab aes_ geom_boxplot geom_density
#' geom_histogram geom_line theme element_text scale_fill_brewer labs ylim scale_fill_manual scale_color_manual
#' geom_point scale_color_brewer xlim aes geom_errorbar geom_smooth
#' @importFrom grDevices dev.off pdf png svg tiff chull pdfFonts postscriptFonts colorRampPalette
#' @importFrom htmltools save_html withTags
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom nimble calculateWAIC nimbleOptions nimbleModel configureMCMC buildMCMC compileNimble runMCMC
#' nimbleFunction registerDistributions rmulti
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom plotly plot_ly layout add_trace renderPlotly plotlyOutput
#' @importFrom RColorBrewer brewer.pal
#' @importFrom readr guess_encoding
#' @importFrom readxl read_excel
#' @importFrom shiny actionButton callModule checkboxInput column conditionalPanel debounce div
#'  downloadButton downloadHandler eventReactive exportTestValues fileInput fluidRow helpText
#'  hideTab HTML includeHTML invalidateLater isolate is.reactive 
#'  mainPanel modalButton modalDialog moduleServer 
#'  navbarMenu NS numericInput observe observeEvent outputOptions plotOutput 
#'  reactive reactiveValues renderText radioButtons reactiveVal reactiveValuesToList removeModal 
#'  renderCachedPlot renderPlot renderPrint renderTable renderUI 
#'  req runApp selectInput selectizeInput setProgress showModal showTab sidebarLayout sidebarPanel 
#'  singleton sliderInput span tableOutput tabPanel tabsetPanel tagAppendAttributes tagList tags 
#'  testServer
#'  textAreaInput textInput textOutput uiOutput updateCheckboxInput updateNumericInput
#'  updateRadioButtons updateSelectInput updateSelectizeInput updateTextAreaInput updateTextInput
#'  validate withProgress
#' @importFrom shinyalert shinyalert
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
