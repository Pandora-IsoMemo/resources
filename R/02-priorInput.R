priorCalculator <- function(funs = FALSE){
  ns <- NS("calc")

  symbolMatrix <- matrix(
    c(7, 8, 9, "/", "(", ">",
      4, 5, 6, "*", ")", "<",
      1, 2, 3, "-", NA, "=",
      NA, 0, ".", "+", "E", "D"),
    nrow = 4, byrow = TRUE)

  funMatrix <- matrix(
    c("step()", "log()", "pow(,)", ",",
    "abs()", "exp()", "sqrt()", NA,
    "cos()", "sin()", "trunc()", NA),
    nrow = 3, byrow = TRUE)

  symbols <- if (funs) funMatrix
  else symbolMatrix

  rowSplit <- split(symbols, rep(1:nrow(symbols), ncol(symbols)))

  tags$table(
    id = if (funs) "prior-calculator-funs" else "prior-calculator",
    lapply(rowSplit, function(r){
      tags$tr(
        lapply(r, function(s){
          if (is.na(s)) tags$td()
        else tags$td(tags$button(id = ns(s), class = "calc-button btn btn-default", s))
        })
      )
    })
  )
}
priorInput <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "ReSources/priorInput.js"))),
    div(id = inputId, class = "prior")
  )
}

updatePriorInput <- function(session, inputId, value = NULL) {
  message <- list(value = value)
  session$sendInputMessage(inputId, message)
}

validatePrior <- function(x){
  stopifnot(length(x) == 1)
  stopifnot(is.character(x))

  res <- TRUE
  res <- step(res, x, priorNotEmpty)
  res <- step(res, x, priorPartMissing)
  res <- step(res, x, priorOpMissing)
  res <- step(res, x, priorOpAbundance)
  res <- step(res, x, priorOpAbundance2)
  res
}

step <- function(res, x, fun){
  if (!res) return(FALSE)

  return(fun(x))
}

priorNotEmpty <- function(x){
  trimws(x) != ""
}

priorPartMissing <- function(x){
  grepl("(.+[><=][^=]+)|(.+>=.+)|(.+<=.+)", x)
}

priorOpMissing <- function(x){
  !grepl("(\\]\\[)|(\\][[:digit:]])|([[:digit:]]\\[)", x)
}

priorOpAbundance <- function(x){
  r <- gregexpr("([><=][^=])|(>=)|(<=)", x)
  m <- regmatches(x, r)[[1]]
  length(m) == 1
}

priorOpAbundance2 <- function(x){
  !grepl("[\\+\\*/-]{2,}", x)
}
