userEstimatesCalculator <- function(funs = FALSE) {
  ns <- NS("calcUserEstimate")

  symbolMatrix <- matrix(
    c(
      7, 8, 9, "/", "(", NA,
      4, 5, 6, "*", ")", NA,
      1, 2, 3, "-", NA, NA,
      NA, 0, ".", "+", "E", "D"
    ),
    nrow = 4, byrow = TRUE
  )

  funMatrix <- matrix(
    c(
      "step()", "log()", "pow(,)", ",",
      "abs()", "exp()", "sqrt()", NA,
      "cos()", "sin()", "trunc()", NA
    ),
    nrow = 3, byrow = TRUE
  )

  symbols <- if (funs) {
    funMatrix
  } else {
    symbolMatrix
  }

  rowSplit <- split(symbols, rep(1:nrow(symbols), ncol(symbols)))

  tags$table(
    id = "userEstimates-calculator",
    lapply(rowSplit, function(r) {
      tags$tr(
        lapply(r, function(s) {
          if (is.na(s)) {
            tags$td()
          } else {
            tags$td(tags$button(
              id = ns(s),
              class = "calcUserEstimate-button btn btn-default", s
            ))
          }
        })
      )
    })
  )
}

validateUserEstimate <- function(x, estimates = character(0)) {
  stopifnot(length(x) == 1)
  stopifnot(is.character(x))
  stopifnot(is.null(estimates) || is.character(estimates))
  res <- TRUE
  res <- step(res, x, userEstimateNotEmpty)
  res <- step(res, x, userEstimateEqualSign)
  res <- step(res, x, userEstimateOpMissing)
  res <- step(res, x, userEstimateOpAbundance)
  res <- step(res, x, userEstimateNameValid)
  res <- step2(res, x, estimates, userEstimateNameNotTaken)
  res
}

step2 <- function(res, x, y, fun) {
  if (!res) {
    return(FALSE)
  }

  return(fun(x, y))
}

userEstimateNotEmpty <- function(x) {
  trimws(x) != "" & trimws(gsub(".*=", "", x)) != "" & trimws(gsub("=.*", "", x)) != ""
}

userEstimateEqualSign <- function(x) {
  x <- sub(".*=(.*)", "\\1", x)
  !grepl("=", x)
}

userEstimateOpMissing <- function(x) {
  !grepl("(\\]\\[)|(\\][[:digit:]])|([[:digit:]]\\[)", x)
}

userEstimateOpAbundance <- function(x) {
  !grepl("[\\+\\*/-]{2,}", x)
}

userEstimateNameValid <- function(x) {
  name <- sub("(.*)=.*", "\\1", x)
  !grepl("[!?\\=#%<>/;~,:+*'}{]", name)
  # !grepl("[[:punct:]]", name)
}

userEstimateNameNotTaken <- function(x, estimates) {
  name <- sub("(.*)=.*", "\\1", x)
  names <- sub("(.*)=.*", "\\1", estimates)
  !(name %in% names)
}
