validInput <- function(model) {
  if (is.list(model)) {
    NULL
  } else if (is.null(model)) {
    "Please define data and run a model"
  }
}

validModelOutput <- function(model) {
  if (is.list(model) && is.list(model$modelResults)) {
    NULL
  } else if (is.null(model)) {
    "Please define data and run a model"
  } else if (is.null(model$modelResults)) {
    "Please uncheck 'Only show nimble input' and run a model"
  }
}
