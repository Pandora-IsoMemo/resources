validInput <- function(model) {
  if (is.list(model)) {
    NULL
  } else if (is.null(model)) {
    "Please define data and run a model"
  }
}
