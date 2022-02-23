#' Create or Alter a Model Object
#'
#' @param mode How should the model be run? One of Run_update, Run_independent,
#' Run_baseline
#' @param notes Notes to save with model
#' @param file Read model specifications from file
#'
#' @export
createModel <- function(mode = "Run_update", notes = character(0),
                        file = NULL) {
  list()
}

#' @describeIn createModel
#'
#' @param model Model Object
alterModel <- function(model, mode = NULL, notes = NULL) {
  list()
}
