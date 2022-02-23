context("Constructor for fruits class")

test_that("object of class fruits is created", {
  valueNames <- list(
    targets = c("Carbon", "Nitrogen"),
    fractions = c("Protein", "Energy"),
    sources = c("Herbivores", "Carnivores", "Plants", "Fish1", "Fish2")
  )
  obsvn <- (default <- matrix(c(-19, 12, -17, 11, -21, 14, -20, 9, -19, 10),
    ncol = length(valueNames$targets),
    dimnames = list(
      paste0("ind", 1:5),
      valueNames$targets
    )
  ))
  obsvnError <- (default <- matrix(rep(0.5, 10),
    ncol = length(valueNames$targets),
    dimnames = list(
      paste0("ind", 1:5),
      valueNames$targets
    )
  ))
  weights <- matrix(c(74, 26, 100, 0),
    nrow = length(valueNames$targets),
    ncol = length(valueNames$fractions),
    dimnames = list(
      valueNames$targets,
      valueNames$fractions
    )
  )
  weightsUncert <- matrix(c(0.5, 0.5, 0.5, 0.5),
    nrow = length(valueNames$targets),
    ncol = length(valueNames$fractions),
    dimnames = list(
      valueNames$targets,
      valueNames$fractions
    )
  )
  weightOffset <- c(4, 7)
  weightOffsetUncert <- c(.5, 1)
  concentration <- list(matrix(c(80, 52, 10, 90, 92, 20, 48, 90, 10, 8),
    nrow = length(valueNames$sources),
    ncol = length(valueNames$fractions),
    dimnames = list(
      valueNames$sources,
      valueNames$fractions
    )
  ))
  concentrationUncert <- list(matrix(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    nrow = length(valueNames$sources),
    ncol = length(valueNames$fractions),
    dimnames = list(
      valueNames$sources,
      valueNames$fractions
    )
  ))
  source <- list(list(
    Carbon = matrix(c(-19, -21, -25, -13, -19, -24, -26, -29, -16, -23),
      nrow = length(valueNames$sources),
      ncol = length(valueNames$fractions),
      dimnames = list(
        valueNames$sources,
        valueNames$fractions
      )
    ),
    Nitrogen = matrix(c(5, 7, 1, 10, 11, 0, 0, 0, 0, 0),
      nrow = length(valueNames$sources),
      ncol = length(valueNames$fractions),
      dimnames = list(
        valueNames$sources,
        valueNames$fractions
      )
    )
  ))
  sourceUncert <- list(list(
    Carbon = matrix(rep(0.5, 10),
      nrow = length(valueNames$sources),
      ncol = length(valueNames$fractions),
      dimnames = list(
        valueNames$sources,
        valueNames$fractions
      )
    ),
    Nitrogen = matrix(rep(0.01, 10),
      nrow = length(valueNames$sources),
      ncol = length(valueNames$fractions),
      dimnames = list(
        valueNames$sources,
        valueNames$fractions
      )
    )
  ))

  covariates <- NULL
  data <- list(
    obsvn = obsvn,
    obsvnError = obsvnError,
    weightOffset = weightOffset,
    weightOffsetUncert = weightOffsetUncert,
    weights = weights,
    weightsUncert = weightsUncert,
    concentration = concentration,
    concentrationUncert = concentrationUncert,
    covariates = covariates,
    source = source,
    sourceUncert = sourceUncert,
    sourceOffset = NULL,
    sourceOffsetUnc = NULL
  )

  modelOptions <- list(
    modelType = "1", modelWeights = TRUE, modelConcentrations = TRUE,
    alphaHyper = 1, modelWeightsContrained = TRUE, modelConcentrationsContrained = TRUE,
    targetOffset = TRUE, burnin = 10000, iterations = 10000, thinning = 10, nchains = 2,
    hierarchical = FALSE, includeSourceOffset = FALSE, weightsDist = "normal",
    sourceDist = list(default = "normal"), concentrationDist = "normal",
    obsvnDist = list(default = "normal"), minUnc = 1E-4, inflatedBeta = "0",
    covariateType = "0", categoricalVars = NULL, numericVars = NULL,
    concentrationDistCovRep = FALSE,
    sourceDistCovRep = FALSE
  )

  x <- fruits(
    data,
    modelOptions,
    valueNames
  )
  expect_named(x, c(
    "data", "priors", "modelOptions", "constants",
    "userEstimates", "valueNames", "modelCode"
  ))
  expect_length(x, 7)
  expect_is(x, "fruits")
})
