% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-runModel.R
\name{compileRunModel}
\alias{compileRunModel}
\title{Modeling ReSources}
\usage{
compileRunModel(
  fruitsObj,
  progress = FALSE,
  onlySim = FALSE,
  userDefinedAlphas = NULL,
  seqSim = 0.2,
  simSourceNames = NULL,
  onlyShowNimbleInput = FALSE
)
}
\arguments{
\item{fruitsObj}{object of class fruits: input data}

\item{progress}{boolean: show progress in shiny}

\item{onlySim}{boolean: only simulate from prior}

\item{userDefinedAlphas}{list of matrices: for simulation only: food source intakes values}

\item{seqSim}{numeric grid of mixture steps}

\item{simSourceNames}{names of sources to simulate}

\item{onlyShowNimbleInput}{boolean: run the model if FALSE, only show input for nimbleModel() if
TRUE}
}
\description{
Run model on ReSources object
}
