convergenceDiagnostics <- function(parameters, fruitsObj) {
  # parameters <- do.call("cbind", translateParameters(parameters, fruitsObj, addNameTypes = TRUE))
  splitChains <- factor(rep(1:fruitsObj$modelOptions$nchains,
    each = nrow(parameters) / fruitsObj$modelOptions$nchains
  ))

  mcmcObject <- split(as.data.frame(parameters), splitChains)

  mcmcObject <- lapply(mcmcObject, function(x) {
    x <- as.matrix(x)
    x <- mcmc(x, start = 1, end = nrow(x))
    x
  })

  raftery <- try(
    {
      raftery.diag(parameters)
    },
    silent = TRUE
  )
  gelman <- try(
    {
      gelman.diag(mcmcObject, autoburnin = FALSE, multivariate = FALSE)
    },
    silent = TRUE
  )
  geweke <- try(
    {
      geweke.diag(mcmcObject)
    },
    silent = TRUE
  )
  heidel <- try(
    {
      heidel.diag(parameters)
    },
    silent = TRUE
  )

  if (fruitsObj$modelOptions$nchains == 1) {
    gelman <- "For Gelman-Rubin diagnostics, at least 2 chains are required.
    Number of chains option available in the model options tab"
  }

  return(list(raftery = raftery, gelman = gelman, geweke = geweke, heidel = heidel))
}

produceOutText <- function(fruitsObj, diagnostic) {
  outText <- paste0(
    "ReSources ran ", fruitsObj$modelOptions$nchains,
    " MCMC chain(s) with a burn-in of ", fruitsObj$modelOptions$burnin,
    " draws followed by ", fruitsObj$modelOptions$iterations,
    " draws from the posterior with a thinning of ",
    fruitsObj$modelOptions$thinning, "."
  )
  if (fruitsObj$modelOptions$modelType == "1") {
    outText <- paste0(outText, "<br> The targets were run together.")
  } else {
    outText <- paste0(outText, "<br> The targets were run independently.")
  }
  diagFailIndividual <- (((1 - pnorm(abs(diagnostic[grepl("alpha", names(diagnostic))]))) * 2) %>%
    p.adjust("BH") < 0.05)
  if (fruitsObj$modelOptions$modelType != "1") {
    failedIndividuals <- unique(rep(
      1:fruitsObj$constants$nTargets,
      fruitsObj$constants$nSources
    )[which(diagFailIndividual)])
  } else {
    failedIndividuals <- "all"
  }
  diagFailAll <- na.omit(((1 - pnorm(abs(diagnostic))) * 2) %>% p.adjust("BH")) < 0.05
  outText <- paste0(outText, "<br> <br>  <b> Convergence diagnostics </b> (using geweke diagnostic with alpha = 5% and Benjamini-Hochberg correction): <br> ")

  if (any(diagFailIndividual) == FALSE) {
    outText <- paste0(outText, "<br> No convergence failures found for food sources.")
  } else {
    outText <- paste0(
      outText, "<br> Convergence failures found for sources, for target(s) no. ",
      paste0(failedIndividuals, collapse = ", "), "."
    )
  }
  if (any(diagFailAll) == FALSE) {
    outText <- paste0(outText, " <br>  No overall convergence failures found.")
  } else {
    outText <- paste0(outText, " <br> Convergence failures found for some parameters. Please check the model diagnostics tab for further details.")
  }
  if (any(diagFailAll) | any(diagFailIndividual)) {
    outText <- paste0(outText, " <br> <br> To improve convergence of the model, please check the data, try to increase the number of iterations (including burn-in) or increase uncertainty of the target values.")
  }
  return(outText)
}
