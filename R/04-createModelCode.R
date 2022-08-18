#' Create model code
#' @param priors, character vector of priors from shiny interface
#' @param userEstimates, character vector of priors from shiny interface
#' @param valueNames, list of value names
#' @param constants, list of constants
#' @param individualNames, names of the individuals
#' @param modelOptions, list of model options
#' @param covariates, character matrix of categorical covariates
#' @param covariatesNum, numeric atrix of numeric covariates
#' @param termsSources, list of sourceTerms
#' @param termsTargets, list of termsTargets
#'
#' @return Nimble code to be used in \code{\link[nimble]{nimbleModel}}
#' @export
createModelCode <- function(priors, userEstimates, valueNames,
                            constants, individualNames, modelOptions,
                            covariates, termsSources, termsTargets,
                            covariatesNum = NULL) {
  nimbleTemplate <- readSystemFile("nimbleCode/nimbleTemplate")
  if (modelOptions$inflatedBeta == "1") {
    defineRegisterInflBetaDist()
  }

  if (modelOptions$modelType == "1") {
    likelihood <- readSystemFile("nimbleCode/lhNormSingle")
    if (modelOptions$inflatedBeta == "0") {
      priorAlpha <- readSystemFile("nimbleCode/priorAlphaSingle")
    } else {
      priorAlpha <- readSystemFile("nimbleCode/priorAlphaSingleInflatedBeta")
    }
  }

  if (modelOptions$modelType %in% c("2", "3", "4", "5")) {
    likelihood <- readSystemFile("nimbleCode/lhNorm")
    if (modelOptions$modelType %in% c("4", "5")) {
      likelihood <- readSystemFile("nimbleCode/lhNormInd")
    }
    if (modelOptions$modelType %in% c("3")) {
      likelihood <- readSystemFile("nimbleCode/lhNormBase")
    }

    if (modelOptions$inflatedBeta == "0") {
      priorAlpha <- readSystemFile("nimbleCode/priorAlphaAll")
    } else {
      priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllInflatedBeta")
    }
  }
  if (modelOptions$hierarchical || !is.null(covariatesNum)) {
    if (modelOptions$inflatedBeta == "0") {
      if (modelOptions$covariateType != "0") {
        if (is.null(covariatesNum)) {
          if (modelOptions$covariateType != "1") {
            priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchical")
          } else {
            priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalFixed")
          }
        } else {
          if (modelOptions$hierarchical) {
            if (modelOptions$covariateType == "1") {
              priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalNumericFF")
            }
            if (modelOptions$covariateType == "2") {
              priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalNumericRF")
            }
            if (modelOptions$covariateType == "3") {
              priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalNumericRR")
            }
          } else {
            if (modelOptions$covariateType == "1") {
              priorAlpha <- readSystemFile("nimbleCode/priorAlphaNumericFixed")
            }
          }
        }
      }
    } else {
      if (modelOptions$covariateType != "0") {
        if (is.null(covariatesNum)) {
          if (modelOptions$covariateType != "1") {
            priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalInflatedBeta")
          } else {
            priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalInflatedBetaFixed")
          }
        } else {
          if (modelOptions$hierarchical) {
            if (modelOptions$covariateType == "1") {
              priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalNumericBetaFF")
            }
            if (modelOptions$covariateType == "2") {
              priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalNumericBetaRF")
            }
            if (modelOptions$covariateType == "3") {
              priorAlpha <- readSystemFile("nimbleCode/priorAlphaAllHierarchicalNumericBetaRR")
            }
          } else {
            if (modelOptions$covariateType == "1") {
              priorAlpha <- readSystemFile("nimbleCode/priorAlphaNumericFixed")
            }
          }
        }
      }
    }
  }

  likelihood <- manipulateTargetLikelihood(likelihood, modelOptions)
  sourceManipulation <- manipulateSourceLikelihood(likelihood, modelOptions)

  likelihood <- sourceManipulation$likelihood
  priorSources <- sourceManipulation$priorSources
  combineSources <- sourceManipulation$combineSources

  if (modelOptions$includeSourceOffset) {
    if (modelOptions$modelType %in% c("3", "5")) {
      priorSourceOffsets <- readSystemFile("nimbleCode/priorSourceOffsetsBaseline")
    }
    if (modelOptions$modelType %in% c("4", "5")) {
      priorSourceOffsets <- readSystemFile("nimbleCode/priorSourceOffsetsInd")
    }
    if (modelOptions$modelType %in% c("1", "2")) {
      priorSourceOffsets <- readSystemFile("nimbleCode/priorSourceOffsets")
    }
  } else {
    priorSourceOffsets <- ""
  }
  if (modelOptions$targetOffset == TRUE) {
    if (modelOptions$modelType %in% c("4", "5")) {
      priorOffsets <- readSystemFile("nimbleCode/priorOffsetsInd")
    } else {
      priorOffsets <- readSystemFile("nimbleCode/priorOffsets")
    }
  } else {
    priorOffsets <- ""
  }
  if (modelOptions$modelType %in% c("3", "4", "5")) {
    priorConcentrations <- readSystemFile("nimbleCode/priorConcentrationsInd")
    if (modelOptions$concentrationDist == "multivariate-normal") {
      priorConcentrations <- readSystemFile("nimbleCode/priorConcentrationsIndMult")
    }
  } else {
    priorConcentrations <- readSystemFile("nimbleCode/priorConcentrations")
    if (modelOptions$concentrationDist == "multivariate-normal") {
      priorConcentrations <- readSystemFile("nimbleCode/priorConcentrationsMult")
    }
  }
  if (modelOptions$modelConcentrations) {
    if (modelOptions$modelConcentrationsContrained == TRUE) {
      if (modelOptions$concentrationDist == "log-normal") {
        priorConcentrations <- sub("TERM", "C_[i_,j_] ~ T(dlnorm(log(concentration[i_,j_] /
                                                      sqrt(1 + pow(concentrationUncert[i_,j_] /
                                                              concentration[i_,j_], 2))),
                               1 / log((1 + pow(concentrationUncert[i_,j_] /
                               concentration[i_,j_],2)))),
                               -0.0001,100)", priorConcentrations)
      } else {
        if (modelOptions$concentrationDist == "multivariate-normal") {
          if (modelOptions$modelType %in% c("3", "5")) {
            priorConcentrations <- sub("TERM", "C_[h_, i_,1:nFractions] ~ dmnorm(concentration[i_,1:nFractions,h_],
        concentrationCov[1:nFractions,1:nFractions])", priorConcentrations)
          } else {
            if (modelOptions$modelType %in% c("4")) {
              priorConcentrations <- sub("TERM", "C_[h_, i_,1:nFractions] ~ dmnorm(concentration[i_,1:nFractions],
        concentrationCov[1:nFractions,1:nFractions])", priorConcentrations)
            } else {
              priorConcentrations <- sub("TERM", "C_[i_,1:nFractions] ~ dmnorm(concentration[i_,1:nFractions],
        concentrationCov[1:nFractions,1:nFractions])", priorConcentrations)
            }
          }
        } else {
          priorConcentrations <- sub("TERM", "C_[i_,j_] ~ T(dnorm(concentration[i_,j_],
        pow(concentrationUncert[i_,j_],-2)),0,100)", priorConcentrations)
        }
      }
    } else {
      if (modelOptions$concentrationDist == "log-normal") {
        priorConcentrations <- sub("TERM", "C_[i_,j_] ~ dlnorm(log(concentration[i_,j_] /
                                                      sqrt(1 + pow(concentrationUncert[i_,j_] /
                                                              concentration[i_,j_], 2))),
                               1 / log((1 + pow(concentrationUncert[i_,j_] /
                               concentration[i_,j_],2))))", priorConcentrations)
      } else {
        if (modelOptions$concentrationDist == "multivariate-normal") {
          priorConcentrations <- sub("TERM", "C_[i_,1:nFractions] ~ dmnorm(concentration[i_,1:nFractions],
        concentrationCov[1:nFractions,1:nFractions])", priorConcentrations)
        } else {
          priorConcentrations <- sub("TERM", "C_[i_,j_] ~ dnorm(concentration[i_,j_],
        pow(concentrationUncert[i_,j_],-2))", priorConcentrations)
        }
      }
    }
    if (modelOptions$modelType %in% c("3", "4", "5")) {
      priorConcentrations <- gsub("C_[i_,j_]", "C_[h_,i_,j_]", priorConcentrations, fixed = TRUE)
    }
    if (modelOptions$modelType %in% c("3", "5")) {
      priorConcentrations <- gsub("concentration[i_,j_]", "concentration[i_,j_,h_]", priorConcentrations, fixed = TRUE)
      priorConcentrations <- gsub("concentrationUncert[i_,j_]", "concentrationUncert[i_,j_,h_]", priorConcentrations, fixed = TRUE)
      priorConcentrations <- gsub("concentrationCov[1:nFractions,1:nFractions]", "concentrationCov[1:nFractions,1:nFractions,h_]", priorConcentrations, fixed = TRUE)
    }
  } else {
    priorConcentrations <- ""
    if (modelOptions$modelType %in% c("3", "4", "5")) {
      likelihood <- gsub("C_[h_,1:nSources,j_] *", "", likelihood, fixed = TRUE)
      likelihood <- gsub("*C_[h_,1:nSources,j_])", ")", likelihood, fixed = TRUE)
      likelihood <- gsub("%*%C_[h_,1:nSources,1:nFractions])", ")", likelihood, fixed = TRUE)
      likelihood <- gsub("*C_[h_,i_,j_]/", "/", likelihood, fixed = TRUE)
    } else {
      likelihood <- gsub("C_[1:nSources,j_] *", "", likelihood, fixed = TRUE)
      likelihood <- gsub("*C_[1:nSources,j_])", ")", likelihood, fixed = TRUE)
      likelihood <- gsub("%*%C_[1:nSources,1:nFractions])", ")", likelihood, fixed = TRUE)
      likelihood <- gsub("*C_[i_,j_]/", "/", likelihood, fixed = TRUE)
    }
  }
  if (modelOptions$modelType %in% c("4", "5")) {
    priorWeights <- readSystemFile("nimbleCode/priorWeightsInd")
  } else {
    priorWeights <- readSystemFile("nimbleCode/priorWeights")
  }
  # if(modelOptions$modelWeights){
  if (modelOptions$modelWeightsContrained == TRUE) {
    if (modelOptions$weightsDist == "log-normal") {
      priorWeights <- sub("TERM", "W_[k_, j_] ~ T(dlnorm(log(weights[k_,j_] /
                                                      sqrt(1 + pow(weightsUncert[k_,j_] /
                                                              weights[k_,j_], 2))),
                               1 / log((1 + pow(weightsUncert[k_,j_] / weights[k_,j_], 2)))),
                        -0.0001,100)", priorWeights)
    } else {
      priorWeights <- sub("TERM", "W_[k_, j_] ~ T(dnorm(weights[k_,j_],
                               pow(weightsUncert[k_,j_],-2)),-0.0001,100)", priorWeights)
    }
  } else {
    if (modelOptions$weightsDist == "log-normal") {
      priorWeights <- sub(
        "TERM", "W_[k_, j_] ~ dlnorm(log(weights[k_,j_] /
                                                      sqrt(1 + pow(weightsUncert[k_,j_] /
                                                              weights[k_,j_], 2))),
                               1 / log((1 + pow(weightsUncert[k_,j_] / weights[k_,j_], 2))))",
        priorWeights
      )
    } else {
      priorWeights <- sub("TERM", "W_[k_, j_] ~ dnorm(weights[k_,j_],
                               pow(weightsUncert[k_,j_],-2))", priorWeights)
    }
  }
  if (modelOptions$modelType %in% c("4", "5")) {
    priorWeights <- gsub("W_[k_, j_]", "W_[h_,k_,j_]", priorWeights, fixed = TRUE)
  }
  #   }else {
  #   priorWeights <- ""
  #   likelihood <- gsub("W_[k_,j_] * ", "", likelihood, fixed = TRUE)
  #   likelihood <- gsub(" / sum(W_[k_, 1:nFractions])", "", likelihood, fixed = TRUE)
  #   likelihood <- gsub("*W_[k_,1:nFractions])", ")", likelihood, fixed = TRUE)
  #   likelihood <- gsub("%*% W_[k_,1:nFractions])", ")", likelihood, fixed = TRUE)
  # }
  if (length(priors) > 0) {
    parLeft <- sapply(priors, function(x) grepl("\\(", x))
    parRight <- sapply(priors, function(x) grepl("\\)", x))
    if (any(parLeft != parRight)) {
      stop(paste0("Parenthesis not matching. Look at prior: ", priors[[which(parLeft != parRight)]]))
    }
    userDefinedPriors <- paste(translatePriors(
      priors, valueNames, constants,
      individualNames, modelOptions,
      covariates
    ),
    collapse = "\n"
    )
  } else {
    userDefinedPriors <- ""
  }
  if (length(userEstimates) > 0) {
    firstChar <- sapply(userEstimates, function(x) substr(x, 1, 1))
    if (any(!grepl("[^0-9]", firstChar))) {
      stop("Dont use numbers as first character in user estimate or user estimate group name")
    }
    parLeft <- sapply(userEstimates, function(x) grepl("\\(", x))
    parRight <- sapply(userEstimates, function(x) grepl("\\)", x))

    parLeft <- lengths(regmatches(userEstimates, gregexpr("\\(", userEstimates)))
    parRight <- lengths(regmatches(userEstimates, gregexpr("\\)", userEstimates)))

    if (any(parLeft != parRight)) {
      stop(paste0("Parenthesis not matching. Look at user estimate: ", userEstimates[which(parLeft != parRight)]))
    }

    parLeft2 <- lengths(regmatches(userEstimates, gregexpr("\\[", userEstimates)))
    parRight2 <- lengths(regmatches(userEstimates, gregexpr("\\]", userEstimates)))

    if (any(parLeft2 != parRight2)) {
      stop(paste0("Square brackets not matching. Look at user estimate: ", userEstimates[which(parLeft2 != parRight2)]))
    }
    userDefinedEstimates <- paste(translatePriors(userEstimates, valueNames, constants,
      individualNames, modelOptions,
      covariates,
      type = "userEstimates"
    ),
    collapse = "\n"
    )
  } else {
    userDefinedEstimates <- ""
  }

  templateFilled <- tmpl(nimbleTemplate,
    likelihood = likelihood,
    priorAlpha = priorAlpha,
    combineSources = combineSources,
    priorSources = priorSources,
    priorSourceOffsets = priorSourceOffsets,
    priorOffsets = priorOffsets,
    priorConcentrations = priorConcentrations,
    priorWeights = priorWeights,
    userDefinedPriors = userDefinedPriors,
    userDefinedEstimates = userDefinedEstimates
  )
  code <- try(
    {
      tmplEval(templateFilled)
    },
    silent = TRUE
  )
  if (class(code) == "try-error") {
    stop("Error while creating model code. The definition of priors or user estimate definition might be erroneous. Please also check that in naming estimates or model parameters no spaces or special characters are used and that no parenthesis are missing.")
  }
  return(code)
}


#' Read system file form ReSources package
#' @description Wrapper for \code{\link[base]{readLines}} and
#' \code{\link[base]{system.file}}
#' @param path character: path relative to inst directory
readSystemFile <- function(path) {
  paste(readLines(system.file(path, package = "ReSources")), collapse = "\n")
}

manipulateTargetLikelihood <- function(likelihood, modelOptions) {
  if (modelOptions$obsvnDist$default == "multivariate-normal") {
    targetMult <- "obsvn[h_,1:nProxies] ~ dmnorm(mu[h_, 1:nProxies],
                    cov = obsvnCov[1:nProxies,1:nProxies,h_])"
    targetExp <- ""
  }
  if (modelOptions$obsvnDist$default == "normal" | modelOptions$obsvnDist$default == "constant") {
    targetExp <- "obsvn[h_,k_] ~ dnorm(mu[h_,k_], pow(obsvnError[h_,k_],-2))"
    targetMult <- ""
  }
  if (modelOptions$obsvnDist$default == "log-normal") {
    targetExp <- "obsvn[h_,k_] ~ dlnorm(log(mu[h_,k_] / sqrt(1 + pow(obsvnError[h_,k_] /
                                          mu[h_,k_], 2))), 1 /
    log((1 + pow(obsvnError[h_,k_] / mu[h_,k_], 2))))"
    targetMult <- ""
  }

  addTerms <- ""

  if (!is.null(modelOptions$obsvnDist$term1)) {
    if (modelOptions$obsvnDist$term1 == "multivariate-normal") {
      targetMult1 <- "HT1[h_,1:nProxies] ~ dmnorm(obsvnT1[h_, 1:nProxies],
                    cov = obsvnCovT1[1:nProxies,1:nProxies,h_])"
      targetExp1 <- ""
    }
    if (modelOptions$obsvnDist$term1 == "normal" | modelOptions$obsvnDist$term1 == "constant") {
      targetExp1 <- "HT1[h_,k_] ~ dnorm(obsvnT1[h_,k_], pow(obsvnErrorT1[h_,k_],-2))"
      targetMult1 <- ""
    }
    if (modelOptions$obsvnDist$term1 == "log-normal") {
      targetExp1 <- "HT1[h_,k_] ~ dlnorm(log(obsvnT1[h_,k_] /
                                          sqrt(1 + pow(obsvnErrorT1[h_,k_] /
                                          obsvnT1[h_,k_], 2))), 1 /
                                          log((1 + pow(obsvnErrorT1[h_,k_] /
                                          obsvnT1[h_,k_], 2))))"
      targetMult1 <- ""
    }
    addTerms <- paste0(addTerms, "+ HT1[h_,k_]")
  } else {
    targetExp1 <- ""
    targetMult1 <- ""
  }

  if (!is.null(modelOptions$obsvnDist$term2)) {
    if (modelOptions$obsvnDist$term2 == "multivariate-normal") {
      targetMult2 <- "HT2[h_,1:nProxies] ~ dmnorm(obsvnT2[h_, 1:nProxies],
                    cov = obsvnCovT2[1:nProxies,1:nProxies,h_])"
      targetExp2 <- ""
    }
    if (modelOptions$obsvnDist$term2 == "normal" | modelOptions$obsvnDist$term2 == "constant") {
      targetExp2 <- "HT2[h_,k_] ~ dnorm(obsvnT2[h_,k_], pow(obsvnErrorT2[h_,k_],-2))"
      targetMult2 <- ""
    }
    if (modelOptions$obsvnDist$term2 == "log-normal") {
      targetExp2 <- "HT2[h_,k_] ~ dlnorm(log(obsvnT2[h_,k_] /
                                          sqrt(1 + pow(obsvnErrorT2[h_,k_] /
                                          obsvnT2[h_,k_], 2))), 1 /
                                          log((1 + pow(obsvnErrorT2[h_,k_] /
                                          obsvnT2[h_,k_], 2))))"
      targetMult2 <- ""
    }
    addTerms <- paste0(addTerms, "+ HT2[h_,k_]")
  } else {
    targetExp2 <- ""
    targetMult2 <- ""
  }

  if (!is.null(modelOptions$obsvnDist$term3)) {
    if (modelOptions$obsvnDist$term3 == "multivariate-normal") {
      targetMult3 <- "HT3[h_,1:nProxies] ~ dmnorm(obsvnT3[h_, 1:nProxies],
                    cov = obsvnCovT3[1:nProxies,1:nProxies,h_])"
      targetExp3 <- ""
    }
    if (modelOptions$obsvnDist$term3 == "normal" | modelOptions$obsvnDist$term2 == "constant") {
      targetExp3 <- "HT3[h_,k_] ~ dnorm(obsvnT3[h_,k_], pow(obsvnErrorT3[h_,k_],-2))"
      targetMult3 <- ""
    }
    if (modelOptions$obsvnDist$term3 == "log-normal") {
      targetExp3 <- "HT3[h_,k_] ~ dlnorm(log(obsvnT3[h_,k_] /
                                          sqrt(1 + pow(obsvnErrorT3[h_,k_] /
                                          obsvnT3[h_,k_], 2))), 1 /
                                          log((1 + pow(obsvnErrorT3[h_,k_] /
                                          obsvnT3[h_,k_], 2))))"
      targetMult3 <- ""
    }
    addTerms <- paste0(addTerms, "+ HT3[h_,k_]")
  } else {
    targetExp3 <- ""
    targetMult3 <- ""
  }

  likelihood <- sub("ADDTERMS", addTerms, likelihood)

  likelihood <- sub("ReplaceTargets", paste(targetExp1, targetExp2, targetExp3, targetExp,
    sep = "\n"
  ), likelihood)
  likelihood <- sub("ReplaceTargetsMult", paste(targetMult1, targetMult2, targetMult3, targetMult,
    sep = "\n"
  ), likelihood)
  return(likelihood)
}

selectSourcePriorFile <- function(dist, modelType) {
  if (modelType %in% c("3", "5")) {
    if (dist == "multivariate-normal") {
      priorSources <- readSystemFile("nimbleCode/priorSourcesMultiBaseline")
    }
    if (dist == "log-normal") {
      priorSources <- readSystemFile("nimbleCode/priorSourcesLogNormBaseline")
    }
    if (dist %in% c("constant", "normal")) {
      priorSources <- readSystemFile("nimbleCode/priorSourcesBaseline")
    }
  }
  if (modelType == "4") {
    if (dist == "multivariate-normal") {
      priorSources <- readSystemFile("nimbleCode/priorSourcesMultiInd")
    }
    if (dist == "log-normal") {
      priorSources <- readSystemFile("nimbleCode/priorSourcesLogNormInd")
    }
    if (dist %in% c("constant", "normal")) {
      priorSources <- readSystemFile("nimbleCode/priorSourcesInd")
    }
  }
  if (modelType %in% c("1", "2")) {
    if (dist == "multivariate-normal") {
      priorSources <- readSystemFile("nimbleCode/priorSourcesMulti")
    }
    if (dist == "log-normal") {
      priorSources <- readSystemFile("nimbleCode/priorSourcesLogNorm")
    }
    if (dist %in% c("constant", "normal")) {
      priorSources <- readSystemFile("nimbleCode/priorSources")
    }
  }
  return(priorSources)
}

manipulateSourceLikelihood <- function(likelihood, modelOptions) {
  priorSources <- selectSourcePriorFile(modelOptions$sourceDist$default, modelOptions$modelType)
  if (modelOptions$modelType %in% c("3", "4", "5")) {
    Par_Sources <- "I0_[i_,j_,k_,h_]"
  }
  else {
    Par_Sources <- "I0_[i_,j_,k_]"
  }

  if (!is.null(modelOptions$sourceDist$term1)) {
    if (modelOptions$modelType %in% c("3", "4", "5")) {
      Par_Sources <- paste0(Par_Sources, " + S1_[i_,j_,k_,h_]")
    }
    else {
      Par_Sources <- paste0(Par_Sources, " + S1_[i_,j_,k_]")
    }
    priorSources1 <- selectSourcePriorFile(modelOptions$sourceDist$term1, modelOptions$modelType)
    priorSources1 <- sub("I0_", "S1_", priorSources1)
    priorSources1 <- gsub("source\\[", "sourceT1\\[", priorSources1)
    priorSources1 <-
      gsub("sourceUncert\\[", "sourceUncertT1\\[", priorSources1)
    priorSources1 <-
      gsub("sourceCov\\[", "sourceCovT1\\[", priorSources1)
  } else {
    priorSources1 <- ""
  }

  if (!is.null(modelOptions$sourceDist$term2)) {
    if (modelOptions$modelType %in% c("3", "4", "5")) {
      Par_Sources <- paste0(Par_Sources, " + S2_[i_,j_,k_,h_]")
    }
    else {
      Par_Sources <- paste0(Par_Sources, " + S2_[i_,j_,k_]")
    }
    priorSources2 <- selectSourcePriorFile(modelOptions$sourceDist$term2, modelOptions$modelType)
    priorSources2 <- sub("I0_", "S2_", priorSources2)
    priorSources2 <- gsub("source\\[", "sourceT2\\[", priorSources2)
    priorSources2 <-
      gsub("sourceUncert\\[", "sourceUncertT2\\[", priorSources2)
    priorSources2 <-
      gsub("sourceCov\\[", "sourceCovT2\\[", priorSources2)
  } else {
    priorSources2 <- ""
  }

  if (!is.null(modelOptions$sourceDist$term3)) {
    if (modelOptions$modelType %in% c("3", "4", "5")) {
      Par_Sources <- paste0(Par_Sources, " + S3_[i_,j_,k_,h_]")
    }
    else {
      Par_Sources <- paste0(Par_Sources, " + S3_[i_,j_,k_]")
    }
    priorSources3 <- selectSourcePriorFile(modelOptions$sourceDist$term3, modelOptions$modelType)
    priorSources3 <- sub("I0_", "S3_", priorSources3)
    priorSources3 <- gsub("source\\[", "sourceT3\\[", priorSources3)
    priorSources3 <-
      gsub("sourceUncert\\[", "sourceUncertT3\\[", priorSources3)
    priorSources3 <-
      gsub("sourceCov\\[", "sourceCovT3\\[", priorSources3)
  } else {
    priorSources3 <- ""
  }

  if (modelOptions$modelType %in% c("3", "4", "5")) {
    Par_Sources_Comb <- "I_[1:nSources,j_,k_,h_]"
  }
  else {
    Par_Sources_Comb <- "I_[1:nSources,j_,k_]"
  }

  if (modelOptions$includeSourceOffset) {
    if (modelOptions$modelType %in% c("3", "4", "5")) {
      Par_Sources_Comb <- paste0(Par_Sources_Comb, " + S_[1:nSources,j_,k_,h_]")
    }
    else {
      Par_Sources_Comb <- paste0(Par_Sources_Comb, " + S_[1:nSources,j_,k_]")
    }
  }

  if (modelOptions$targetOffset) {
    if (modelOptions$modelType %in% c("4", "5")) {
      Par_Sources_Comb <- paste0(Par_Sources_Comb, " + T_[ h_, k_]")
    } else {
      Par_Sources_Comb <- paste0(Par_Sources_Comb, " + T_[k_]")
    }
  }

  likelihood <- sub("Par_Sources_Comb", Par_Sources_Comb, likelihood)

  if (modelOptions$modelType %in% c("3", "4", "5")) {
    combineSources <- readSystemFile("nimbleCode/combineSourcesBaseline")
  } else {
    combineSources <- readSystemFile("nimbleCode/combineSources")
  }

  combineSources <- sub("Par_Sources", Par_Sources, combineSources)

  priorSources <- paste0(priorSources, priorSources1, priorSources2, priorSources3)

  return(list(
    likelihood = likelihood,
    priorSources = priorSources,
    combineSources = combineSources
  ))
}
