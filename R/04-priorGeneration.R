getNimblePrior <- function(priorParameters, prior, replacements, n, individualNames,
                           Unc = NULL, applyUnc = FALSE) {
  nimblePrior <- prior
  nimblePrior <- gsub("\\[", " ", nimblePrior)
  nimblePrior <- gsub("\\]", " ", nimblePrior)

  for (i in 1:length(priorParameters)) {
    nimblePrior <- gsub(paste0(" ", priorParameters[i], " "), replacements[i], nimblePrior)
  }
  if (grepl("=", nimblePrior) == FALSE) {
    if (applyUnc == FALSE) {
      return(paste0(
        "prior", n[2], "_", individualNames[n[1]],
        " ~ dconstraint(", nimblePrior, "& 10000 > 9999)"
      ))
    } else {
      return(paste0(
        "prior", n[2], "_", individualNames[n[1]],
        " ~ dconstraint(", nimblePrior, "- 1/2* ", Unc, "& 10000 > 9999)"
      ))
    }
  }
  if (grepl(">=", nimblePrior) == TRUE || grepl("<=", nimblePrior) == TRUE) {
    nimblePrior <- gsub("=", "", nimblePrior)
    if (applyUnc == FALSE) {
      return(paste0(
        "prior", n[2], "_", individualNames[n[1]],
        " ~ dconstraint(", nimblePrior, "& 10000 > 9999)"
      ))
    } else {
      return(paste0(
        "prior", n[2], "_", individualNames[n[1]],
        " ~ dconstraint(", nimblePrior, "- 1/2* ", Unc, "& 10000 > 9999)"
      ))
    }
  }
  if (grepl("=", nimblePrior) == TRUE) {
    if (applyUnc == FALSE) {
      nimblePrior <- c(paste0(gsub("=", "<", nimblePrior)), paste0(gsub("=", ">", nimblePrior)))

      nimblePrior <- paste0(nimblePrior, c(paste0("+", Unc), paste0("+", Unc)), n[2])

      return(c(
        paste0(
          "prior", n[2], "_", paste0(individualNames[n[1]], c("a", "b")),
          " ~ dconstraint(", nimblePrior, c("*1/2", "*-1/2"), "& 10000 > 9999)"
        )
      ))
    } else {
      nimblePrior <- c(paste0(gsub("=", "<", nimblePrior)), paste0(gsub("=", ">", nimblePrior)))
      return(paste0("prior", n[2], "_", paste0(
        individualNames[n[1]],
        c("a", "b")
      ), " ~ dconstraint(", nimblePrior, c("*1/2", "*-1/2"), "& 10000 > 9999)"))
    }
  }
}

getUserEstimatesExpressions <- function(userEstimatesParameters,
                                        userEstimates, replacements,
                                        n, individualNames,
                                        Dist) {
  userEstimates <- gsub("\\[", " ", userEstimates)
  userEstimates <- gsub("\\]", " ", userEstimates)
  for (i in 1:length(userEstimatesParameters)) {
    userEstimates <- gsub(
      paste0(" ", userEstimatesParameters[i], " "),
      replacements[i], userEstimates
    )
    userEstimates <- gsub("=", paste0("_", individualNames[n[1]], " <- "), userEstimates)
  }
  return(userEstimates)
}

getNamesInsideBrackets <- function(priorString) {
  priorString <- gsub("(^[^\\[]*\\[)|\\]((?<=\\])[^\\]]*$)",
    "", priorString,
    perl = TRUE
  ) # rm start & end
  priorString <- gsub("\\].*?\\[", ";", priorString) # rm between "]" and "[" and replace by ";"
  priorString <- strsplit(priorString, ";") # split at ";"
  priorString <- unlist(priorString)
  priorString
}

getValuesInsideCurlyBrackets <- function(priorString) {
  priorString <- strsplit(priorString[[1]], "\\{")[[1]][2] # split at "{"
  priorString <- strsplit(priorString, "\\}")[[1]][1]
  priorString
}

getValuesInsideTildeBrackets <- function(priorString) {
  priorString <- strsplit(priorString[[1]], "\\~")[[1]]
  if (length(priorString) > 1) {
    priorString <- priorString[seq(2, length(priorString), by = 2)] # split at "{"
    return(priorString)
  } else {
    return(NA)
  }
  # priorString <- strsplit(priorString, "\\~")[[1]][1]
}


replacePriorUncertainties <- function(priorString, priorUncName) {
  paste0(strsplit(priorString[[1]], "\\{")[[1]][1], priorUncName)
}

replacePriorDists <- function(priorString, priorUncName, priorDistributions) {
  priorString <- gsub("\\~", "", priorString)
  for (i in 1:length(priorDistributions)) {
    priorString <- gsub(priorDistributions[i], priorUncName[i], priorString, fixed = TRUE)
  }
  priorString
}


matchPriorTypes <- function(priorParameters, valueNames) {
  (lapply(priorParameters, function(y) {
    names(which(unlist(lapply(valueNames, function(x) y %in% x))))
  }))
}

translatePriors <- function(priors, valueNames, constants, individualNames, modelOptions,
                            covariates = NULL, type = "priors") {
  unique(unlist(lapply(1:length(priors), function(x) {
    priorParameters <- getNamesInsideBrackets(priors[x])
    priorUncertainties <- getValuesInsideCurlyBrackets(priors[x])
    priorDistributions <- getValuesInsideTildeBrackets(priors[x])

    if (length(na.omit(priorUncertainties)) == 1) {
      priorDistUnc <- paste0("pUnc_", x, " ~ dunif(0,", priorUncertainties, ")")
      priors[x] <- replacePriorUncertainties(priors[x], paste0("pUnc_", x))
    }

    if (length(na.omit(priorDistributions)) > 0) {
      priorDist <- unlist(lapply(1:length(priorDistributions), function(xx) {
        dP <- unlist(regmatches(
          priorDistributions[xx],
          gregexpr("\\)?[0-9.]+", priorDistributions[xx])
        ))


        if (length(dP) > 1) {
          distMean <- dP[1]
          distUnc <- dP[2]
        } else {
          distMean <- dP[1]
          distUnc <- 0
        }
        priorDist <- paste0("pDistUserEstimate_", x, "_", xx, " ~ dnorm(", distMean, ", sd = ", distUnc, ")")
        priorDist
      }))
      priors[x] <- replacePriorDists(priors[x], paste0("pDistUserEstimate_", x, "_", 1:length(priorDistributions)), priorDistributions)
    }
    valueNames$valProxies <- apply(expand.grid(
      valueNames$sources,
      valueNames$fractions,
      valueNames$targets
    ),
    1, paste,
    collapse = "-"
    )
    valueNames$contrProxies <- apply(expand.grid(
      valueNames$targets,
      valueNames$sources
    ),
    1, paste,
    collapse = "-"
    )
    valueNames$weightValues <- apply(expand.grid(
      valueNames$targets,
      valueNames$fractions
    ),
    1, paste,
    collapse = "-"
    )
    valueNames$concValues <- apply(expand.grid(
      valueNames$sources,
      valueNames$fractions
    ),
    1, paste,
    collapse = "-"
    )
    valueNames$consValues <- paste0("Consumer", "-", valueNames$targets)
    if (modelOptions$hierarchical) {
      valueNames$hierValues <- getAllMainInteractions(covariates,
        valueNames$sources,
        vars = colnames(covariates)
      )
      valueNames$hierValuesBeta <- getAllMainInteractions(covariates,
        valueNames$fractions,
        vars = colnames(covariates)
      )
      valueNames$hierValuesTheta <- getAllMainInteractions(covariates,
        valueNames$targets,
        valueNames$sources,
        vars = colnames(covariates)
      )
      if(modelOptions$modelType %in% c("4", "5")){
        valueNames$hierValuesConc <- getAllMainInteractions(covariates,
                                                             valueNames$sources,
                                                             valueNames$fractions,
                                                             vars = colnames(covariates)
        )
        valueNames$hierValuesCons <- getAllMainInteractions(covariates,
                                                            valueNames$targets,
                                                            vars = colnames(covariates)
        )
        
        valueNames$hierValuesWeight <- getAllMainInteractions(covariates,
                                                            valueNames$targets,
                                                            valueNames$fractions,
                                                            vars = colnames(covariates)
        )
        # valueNames$hierValuescontrProxies <- getAllMainInteractions(covariates,
        #                                                       valueNames$targets,
        #                                                       valueNames$sources,
        #                                                       vars = colnames(covariates)
        # )
        valueNames$hierValuesValProxies <- getAllMainInteractions(covariates,
                                                                  valueNames$sources,
                                                                  valueNames$fractions,
                                                                  valueNames$targets,
                                                                  vars = colnames(covariates)
        )
        
      }
    }


    priorTypes <- matchPriorTypes(priorParameters, valueNames)

    sourceParameters <- priorParameters[which(priorTypes == "sources")]
    fractionParameters <- priorParameters[which(priorTypes == "fractions")]
    targetParameters <- priorParameters[which(priorTypes == "targets")]
    contrProxiesParameters <- priorParameters[which(priorTypes == "contrProxies")]
    valProxiesParameters <- priorParameters[which(priorTypes == "valProxies")]
    weightValuesParameters <- priorParameters[which(priorTypes == "weightValues")]
    concValuesParameters <- priorParameters[which(priorTypes == "concValues")]
    consValuesParameters <- priorParameters[which(priorTypes == "consValues")]
    if (modelOptions$hierarchical) {
      hierValuesParameters <- priorParameters[which(priorTypes == "hierValues")]
      hierValuesParametersBeta <- priorParameters[which(priorTypes == "hierValuesBeta")]
      hierValuesParametersTheta <- priorParameters[which(priorTypes == "hierValuesTheta")]
      if(modelOptions$modelType %in% c("4", "5")){
        hierValuesParametersConc <- priorParameters[which(priorTypes == "hierValuesConc")]
        hierValuesParametersCons <- priorParameters[which(priorTypes == "hierValuesCons")]
        hierValuesParametersWeight <- priorParameters[which(priorTypes == "hierValuesWeight")]
        #hierValuesParametersContrProxies <- priorParameters[which(priorTypes == "hierValuescontrProxies")]
        hierValuesParametersValProxies <- priorParameters[which(priorTypes == "hierValuesValProxies")]
      }
    }

    if (modelOptions$modelType == "1") {
      nTargetsAdjusted <- 1
      individualNames <- "All"
    } else {
      nTargetsAdjusted <- constants$nTargets
    }
    priorList <- lapply(1:nTargetsAdjusted, function(j) {
      replacements <- list(
        valProxies = tryCatch(
          {
            sapply(
              1:length(valProxiesParameters),
              function(x) {
                if (modelOptions$modelType %in% c("2", "3")) {
                  paste0(
                    "I_[", match(
                      strsplit(
                        valProxiesParameters,
                        "-"
                      )[[x]][1],
                      valueNames$sources
                    ), ",",
                    match(
                      strsplit(
                        valProxiesParameters,
                        "-"
                      )[[x]][2],
                      valueNames$fractions
                    ), ",",
                    match(
                      strsplit(
                        valProxiesParameters,
                        "-"
                      )[[x]][3],
                      valueNames$targets
                    ), "]"
                  )
                } else {
                  paste0(
                    "I_[", j, ",",
                    match(
                      strsplit(
                        valProxiesParameters,
                        "-"
                      )[[x]][1],
                      valueNames$sources
                    ), ",",
                    match(
                      strsplit(
                        valProxiesParameters,
                        "-"
                      )[[x]][2],
                      valueNames$fractions
                    ), ",",
                    match(
                      strsplit(
                        valProxiesParameters,
                        "-"
                      )[[x]][3],
                      valueNames$targets
                    ), "]"
                  )
                }
              }
            )
          },
          error = function(cond) {
            return(c())
          }
        ),
        contrProxies = tryCatch(
          {
            if (modelOptions$modelType != "1") {
              sapply(
                1:length(contrProxiesParameters),
                function(x) {
                  paste0(
                    "theta_[", j, ",",
                    match(
                      strsplit(
                        contrProxiesParameters,
                        "-"
                      )[[x]][1],
                      valueNames$targets
                    ), ",",
                    match(
                      strsplit(
                        contrProxiesParameters,
                        "-"
                      )[[x]][2],
                      valueNames$sources
                    ), "]"
                  )
                }
              )
            } else {
              sapply(
                1:length(contrProxiesParameters),
                function(x) {
                  paste0(
                    "theta_[",
                    match(
                      strsplit(
                        contrProxiesParameters,
                        "-"
                      )[[x]][1],
                      valueNames$targets
                    ), ",",
                    match(
                      strsplit(
                        contrProxiesParameters,
                        "-"
                      )[[x]][2],
                      valueNames$sources
                    ), "]"
                  )
                }
              )
            }
          },
          error = function(cond) {
            return(c())
          }
        ),
        weightValues = tryCatch(
          {
            sapply(
              1:length(weightValuesParameters),
              function(x) {
                if (!(modelOptions$modelType %in% c("4", "5"))) {
                  paste0(
                    "W_[", match(
                      strsplit(
                        weightValuesParameters,
                        "-"
                      )[[x]][1],
                      valueNames$targets
                    ), ",",
                    match(
                      strsplit(
                        weightValuesParameters,
                        "-"
                      )[[x]][2],
                      valueNames$fractions
                    ), "]"
                  )
                } else {
                  paste0(
                    "W_[", j, ",", match(
                      strsplit(
                        weightValuesParameters,
                        "-"
                      )[[x]][1],
                      valueNames$targets
                    ), ",",
                    match(
                      strsplit(
                        weightValuesParameters,
                        "-"
                      )[[x]][2],
                      valueNames$fractions
                    ), "]"
                  )
                }
              }
            )
          },
          error = function(cond) {
            return(c())
          }
        ),
        concValues = tryCatch(
          {
            sapply(
              1:length(concValuesParameters),
              function(x) {
                if (!(modelOptions$modelType %in% c("3", "4", "5"))) {
                  paste0(
                    "C_[", match(
                      strsplit(
                        concValuesParameters,
                        "-"
                      )[[x]][1],
                      valueNames$sources
                    ), ",",
                    match(
                      strsplit(
                        concValuesParameters,
                        "-"
                      )[[x]][2],
                      valueNames$fractions
                    ), "]"
                  )
                } else {
                  paste0(
                    "C_[", j, ",", match(
                      strsplit(
                        concValuesParameters,
                        "-"
                      )[[x]][1],
                      valueNames$sources
                    ), ",",
                    match(
                      strsplit(
                        concValuesParameters,
                        "-"
                      )[[x]][2],
                      valueNames$fractions
                    ), "]"
                  )
                }
              }
            )
          },
          error = function(cond) {
            return(c())
          }
        ),
        consValues = tryCatch(
          {
            sapply(
              1:length(consValuesParameters),
              function(x) {
                paste0(
                  "mu[", j, ",",
                  match(
                    strsplit(
                      consValuesParameters,
                      "-"
                    )[[x]][2],
                    valueNames$targets
                  ), "]"
                )
              }
            )
          },
          error = function(cond) {
            return(c())
          }
        ),
        sources = tryCatch(
          {
            if (modelOptions$modelType != "1") {
              paste0(
                "alpha_[", j, ",",
                match(
                  sourceParameters,
                  valueNames$sources
                ), "]"
              )
            }
            else {
              paste0(
                "alpha_[",
                match(
                  sourceParameters,
                  valueNames$sources
                ), "]"
              )
            }
          },
          error = function(cond) {
            return(c())
          }
        ),
        fractions = tryCatch(
          {
            if (modelOptions$modelType != "1") {
              paste0(
                "beta_[", j, ",",
                match(
                  fractionParameters,
                  valueNames$fractions
                ), "]"
              )
            }
            else {
              paste0(
                "beta_[",
                match(
                  fractionParameters,
                  valueNames$fractions
                ), "]"
              )
            }
          },
          error = function(cond) {
            return(c())
          }
        ),
        targets = tryCatch(
          {
            if (!(modelOptions$modelType %in% c("4", "5"))) {
              paste0("T_[", match(targetParameters, valueNames$targets), "]")
            } else {
              paste0("T_[", j, ",", match(targetParameters, valueNames$targets), "]")
            }
          },
          error = function(cond) {
            return(c())
          }
        ),
        hierValues = tryCatch(
          {
            getHierPriors(
              hierValuesParameters =
                hierValuesParameters,
              type = "alpha_[",
              covariates = covariates,
              names1 = valueNames$sources
            )
          },
          error = function(cond) {
            return(c())
          }
        ),
        hierValuesBeta = tryCatch(
          {
            getHierPriors(
              hierValuesParameters =
                hierValuesParametersBeta,
              type = "beta_[",
              covariates = covariates,
              names1 = valueNames$fractions
            )
          },
          error = function(cond) {
            return(c())
          }
        ),
        hierValuesTheta = tryCatch(
          {
            getHierPriors(
              hierValuesParameters =
                hierValuesParametersTheta,
              type = "theta_[",
              covariates = covariates,
              names1 = valueNames$sources,
              names2 = valueNames$targets
            )
          },
          error = function(cond) {
            return(c())
          }
        ),
        hierValuesConc = tryCatch(
          {
          if (modelOptions$modelType %in% c("4","5")) {
            if(length((hierValuesParametersConc))>0){
            }
            getHierPriors(
              hierValuesParameters =
                hierValuesParametersConc,
              type = "C_[",
              covariates = covariates,
              names1 = valueNames$fractions,
              names2 = valueNames$sources
            )
          } else {
            c()
          }},
          error = function(cond) {
            return(c())
          }
        ),
        hierValuesCons = tryCatch(
          {
            if (modelOptions$modelType %in% c("4","5")) {
              getHierPriors(
                hierValuesParameters =
                  hierValuesParametersCons,
                type = "mu_[",
                covariates = covariates,
                names1 = valueNames$targets
              )
            } else {
              c()
            }},
          error = function(cond) {
            return(c())
          }
        ),
        hierValuesWeight = tryCatch(
          {
            if (modelOptions$modelType %in% c("4","5")) {
              getHierPriors(
                hierValuesParameters =
                  hierValuesParametersWeight,
                type = "W_[",
                covariates = covariates,
                names1 = valueNames$fractions,
                names2 = valueNames$targets
              )
            } else {
              c()
            }},
          error = function(cond) {
            return(c())
          }
        ),
        hierValuesValProxies = tryCatch(
          {
            if (modelOptions$modelType %in% c("4","5")) {
              getHierPriors(
                hierValuesParameters =
                  hierValuesParametersValProxies,
                type = "I_[",
                covariates = covariates,
                names1 = valueNames$targets,
                names2 = valueNames$fractions,
                names3 = valueNames$sources
              )
            } else {
              c()
            }},
          error = function(cond) {
            return(c())
          }
        )
        
        
      )
      vMatch <- match(unlist(priorTypes), names(replacements))
      mIndex <- sapply(1:length(vMatch), function(x) sum(vMatch[1:x] == vMatch[x]))
      replacements2 <- try(
        {
          sapply(1:length(vMatch), function(i) {
            replacements[[vMatch[i]]][mIndex[i]]
          })
        },
        silent = TRUE
      )
      if (class(replacements2) == "try-error") {
        stop(paste0("Prior or User Estimate Name not found in:", priors[[x]]))
      }
      replacements <- replacements2
      if (length(na.omit(priorUncertainties)) == 1) {
        Unc <- priorUncertainties
        applyUnc <- TRUE
      } else {
        Unc <- modelOptions$minUnc
        applyUnc <- FALSE
      }

      if (!is.null(replacements) || length(na.omit(priorDistributions)) == 1) {
        if (type == "priors") {
          getNimblePrior(priorParameters, priors[x], replacements,
            n = c(j, x),
            individualNames, Unc = Unc, applyUnc = applyUnc
          )
        } else {
          if (!is.null(replacements)) {
            getUserEstimatesExpressions(priorParameters, priors[x],
              replacements,
              n = c(j, x),
              individualNames
            )
          } else {
            priors[x] <- gsub("=", " <- ", priors[x])
            priors[x]
          }
        }
      } else {
        ""
      }
    })

    if (type == "priors" & length(priorList) > 1) {
      priorList2 <- lapply(1:length(priorList), function(y) strsplit(priorList[[y]], "~")[[1]][2])
      priorListNames <- lapply(1:length(priorList), function(y) strsplit(priorList[[y]], "~")[[1]][1])
      priorListNames <- unique(lapply(1:length(priorListNames), function(y) strsplit(priorListNames[[y]], "_")[[1]][1]))
      
      if (length(unique(priorList2)) < length(unique(priorList))) {
        if(length(unique(priorList2)) == 1){
          priorList <- paste0(priorListNames[[1]], " ~ ",priorList2[[1]])
        } else {
          priorList <- priorList[which(!duplicated(priorList2))]
        }
      }
      
      if (length(na.omit(priorDistributions)) >= 1) {
        priorDist <- priorDist[which(!duplicated(priorList2))]
        priorList <- c(priorList, na.omit(priorDist))
      }
      if (length(na.omit(priorUncertainties)) == 1) {
        priorDistUnc <- priorDistUnc[which(!duplicated(priorList2))]
        priorList <- c(priorList, na.omit(priorDistUnc))
      }
    } else {
      priorList2 <- lapply(1:length(priorList), function(y) strsplit(priorList[[y]], "<-")[[1]][2])
      priorListNames <- lapply(1:length(priorList), function(y) strsplit(priorList[[y]], "<-")[[1]][1])
      priorListNames <- unique(lapply(1:length(priorListNames), function(y) strsplit(priorListNames[[y]], "_")[[1]][1]))
      
      if (length(unique(priorList2)) < length(unique(priorList))) {
        if(length(unique(priorList2)) == 1){
          priorList <- paste0(priorListNames[[1]], " <- ",priorList2[[1]])
        } else {
          priorList <- priorList[which(!duplicated(priorList2))]
        }
      }
      
      if (length(na.omit(priorDistributions)) >= 1) {
        priorList <- c(priorList, priorDist)
      }
      if (length(na.omit(priorUncertainties)) == 1) {
        priorList <- c(priorList, priorDistUnc)
      }
    }

    return(priorList)
  })))
}

getAllMainInteractions <- function(covariates, Names1, Names2 = NULL, Names3 = NULL, vars = NULL) {

  if (is.null(covariates) || is.null(vars) || length(vars) == 0 || all(covariates == "")) {
    return(c())
  }

  covariates <- covariates[, vars, drop = FALSE]
  main <- c()
  interactionsL1 <- c()
  interactionsL2 <- c()

  for (i in 1:ncol(covariates)) {
    main <- c(
      main,
      as.character(interaction(as.data.frame(covariates)[, i]))
    )
  }
  if (ncol(covariates) > 1) {
    selectionsL1 <- combn(1:ncol(covariates), 2)
    for (i in 1:ncol(selectionsL1)) {
      interactionsL1 <- c(
        interactionsL1,
        as.character(interaction(as.data.frame(covariates[, selectionsL1[, i]]),
          sep = "-"
        ))
      )
    }
  }
  if (ncol(covariates) > 2) {
    selectionsL2 <- combn(1:ncol(covariates), 3)
    for (i in 1:ncol(selectionsL2)) {
      interactionsL2 <- c(
        interactionsL2,
        as.character(interaction(as.data.frame(covariates[, selectionsL2[, i]]),
          sep = "-"
        ))
      )
    }
  }
  allInteractions <- c(main, interactionsL1, interactionsL2)
  if (!is.null(Names2) & is.null(Names3)) {
    return(unique(interaction(expand.grid(allInteractions, Names1, Names2), sep = "-")))
  }
  if (!is.null(Names2) & !is.null(Names3)) {
      return(unique(interaction(expand.grid(allInteractions, Names1, Names2, Names3), sep = "-")))
    }
  if (is.null(Names2)) {
    return(unique(interaction(expand.grid(allInteractions, Names1), sep = "-")))
  }
}

nullToEmptyList <- function(val){
  if (is.null(val)) {
    return(list())
  } else {
    return(val)
  }
}

getHierPriors <- function(hierValuesParameters, covariates, names1, type = "alpha_[", names2 = NULL, names3 = NULL) {
  sapply(1:length(hierValuesParameters), function(x) {
    if (is.null(names2)) {
      hiersplits <- strsplit(hierValuesParameters, "-")[[x]][-length(strsplit(hierValuesParameters, "-")[[x]])]
      sourceSplits <- match(
        strsplit(hierValuesParameters, "-")[[x]][length(strsplit(hierValuesParameters, "-")[[x]])],
        names1
      )
      hierarchicalReturn <- paste0(type, which(sapply(
        1:nrow(covariates),
        function(y) all(hiersplits %in% covariates[y, ])
      )), ",", sourceSplits, "]")
      if (length(hierarchicalReturn) > 1) {
        hierarchicalReturn <- paste0("(", paste0(hierarchicalReturn, collapse = "+"), ")/", length(hierarchicalReturn))
      }
    } 
    if (!is.null(names2) & is.null(names3)) {
      hiersplits <- strsplit(hierValuesParameters, "-")[[x]][-c(
        length(strsplit(hierValuesParameters, "-")[[x]]),
        length(strsplit(hierValuesParameters, "-")[[x]]) - 1
      )]
      n2Splits <- match(
        strsplit(hierValuesParameters, "-")[[x]][length(strsplit(hierValuesParameters, "-")[[x]]) - 1],
        names2
      )
      n1Splits <- match(
        strsplit(hierValuesParameters, "-")[[x]][length(strsplit(hierValuesParameters, "-")[[x]])],
        names1
      )

      hierarchicalReturn <- paste0(
        type, which(sapply(
          1:nrow(covariates),
          function(y) all(hiersplits %in% covariates[y, ])
        )),
        ",", n2Splits, ",", n1Splits, "]"
      )
      if (length(hierarchicalReturn) > 1) {
        hierarchicalReturn <- paste0("(", paste0(hierarchicalReturn, collapse = "+"), ")/", length(hierarchicalReturn))
      }
    }
    if (!is.null(names3)) {
      hiersplits <- strsplit(hierValuesParameters, "-")[[x]][-c(
        length(strsplit(hierValuesParameters, "-")[[x]]),
        length(strsplit(hierValuesParameters, "-")[[x]]) - 1,
        length(strsplit(hierValuesParameters, "-")[[x]]) - 2
      )]
      n2Splits <- match(
        strsplit(hierValuesParameters, "-")[[x]][length(strsplit(hierValuesParameters, "-")[[x]]) - 1],
        names2
      )
      n1Splits <- match(
        strsplit(hierValuesParameters, "-")[[x]][length(strsplit(hierValuesParameters, "-")[[x]])],
        names1
      )
      n3Splits <- match(
        strsplit(hierValuesParameters, "-")[[x]][length(strsplit(hierValuesParameters, "-")[[x]]) - 2],
        names2
      )
      
      hierarchicalReturn <- paste0(
        type, which(sapply(
          1:nrow(covariates),
          function(y) all(hiersplits %in% covariates[y, ])
        )),
        ",", n3Splits,",", n2Splits, ",", n1Splits, "]"
      )
      if (length(hierarchicalReturn) > 1) {
        hierarchicalReturn <- paste0("(", paste0(hierarchicalReturn, collapse = "+"), ")/", length(hierarchicalReturn))
      }
    }
    
    return(hierarchicalReturn)
  })
}
