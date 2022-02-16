library(nimble)
library(dplyr)

valueNames <- list(targets = c("Carbon", "Nitrogen"),
                   fractions = c("Protein", "Energy"),
                   sources = c("Herbivores", "Carnivores", "Plants", "Fish1", "Fish2"))

# values measured in targets/consumers
obsvn <- matrix(c(-19, 12, -17, 11, - 21, 14, -20, 9, -19, 10), ncol = length(valueNames$targets))
# errors associated with values measured in targets/consumers
obsvnError <- matrix(rep(0.5, 10), ncol = length(valueNames$targets))

constants <- list(
  nTargets = nrow(obsvn), # number of targets/consumers
  nSources = length(valueNames$sources),   # number of sources/food groups
  nFractions = length(valueNames$fractions), # number of fractions/food fractions
  nProxies = length(valueNames$targets)    # number of proxies/dietary proxies
)

weights <- matrix(c(74,26,100,0), nrow = constants$nFractions, ncol = constants$nProxies)
weightsUncert <- matrix(c(0.5,0.5,0.5,0.5), nrow = constants$nFractions, ncol = constants$nProxies)

weightOffset <- c(4, 7) # Weight offset 
weightOffsetUncert <- c(.5, 1) # error associated with Weight offset

concentration <- matrix(c(80,52,10,90,92,20,48,90,10,8), nrow = constants$nSources, ncol = constants$nFractions)
concentrationUncert <- matrix(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), nrow = constants$nSources, ncol = constants$nFractions)
source <- array(c(-19,-21,-25,-13,-19,-24,-26,-29,-16,-23,5,7,1,10,11,0,0,0,0,0),
                dim = c(constants$nSources, constants$nFractions, constants$nProxies))
sourceUncert <- array(c(rep(0.5, 15), rep(1E-6, 5)),
                      dim = c(constants$nSources, constants$nFractions, constants$nProxies))

# sourceDirichPrior=c(1, 1, 1, 1, 1)

data <- list(obsvn = obsvn,
             obsvnError = obsvnError,
             weightOffset = weightOffset,
             weightOffsetUncert = weightOffsetUncert,
             weights = weights,
             weightsUncert = weightsUncert,
             concentration = concentration,
             concentrationUncert = concentrationUncert,
             source = source,
             sourceUncert = sourceUncert#,
             # sourceDirichPrior = sourceDirichPrior
)

modelOptions <- list(burnin = 1000, iterations = 1000, nchains = 2, thinning  = 10)

# FRUITS <- list(modelCode = modelCode, constants = constants,
#                data = data, modelOptions = modelOptions,
#                valueNames = valueNames)

FRUITS <- fruits(data, modelOptions, valueNames)

# FRUITS <- nimbleModel(code = modelCode, name = 'FRUITS', constants = constants,
#                       data = data)
FRUITS <- nimbleModel(code = FRUITS$modelCode,
                      name = 'FRUITS',
                      constants = FRUITS$constants,
                      data = FRUITS$data)

FRUITSMCMC <- buildMCMC(FRUITS)
CFRUITS <- compileNimble(FRUITS)
CFRUITSMCMC <- compileNimble(FRUITSMCMC, project = FRUITS)
CFRUITSMCMC$run(modelOptions$burnin + modelOptions$iterations)

MCMCsamples <- as.matrix(CFRUITSMCMC$mvSamples)
MCMCsamples[-c(1:modelOptions$burnin), grep("alpha", colnames(MCMCsamples))] %>% boxplot
MCMCsamples[-c(1:modelOptions$burnin), grep("alpha", colnames(MCMCsamples))] %>% summary
