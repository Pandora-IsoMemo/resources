context("Model Summary")


test_that("extractResultMatrixOfChain with Exmpl data", {
  testDataExmpl <- readRDS("test-04-modelSummary_dataExmpl.rds")
  renamedChains <- testDataExmpl$renamedChains
  fruitsObj <- testDataExmpl$fruitsObj
  
  statisticsNames <- c("mean", "sd", "lower 95%", "upper 95%")
  statisticsFunctions <- c("mean", "sd", "function(y) quantile(y, 0.025, na.rm = TRUE)", 
                           "function(y) quantile(y, 0.975, na.rm = TRUE)")
  
  resultMatrixList <- lapply(1:length(renamedChains), function(x) {
    extractResultMatrixOfChain(x, 
                               renamedChains = renamedChains, 
                               statisticsNames = statisticsNames, 
                               statisticsFunctions = statisticsFunctions,
                               fruitsObj = fruitsObj)
  })
  
  resultMatrixList <- resultMatrixList[sapply(resultMatrixList, function(x) length(x) > 0)]
  
  expect_true(all(sapply(resultMatrixList,
                         function(x) all(names(x) %in% names(resultMatrixList[[1]])))))
})


test_that("extractResultMatrixOfChain with User data", {
  testData <- readRDS("test-04-modelSummary_dataUser.rds")
  renamedChains <- testData$renamedChains
  fruitsObj <- testData$fruitsObj
  
  statisticsNames <- c("mean", "sd", "lower 95%", "upper 95%")
  statisticsFunctions <- c("mean", "sd", "function(y) quantile(y, 0.025, na.rm = TRUE)", 
                           "function(y) quantile(y, 0.975, na.rm = TRUE)")
  
  resultMatrixList <- lapply(1:length(renamedChains), function(x) {
    extractResultMatrixOfChain(x, 
                               renamedChains = renamedChains, 
                               statisticsNames = statisticsNames, 
                               statisticsFunctions = statisticsFunctions,
                               fruitsObj = fruitsObj)
  })
  
  resultMatrixList <- resultMatrixList[sapply(resultMatrixList, function(x) length(x) > 0)]
  
  expect_true(all(sapply(resultMatrixList,
                         function(x) all(names(x) %in% names(resultMatrixList[[1]])))))
})
