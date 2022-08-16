test_that("Test module targetValuesServer", {
  testValues <-
    list(
      modelWeightsContrained = TRUE,
      targetOffset = TRUE,
      obsvnDistribution = list(
        default = "normal",
        term1 = "normal",
        term2 = "normal",
        term3 = "normal"
      ),
      offsetNames = "Offset",
      fractionNames = "proxy_1",
      weightOffset = structure(
        NA_real_,
        dim = c(1L,
                1L),
        dimnames = list("proxy_1", "Offset")
      ),
      obsvn = list(
        default = structure(
          NA_real_,
          dim = c(1L, 1L),
          dimnames = list("Individual_1", "proxy_1")
        ),
        term1 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("Individual_1", "proxy_1")
        ),
        term2 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("Individual_1", "proxy_1")
        ),
        term3 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("Individual_1", "proxy_1")
        )
      ),
      sourceDistribution = list(
        default = "normal",
        term1 = "normal",
        term2 = "normal",
        term3 = "normal"
      ),
      targetValuesShowCoordinates = FALSE,
      obsvnError = list(
        default = structure(
          NA_real_,
          dim = c(1L,
                  1L),
          dimnames = list("Individual_1", "proxy_1")
        ),
        term1 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("Individual_1", "proxy_1")
        ),
        term2 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("Individual_1", "proxy_1")
        ),
        term3 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("Individual_1", "proxy_1")
        )
      ),
      concentrationCovariance = list(structure(
        NA,
        dim = c(1L, 1L),
        dimnames = list("proxy_1",
                        "proxy_1")
      )),
      sourceDistCovRep = list(default = FALSE),
      sourceUncert = list(
        default = list(list(proxy_1 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("source_1", "proxy_1")
        ))),
        term1 = list(list(proxy_1 = structure(
          NA,
          dim = c(1L, 1L),
          dimnames = list("source_1", "proxy_1")
        ))),
        term2 = list(list(proxy_1 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("source_1", "proxy_1")
        ))),
        term3 = list(list(proxy_1 = structure(
          NA,
          dim = c(1L, 1L),
          dimnames = list("source_1", "proxy_1")
        )))
      ),
      sourceNames = "source_1",
      sourceCovariance = list(
        default = list(structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("proxy_1", "proxy_1")
        )),
        term1 = list(structure(
          NA,
          dim = c(1L, 1L),
          dimnames = list("proxy_1",
                          "proxy_1")
        )),
        term2 = list(structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("proxy_1", "proxy_1")
        )),
        term3 = list(structure(
          NA,
          dim = c(1L, 1L),
          dimnames = list("proxy_1",
                          "proxy_1")
        ))
      ),
      targetValuesCovariance = list(
        default = list(Individual_1 = structure(
          1,
          dim = c(1L, 1L),
          dimnames = list("proxy_1", "proxy_1")
        )),
        term1 = list(Individual_1 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("proxy_1", "proxy_1")
        )),
        term2 = list(Individual_1 = structure(
          NA,
          dim = c(1L, 1L),
          dimnames = list("proxy_1", "proxy_1")
        )),
        term3 = list(Individual_1 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("proxy_1", "proxy_1")
        ))
      ),
      concentrationDistribution = "normal",
      weightOffsetUncert = structure(
        NA_real_,
        dim = c(1L, 1L),
        dimnames = list("proxy_1", "Offset")
      ),
      iterations = 10000,
      includeSourceOffset = FALSE,
      modelWeights = FALSE,
      thinning = 10,
      priors = character(0),
      sourceOffsetUncert = list(list(proxy_1 = structure(
        NA,
        dim = c(1L,
                1L),
        dimnames = list("source_1", "proxy_1")
      ))),
      concentrationDistCovRep = FALSE,
      weightDistribution = list(default = "normal"),
      numericVars = NULL,
      weightsUncert = structure(
        NA,
        dim = c(1L, 1L),
        dimnames = list("proxy_1", "proxy_1")
      ),
      concentration = list(structure(
        NA,
        dim = c(1L,
                1L),
        dimnames = list("source_1", "proxy_1")
      )),
      targetValuesCovariatesNames = "Covariate_1",
      targetNames = "proxy_1",
      nchains = 1,
      inflatedBeta = "0",
      userEstimate = character(0),
      modelConcentrations = TRUE,
      fileNotes = NULL,
      burnin = 10000,
      targetValuesCovariates = structure(
        "",
        dim = c(1L,
                1L),
        dimnames = list("Individual_1", "Covariate_1")
      ),
      modelType = "1",
      obsvnNames = "Individual_1",
      alphaHyper = 1,
      covariateType = "0",
      targetValuesShowCovariates = TRUE,
      source = list(
        default = list(list(proxy_1 = structure(
          NA,
          dim = c(1L, 1L),
          dimnames = list("source_1", "proxy_1")
        ))),
        term1 = list(list(proxy_1 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("source_1", "proxy_1")
        ))),
        term2 = list(list(proxy_1 = structure(
          NA,
          dim = c(1L, 1L),
          dimnames = list("source_1", "proxy_1")
        ))),
        term3 = list(list(proxy_1 = structure(
          NA,
          dim = c(1L,
                  1L),
          dimnames = list("source_1", "proxy_1")
        )))
      ),
      concentrationUncert = list(structure(
        NA,
        dim = c(1L, 1L),
        dimnames = list("source_1",
                        "proxy_1")
      )),
      exportCoordinates = structure(
        c(NA,
          NA, NA, NA),
        dim = c(1L, 4L),
        dimnames = list(
          "Individual_1",
          c(
            "Longitude",
            "Latitude",
            "LowerLimit/Mean/Point",
            "UpperLimit/SD"
          )
        )
      ),
      userEstimateGroups = list(),
      statusSim = "INITIALIZE",
      status = "INITIALIZE",
      modelConcentrationsContrained = TRUE,
      sourceOffset = list(list(proxy_1 = structure(
        NA,
        dim = c(1L,
                1L),
        dimnames = list("source_1", "proxy_1")
      ))),
      categoricalVars = NULL,
      optimalPrior = TRUE,
      weights = structure(
        NA,
        dim = c(1L,
                1L),
        dimnames = list("proxy_1", "proxy_1")
      )
    )
  
  testEvents <-
    list(
      processedCache = 8,
      name = list(
        list(
          event = "update",
          variable = "covariateNames",
          old = "covariate",
          new = "Covariate_1"
        )
      ),
      removeName = NULL,
      copyField = "",
      adaptive = FALSE,
      processed = 16
    )
  
  inputNames <- c("targetValues-copyTargetCov", "targetValues-exportCov-export", 
                  "weightOffset-copyTarget", "targetValuesCovariates-import-openPopup", 
                  "targetValues-term", "exportCoordinates-batchImport-openPopup", 
                  "targetValues-import-openPopup", "exportCoordinates-import-openPopup", 
                  "targetValuesShowCovariates", "targetValues-batchImportCov-openPopup", 
                  "targetValuesShowCoordinates", "targetValues-pasteModeCov", "targetOffset", 
                  "targetValues-copyCov", "weightOffset-export-export", "weightOffset-table", 
                  "weightOffset-pasteMode", "weightOffset-import-openPopup", "targetValues-obsvn", 
                  "exportCoordinates-export-export", "exportCoordinates-copy", 
                  "weightOffset-batchImport-openPopup", "targetValues-batchImport-openPopup", 
                  "exportCoordinates-copyTarget", "exportCoordinates-table", "targetValuesCovariates-copyTarget", 
                  "targetValuesCovariates-copy", "targetValues-importCov-openPopup", 
                  "targetValuesCovariates-batchImport-openPopup", "targetValuesCovariates-export-export", 
                  "exportCoordinates-pasteMode", "targetValuesCovariates-pasteMode", 
                  "targetValues-distribution", "targetValues-pasteMode", "targetValues-table", 
                  "targetValues-copy", "targetValues-export-export", "targetValues-covariance", 
                  "weightOffset-copy", "targetValues-copyTarget", "targetValuesCovariates-table"
  )
  
  # test targetValuesServer ----
  testServer(targetValuesServer,
             args = list(
               values = do.call(reactiveValues, testValues),
               events = do.call(reactiveValues, testEvents),
               termChoices = reactive(c(
                 `Default term` = "default",
                 `Add term 1` = "term1",
                 `Add term 2` = "term2",
                 `Add term 3` = "term3"
               )),
               modelType = reactive("1")
             ),
             {
               # Arrange
               print("test targetValuesServer")
               # Act
               session$setInputs(
                 targetOffset = TRUE,
                 targetValuesShowCovariates = FALSE,
                 targetValuesShowCoordinates = TRUE
               )
               
               expect_true(identical(reactiveValuesToList(values), testValues))
             })
  
  
  # test componentsServer ----
  testServer(componentsServer,
             args = list(
               values = do.call(reactiveValues, testValues),
               events = do.call(reactiveValues, testEvents)
             ),
             {
               # Arrange
               print("test componentsServer")
               # Act
               expect_true(identical(reactiveValuesToList(values), testValues))
             })
  
  testServer(componentsServer,
             args = list(
               values = do.call(reactiveValues, testValues),
               events = do.call(reactiveValues, testEvents)
             ),
             {
               # Arrange
               print("test componentsServer")
               # Act
               expect_true(identical(reactiveValuesToList(values), testValues))
             })
  
  
  # test sourcesServer ----
  testServer(sourcesServer,
             args = list(
               values = do.call(reactiveValues, testValues),
               events = do.call(reactiveValues, testEvents),
               hideTargetFilter = reactive(TRUE),
               termChoices = reactive(c(
                 `Default term` = "default",
                 `Add term 1` = "term1",
                 `Add term 2` = "term2",
                 `Add term 3` = "term3"
               )),
               sourceObsvnFilterChoices = reactive(NA),
               sourceObsvnFilterHide = reactive(TRUE)
             ),
             {
               # Arrange
               print("test sourcesServer")
               # Act
               expect_true(identical(reactiveValuesToList(values), testValues))
             })
  
  testServer(sourcesServer,
             args = list(
               values = do.call(reactiveValues, testValues),
               events = do.call(reactiveValues, testEvents),
               hideTargetFilter = reactive(FALSE),
               termChoices = reactive(c(
                 `Default term` = "default",
                 `Add term 1` = "term1",
                 `Add term 2` = "term2",
                 `Add term 3` = "term3"
               )),
               sourceObsvnFilterChoices = reactive(NA),
               sourceObsvnFilterHide = reactive(FALSE)
             ),
             {
               # Arrange
               print("test sourcesServer")
               # Act
               expect_true(identical(reactiveValuesToList(values), testValues))
             })
  
  
  # test concentrationsServer ----
  testServer(concentrationsServer,
             args = list(
               values = do.call(reactiveValues, testValues),
               events = do.call(reactiveValues, testEvents),
               hideTargetFilter = reactive(TRUE),
               sourceObsvnFilterChoices = reactive(NA),
               sourceObsvnFilterHide = reactive(TRUE)
             ),
             {
               # Arrange
               print("test concentrationsServer")
               # Act
               expect_true(identical(reactiveValuesToList(values), testValues))
             })

  testServer(concentrationsServer,
             args = list(
               values = do.call(reactiveValues, testValues),
               events = do.call(reactiveValues, testEvents),
               hideTargetFilter = reactive(FALSE),
               sourceObsvnFilterChoices = reactive(NA),
               sourceObsvnFilterHide = reactive(FALSE)
             ),
             {
               # Arrange
               print("test concentrationsServer")
               # Act
               expect_true(identical(reactiveValuesToList(values), testValues))
               
             })  
})