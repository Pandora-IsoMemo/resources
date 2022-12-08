test_that("Test matching of names", {
  testValues <-
    readRDS(testthat::test_path("fiveSourcesData_default.rds"))
  
  targetNames <- colnames(testValues[["obsvn"]][[1]])
  obsvnNames <- rownames(testValues[["obsvn"]][[1]])
  
  expect_equal(targetNames, c("Carbon", "Nitrogen"))
  expect_equal(obsvnNames[1:2], c("Individual_1", "Individual_2"))
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("Carbon", "Nitrogen"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("Carbon", "Nitrogen"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("Carbon", "Nitrogen"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c("Carbon", "Nitrogen"))
  
  expect_null(names(testValues[["sourceCovariance"]][[1]]))
  expect_null(names(testValues[["concentration"]]))
  expect_null(names(testValues[["concentrationUncert"]]))
  expect_null(names(testValues[["concentrationCovariance"]]))
  
  testValues <-
    readRDS(testthat::test_path("blackBearData_baselineModel.rds"))
  
  targetNames <- colnames(testValues[["obsvn"]][[1]])
  obsvnNames <- rownames(testValues[["obsvn"]][[1]])
  
  expect_equal(targetNames, c("d13C", "d15N"))
  expect_equal(obsvnNames[1:2], c("Individual_1", "Individual_2"))
  
  expect_equal(names(testValues[["source"]][[1]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceUncert"]][[1]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffset"]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffsetUncert"]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c("d13C", "d15N"))
  
  expect_equal(names(testValues[["sourceCovariance"]][[1]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["concentration"]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["concentrationUncert"]])[1:2], c("Individual_1", "Individual_2"))
  expect_equal(names(testValues[["concentrationCovariance"]])[1:2], c("Individual_1", "Individual_2"))
})

test_that("Test getDepthAndTable", {
  testFile <-
    c(
      "blackBearData_baselineModel.rds",
      "blackBearData_default.rds",
      "brownBearData_default.rds",
      "dataWithPriors_default.rds",
      "fiveSourcesData_default.rds"
    ) %>%
    sample(size = 1)
  testValues <- readRDS(testthat::test_path(testFile))
  
  # isEntryFun = isDeepestEntry
  expect_equal(getDepthAndTable(testValues[["source"]], isEntryFun = isDeepestEntry)$nFlatten,
               2)
  expect_equal(getDepthAndTable(testValues[["sourceUncert"]], isEntryFun = isDeepestEntry)$nFlatten,
               2)
  expect_equal(getDepthAndTable(testValues[["sourceOffset"]], isEntryFun = isDeepestEntry)$nFlatten,
               1)
  expect_equal(getDepthAndTable(testValues[["sourceOffsetUncert"]], isEntryFun = isDeepestEntry)$nFlatten,
               1)
  
  # isEntryFun = isPreDeepestEntry
  expect_equal(getDepthAndTable(testValues[["source"]], isEntryFun = isPreDeepestEntry)$nFlatten,
               1)
  expect_equal(getDepthAndTable(testValues[["sourceUncert"]], isEntryFun = isPreDeepestEntry)$nFlatten,
               1)
  expect_equal(getDepthAndTable(testValues[["sourceOffset"]], isEntryFun = isPreDeepestEntry)$nFlatten,
               0)
  expect_equal(getDepthAndTable(testValues[["sourceOffsetUncert"]], isEntryFun = isPreDeepestEntry)$nFlatten,
               0)
  
  # isEntryFun = isDeepestEntry
  expect_equal(getDepthAndTable(testValues[["sourceCovariance"]], isEntryFun = isDeepestEntry)$nFlatten,
               1)
  expect_equal(getDepthAndTable(testValues[["concentration"]], isEntryFun = isDeepestEntry)$nFlatten,
               0)
  expect_equal(getDepthAndTable(testValues[["concentrationUncert"]], isEntryFun = isDeepestEntry)$nFlatten,
               0)
  expect_equal(getDepthAndTable(testValues[["concentrationCovariance"]], isEntryFun = isDeepestEntry)$nFlatten,
               0)
})


test_that("Test deleteTableFromList", {
  testValues <-
    readRDS(testthat::test_path("blackBearData_default.rds"))
  
  expect_equal(names(testValues[["source"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceUncert"]][[1]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffset"]][[1]]), c("d13C", "d15N"))
  expect_equal(names(testValues[["sourceOffsetUncert"]][[1]]), c(c("d13C", "d15N")))
  
  testFile <-
    c(
      "blackBearData_default.rds",
      "brownBearData_default.rds",
      "dataWithPriors_default.rds",
      "fiveSourcesData_default.rds"
    ) %>%
    sample(size = 1)
  testValues <- readRDS(testthat::test_path(testFile))
  
  testCols <- names(testValues[["source"]][[1]][[1]])
  colToDelete <- testCols[1]
  leftCols <- testCols[testCols != colToDelete]
  
  expect_equal(names(
    deleteTableFromList(testValues[["source"]],
                        depth = 2,
                        colName = colToDelete)[[1]][[1]]
  ),
  leftCols)
  expect_equal(names(
    deleteTableFromList(testValues[["sourceUncert"]],
                        depth = 2,
                        colName = colToDelete)[[1]][[1]]
  ),
  leftCols)
  expect_equal(names(
    deleteTableFromList(testValues[["sourceOffset"]],
                        depth = 1,
                        colName = colToDelete)[[1]]
  ),
  leftCols)
  expect_equal(names(
    deleteTableFromList(testValues[["sourceOffsetUncert"]],
                        depth = 1,
                        colName = colToDelete)[[1]]
  ),
  leftCols)
})
