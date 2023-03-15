test_that("Test module importData", {
  testServer(importDataServer,
             {
               # Arrange
               print("test empty data input")
               # Act
               session$setInputs(openPopup = TRUE)
               expect_equal(session$returned(), list())
             })
  
  testServer(importDataServer,
             args = list(outputAsMatrix = TRUE),
             {
               # Arrange
               print("test import of batch covariance")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "file",
                 type = "csv",
                 withRownames = FALSE,
                 colSep = ",",
                 decSep = ".",
                 withColnames = TRUE,
                 includeSd = TRUE,
                 file = structure(
                   list(
                     name = "batch_covariance.csv",
                     size = 199L,
                     type = "text/csv",
                     datapath = testthat::test_path("batch_covariance.csv")
                   ),
                   class = "data.frame",
                   row.names = c(NA,
                                 -1L)
                 ),
                 accept = TRUE
               )
               
               expect_type(session$returned()[["batch_covariance.csv"]], "character")
               expect_equal(class(session$returned()[["batch_covariance.csv"]]),
                            c("matrix", "array"))
               expect_true(attr(session$returned()[["batch_covariance.csv"]],
                                which = "includeSd"))
               expect_false(attr(session$returned()[["batch_covariance.csv"]],
                                 which = "includeRownames"))
               expect_equal(
                 session$returned()[["batch_covariance.csv"]],
                 structure(
                   c(
                     "Individual_1",
                     "Individual_1",
                     "Individual_2",
                     "Individual_2",
                     "Individual_3",
                     "Individual_3",
                     "Individual_4",
                     "Individual_4",
                     "Individual_5",
                     "Individual_5",
                     "1.0",
                     "0.0",
                     "1.0",
                     "0.5",
                     "1.0",
                     "0.0",
                     "1.0",
                     "0.2",
                     "1.0",
                     "0.0",
                     "0.0",
                     "1.0",
                     "0.0",
                     "1.0",
                     "0.3",
                     "1.0",
                     "0.0",
                     "1.0",
                     "0.0",
                     "1.0"
                   ),
                   dim = c(10L, 3L),
                   dimnames = list(NULL, c("target", "Carbon", "Nitrogen")),
                   includeSd = TRUE,
                   includeRownames = FALSE
                 )
               )
             })
  
  testServer(importDataServer,
             args = list(outputAsMatrix = TRUE),
             {
               # Arrange
               print("test import with duplicate rownames")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "file",
                 type = "xlsx",
                 sheet = "1",
                 withRownames = TRUE,
                 colSep = ",",
                 decSep = ".",
                 withColnames = TRUE,
                 includeSd = TRUE,
                 file = structure(
                   list(
                     name = "sources.xlsx",
                     size = 199L,
                     type = "xlsx",
                     datapath = testthat::test_path("sources.xlsx")
                   ),
                   class = "data.frame",
                   row.names = c(NA,
                                 -1L)
                 ),
                 accept = TRUE
               )
               
               expect_type(session$returned()[["sources.xlsx"]], "double")
               expect_equal(class(session$returned()[["sources.xlsx"]]), c("matrix", "array"))
               expect_equal(dim(session$returned()[["sources.xlsx"]]), c(48, 6))
               expect_setequal(
                 rownames(session$returned()[["sources.xlsx"]]),
                 c(
                   "Plants",
                   "TerrestrialAnimals",
                   "MarineFish",
                   "FreshwaterFish"
                 )
               )
               expect_equal(session$returned()[["sources.xlsx"]][1:3, ],
                            structure(
                              c(
                                -25,
                                -24,
                                -25,
                                0.5,
                                0.5,
                                0.5,
                                3,
                                1,
                                2,
                                0.5,
                                0.5,
                                0.5,
                                6,
                                6,
                                6,
                                0.5,
                                0.5,
                                0.5
                              ),
                              dim = c(3L, 6L),
                              dimnames = list(
                                c("Plants", "Plants", "Plants"),
                                c("x13C", "unc", "x15N", "unc.1",
                                  "x34S", "unc.2")
                              )
                            ))
             })
})
