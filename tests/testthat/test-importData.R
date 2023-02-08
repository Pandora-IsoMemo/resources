test_that("Test module importData", {
  testServer(importDataServer,
             {
               # Arrange
               print("test empty data input")
               # Act
               session$setInputs(openPopup = TRUE)
               expect_equal(session$returned(), NULL)
             })
  
  testServer(importDataServer,
             {
               # Arrange
               print("test import of batch covariance")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "file",
                 type = "csv",
                 rownames = FALSE,
                 colSep = ",",
                 decSep = ".",
                 colnames = TRUE,
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
               
               expect_type(session$returned(), "character")
               expect_equal(class(session$returned()),
                            c("matrix", "array"))
               expect_true(attr(session$returned(),
                                which = "includeSd"))
               expect_false(attr(session$returned(),
                                 which = "includeRownames"))
               expect_equal(
                 session$returned(),
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
             {
               # Arrange
               print("test import with rownames")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "file",
                 type = "xlsx",
                 rownames = TRUE,
                 colSep = ",",
                 decSep = ".",
                 colnames = TRUE,
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
               expect_type(session$returned(), "double")
               expect_equal(class(session$returned()), c("matrix", "array"))
               expect_equal(dim(session$returned()), c(48, 6))
               expect_setequal(
                 rownames(session$returned()),
                 c(
                   "Plants",
                   "TerrestrialAnimals",
                   "MarineFish",
                   "FreshwaterFish"
                 )
               )
               expect_equal(session$returned()[1:3, ],
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
                                c("13C", "unc", "15N", "unc.1",
                                  "34S", "unc.2")
                              )
                            ))
             })
})
