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
})
