test_that("Update Array Dimensions", {
  a <- NULL

  expect_equal(updateArray(a, letters[1:2], letters[1:2], letters[1:2]),
               array(NA, dim = c(2, 2, 2),
                     dimnames = list(
                       letters[1:2],
                       letters[1:2],
                       letters[1:2]
                     ))
               )

})
