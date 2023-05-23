test_that("set default names", {
  expect_equal(defaultNames(character(0)), character(0))
  expect_equal(defaultNames("", prefix = "test"), "test_1")
  expect_equal(defaultNames(c("", "test"), prefix = "test"), c("test_1", "test"))
})

test_that("Repeated Values", {
  expect_equal(defaultNames(c("test", "test", ""), prefix = "test"), c("test", "test", "test_3"))
})

test_that("Values exist", {
  expect_equal(defaultNames(c("", "test_1"), prefix = "test"), c("test_1a", "test_1"))
  expect_equal(
    defaultNames(c("", "test_1", "test_2"), prefix = "test"),
    c("test_1a", "test_1", "test_2")
  )
  expect_equal(
    defaultNames(c("", "test_1", "test_1a"), prefix = "test"),
    c("test_1b", "test_1", "test_1a")
  )
})

test_that("getResetedMatrix()", {
  testMeanData <-
    structure(
      c(-19, -15, -17, -16, -21, 14, 15, 12, 17, 13),
      dim = c(5L,
              2L),
      dimnames = list(
        c(
          "Individual_1",
          "Individual_2",
          "Individual_3",
          "Individual_4",
          "Individual_5"
        ),
        c("Carbon", "Nitrogen")
      )
    )

  testSDData <-
    structure(
      c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
      dim = c(5L,
              2L),
      dimnames = list(
        c(
          "Individual_1",
          "Individual_2",
          "Individual_3",
          "Individual_4",
          "Individual_5"
        ),
        c("Carbon", "Nitrogen")
      )
    )
  
  resetedData <- structure(
    c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    dim = c(5L,
            2L),
    dimnames = list(
      c(
        "Individual_1",
        "Individual_2",
        "Individual_3",
        "Individual_4",
        "Individual_5"
      ),
      c("Carbon", "Nitrogen")
    )
  )
  
  expect_equal(getResetedMatrix(testMeanData), resetedData)
  expect_equal(getResetedMatrix(testSDData), resetedData)
})
